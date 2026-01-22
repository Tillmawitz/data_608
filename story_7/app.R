# app.R - Main Shiny application file for deployment to shinyapps.io
# Save this file as "app.R" in your app directory (e.g., "story_7" folder)

# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("leaflet")) install.packages("leaflet")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("htmltools")) install.packages("htmltools")
if (!require("sf")) install.packages("sf")
if (!require("shiny")) install.packages("shiny")

# Load libraries
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(sf)
library(shiny)

# Load the processed data (assuming we've already run the data processing script)
# Either load from RDS if available:
if (file.exists("states_energy_data.rds")) {
  states_data <- readRDS("states_energy_data.rds")
} else {
  # Or load from CSV if that's what we have:
  map_data <- read_csv("map_energy_data.csv")
  
  # Load US states map data
  states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  states_sf$ID <- tolower(states_sf$ID)
  
  # Create a mapping between state.name and lowercase state names for joining
  state_mapping <- tibble(
    state = c(state.name, "District of Columbia"),
    ID = tolower(gsub(" ", "", c(state.name, "district of columbia")))
  )
  
  # Transform energy data to match map data format
  map_data <- map_data %>%
    left_join(state_mapping, by = "state")
  
  # Join the energy data with the spatial data
  states_data <- states_sf %>%
    left_join(map_data, by = "ID")
}

# Function to create HTML content for popups
create_popup_content <- function(state_name, total_energy_val, data_row) {
  # Extract all energy source columns
  energy_cols <- names(data_row)[str_detect(names(data_row), "^energy_")]
  
  # Make sure we have a valid state and energy data
  if(is.na(state_name) || is.na(total_energy_val) || length(energy_cols) == 0) {
    return(NULL)
  }
  
  # Create a more robust way to extract values
  sources <- c()
  values <- c()
  
  for(col in energy_cols) {
    # Extract one value at a time to avoid dimension issues
    val <- as.numeric(data_row[[col]])
    # Only include if it's a valid value
    if(length(val) == 1 && !is.na(val) && val > 0) {
      sources <- c(sources, col)
      values <- c(values, val)
    }
  }
  
  # If no valid energy sources found, return NULL
  if(length(sources) == 0) {
    return(NULL)
  }
  
  # Now create a data frame with our carefully extracted data
  sources_df <- tibble(
    source = str_replace(sources, "energy_", ""),
    value = values
  ) %>%
    # Calculate percentage
    mutate(
      percentage = value / total_energy_val * 100,
      # Format source name for display
      source_display = str_replace_all(source, "_", " "),
      source_display = str_to_title(source_display)
    ) %>%
    # Sort by value descending
    arrange(desc(value))
  
  # Combine into HTML content
  content <- paste0(
    "<strong style='font-size: 16px;'>", state_name, "</strong><br>",
    "<b>Total Energy Production:</b> ", format(round(total_energy_val), big.mark=","), " thousand MWh<br>",
    "<hr style='margin: 8px 0;'>",
    "<b>Energy Sources:</b><br>",
    "<div style='display: flex; flex-direction: column; gap: 4px;'>"
  )
  
  # Define source colors
  source_colors <- list(
    coal = "#333333",            # Dark gray/black
    natural_gas = "#4682B4",     # Steel blue
    nuclear = "#FF6B6B",         # Red/salmon
    hydro = "#00BFFF",           # Deep sky blue
    conventional_hydroelectric = "#00BFFF", # Deep sky blue
    wind = "#98FB98",            # Pale green
    solar = "#FFD700",           # Gold
    all_solar = "#FFD700",       # Gold
    biomass = "#8B4513",         # Saddle brown
    geothermal = "#FF7F50"       # Coral
  )
  
  # Add each energy source with color-coded markers
  for(i in 1:nrow(sources_df)) {
    source_name <- sources_df$source[i]
    
    # Determine marker color
    marker_color <- "#808080"  # Default gray
    if (str_detect(source_name, "coal")) marker_color <- source_colors$coal
    else if (str_detect(source_name, "natural_gas")) marker_color <- source_colors$natural_gas
    else if (str_detect(source_name, "nuclear")) marker_color <- source_colors$nuclear
    else if (str_detect(source_name, "hydro|conventional_hydroelectric")) marker_color <- source_colors$hydro
    else if (str_detect(source_name, "wind")) marker_color <- source_colors$wind
    else if (str_detect(source_name, "solar|all_solar")) marker_color <- source_colors$solar
    else if (str_detect(source_name, "biomass|wood")) marker_color <- source_colors$biomass
    else if (str_detect(source_name, "geothermal")) marker_color <- source_colors$geothermal
    
    content <- paste0(
      content,
      "<div style='display: flex; align-items: center;'>",
      "<div style='background-color: ", marker_color, "; width: 12px; height: 12px; margin-right: 6px; border-radius: 2px;'></div>",
      "<div style='display: flex; justify-content: space-between; width: 100%;'>",
      "<div style='margin-right: 10px;'>", sources_df$source_display[i], ":</div>",
      "<div><b>", format(round(sources_df$value[i]), big.mark=","), "</b> MWh (", 
      round(sources_df$percentage[i], 1), "%)</div>",
      "</div></div>"
    )
  }
  
  content <- paste0(content, "</div>")
  
  return(HTML(content))
}

# Create popups for each state
create_all_labels <- function(states_data) {
  state_labels <- rep(NULL, nrow(states_data))
  for(i in 1:nrow(states_data)) {
    if(!is.na(states_data$state[i]) && !is.na(states_data$total_energy[i])) {
      # Generate HTML content for the label
      html_content <- create_popup_content(
        states_data$state[i],
        states_data$total_energy[i],
        states_data[i, ]
      )
      
      # Store the HTML content
      if(!is.null(html_content)) {
        # Save the raw HTML string without wrapping it in htmltools::HTML
        state_labels[i] <- as.character(html_content)
      }
    }
  }
  
  # For states without data, use a simple label
  for(i in 1:nrow(states_data)) {
    if(is.null(state_labels[i])) {
      if(!is.na(states_data$state[i])) {
        state_labels[i] <- states_data$state[i]
      } else if(!is.na(states_data$ID[i])) {
        state_labels[i] <- states_data$ID[i]
      } else {
        state_labels[i] <- "Unknown"
      }
    }
  }
  
  return(state_labels)
}

# Calculate state labels once
state_labels <- create_all_labels(states_data)

# Get list of energy source columns
energy_cols <- names(states_data)[str_detect(names(states_data), "^energy_")]

# Create a simple Shiny UI
ui <- fluidPage(
  titlePanel("US Energy Production by State"),
  
  sidebarLayout(
    sidebarPanel(
      # Add source filter dropdown
      selectInput("source_filter", "Energy Source Filter:",
                  choices = c("All Sources" = "all",
                              "Coal" = "energy_coal",
                              "Natural Gas" = "energy_natural_gas",
                              "Nuclear" = "energy_nuclear",
                              "Hydroelectric" = "energy_conventional_hydroelectric", 
                              "Wind" = "energy_wind",
                              "Solar" = "energy_all_solar",
                              "Other" = "energy_other"),
                  selected = "all"),
      
      # Info box
      HTML("<div style='padding: 10px; background-color: #f8f9fa; border-radius: 5px; margin-top: 20px;'>
            <h4>About this Map</h4>
            <p>This interactive map shows energy production data by state from the U.S. Energy Information Administration (EIA).</p>
            <p>Use the dropdown above to filter by specific energy sources.</p>
            <p>Hover over any state to see detailed energy production information.</p>
           </div>")
    ),
    
    mainPanel(
      # Leaflet map output
      leafletOutput("energy_map", height = "600px")
    )
  )
)

# Create a simple server function
server <- function(input, output, session) {
  
  # Render the map
  output$energy_map <- renderLeaflet({
    # Base map setup
    map <- leaflet(states_data) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setMaxBounds(lng1 = -125, lat1 = 24, lng2 = -66, lat2 = 50)
    
    # Source-specific color and styling
    if (input$source_filter == "all") {
      # Show total energy for all sources
      pal <- colorNumeric("YlOrRd", states_data$total_energy)
      
      map <- map %>%
        addPolygons(
          fillColor = ~ifelse(is.na(total_energy), "#f5f5f5", pal(total_energy)),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = lapply(state_labels, HTML),
          labelOptions = labelOptions(
            style = list(
              "background-color" = "white",
              "border" = "1px solid #ccc",
              "border-radius" = "5px",
              "padding" = "8px",
              "font-family" = "Arial, sans-serif",
              "max-width" = "300px",
              "min-width" = "250px"
            ),
            direction = "auto",
            opacity = 0.95,
            sticky = TRUE,
            offset = c(0, -10),
            textsize = "13px"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = states_data$total_energy,
          title = "Total Energy Production<br>(thousand MWh)",
          opacity = 0.7,
          na.label = "No data"
        )
    } else {
      # Show specific energy source
      source_col <- input$source_filter
      source_name <- str_replace(source_col, "energy_", "")
      source_name <- str_replace_all(source_name, "_", " ")
      source_name <- str_to_title(source_name)
      
      # Define source colors (duplicate from above function for simplicity)
      source_colors <- list(
        coal = "#333333",            
        natural_gas = "#4682B4",     
        nuclear = "#FF6B6B",         
        conventional_hydroelectric = "#00BFFF",
        hydro = "#00BFFF",           
        wind = "#98FB98",            
        all_solar = "#FFD700",       
        solar = "#FFD700",           
        biomass = "#8B4513",         
        geothermal = "#FF7F50",
        other = "#808080"
      )
      
      # Get color for this source
      base_color <- "#808080"  # Default gray
      for (name in names(source_colors)) {
        if (str_detect(source_name, name)) {
          base_color <- source_colors[[name]]
          break
        }
      }
      
      # Create color palette for this source
      pal <- colorNumeric(
        palette = colorRampPalette(c("#f5f5f5", base_color))(10),
        domain = states_data[[source_col]],
        na.color = "#f5f5f5"
      )
      
      map <- map %>%
        addPolygons(
          fillColor = ~pal(get(source_col)),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = lapply(state_labels, HTML),
          labelOptions = labelOptions(
            style = list(
              "background-color" = "white",
              "border" = "1px solid #ccc",
              "border-radius" = "5px",
              "padding" = "8px",
              "font-family" = "Arial, sans-serif",
              "max-width" = "300px",
              "min-width" = "250px"
            ),
            direction = "auto",
            opacity = 0.95,
            sticky = TRUE,
            offset = c(0, -10),
            textsize = "13px"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = states_data[[source_col]],
          title = paste0(source_name, " Production<br>(thousand MWh)"),
          opacity = 0.7,
          na.label = "No data"
        )
    }
    
    return(map)
  })
}

# Run the Shiny app
shinyApp(ui, server)