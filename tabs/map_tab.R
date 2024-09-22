# Load necessary libraries
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(scales)
library(maps)
library(plotly)

# Read and preprocess the data
data <- read.csv("us_disaster_declarations.csv") %>%
  filter(fy_declared >= 2003)

# Function to format large numbers
format_number <- function(count) {
  label_number(accuracy = 0.01, scale_cut = scales::cut_short_scale())(count)
}

# Prepare data for the heatmap
disaster_by_state <- data %>%
  group_by(state) %>%
  summarise(count = n_distinct(disaster_number))

# Get US states map data
map_states <- map("state", fill = TRUE, plot = FALSE)

# Extract state names from the map data
map_names <- lapply(strsplit(map_states$names, ":"), function(x) x[1])

# Convert state abbreviations to full state names in the data
disaster_by_state$state_full <- tolower(state.name[match(disaster_by_state$state, state.abb)])

# Match disaster counts to the map order
total_disaster <- disaster_by_state$count[match(map_names, disaster_by_state$state_full)]

# Create a color palette for the heatmap
cpol <- colorNumeric("Greens", na.color = NA, domain = total_disaster)

# Format the disaster counts for display
formatted_total_disaster <- format_number(total_disaster)

# Define the UI
ui <- fluidPage(
  titlePanel("US Disaster Map with Interactive Bar Chart"),
  fluidRow(
    column(6,
           leafletOutput("map", height = 600)
    ),
    column(6,
           plotlyOutput("bar", height = 600)
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Render the map
  output$map <- renderLeaflet({
    leaflet(map_states) %>%
      addTiles() %>%
      addPolygons(
        stroke = FALSE,
        fillOpacity = 1,
        fillColor = cpol(total_disaster),
        layerId = map_names,
        popup = paste("<b>State:</b>", map_names, "<br>",
                      "<b>Total Disasters:</b>", formatted_total_disaster)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = cpol,
        values = total_disaster,
        title = "Total Disasters"
      )
  })
  
  # Reactive expression to get the selected state data
  selected_state_data <- reactive({
    if (is.null(input$map_shape_click)) {
      # No state selected; return data for all states
      data
    } else {
      # Get the clicked state name in lowercase
      state_clicked <- input$map_shape_click$id
      
      # Filter data for the selected state
      selected_data <- data %>%
        filter(
          tolower(state.name[match(state, state.abb)]) == state_clicked
        )
      
      # If the state name wasn't matched (e.g., territories), return empty data
      if (nrow(selected_data) == 0) {
        data.frame()
      } else {
        selected_data
      }
    }
  })
  
  # Render the bar chart
  output$bar <- renderPlotly({
    # Get data based on the selected state
    plot_data <- selected_state_data() %>%
      group_by(incident_type) %>%
      summarise(count = n_distinct(disaster_number))
    
    # Check if there's data to plot
    if (nrow(plot_data) == 0) {
      plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      # Create the bar chart
      plot <- ggplot(plot_data, aes(x = reorder(incident_type, -count), y = count, fill = incident_type)) +
        geom_bar(stat = "identity") +
        labs(x = "Incident Type", y = "Count", title = if (is.null(input$map_shape_click)) {
          "Total Disasters by Type (All States)"
        } else {
          paste("Total Disasters by Type in", tools::toTitleCase(input$map_shape_click$id))
        }) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(legend.position = "none")
    }
    
    ggplotly(plot)
  })
}

# Run the Shiny app
shinyApp(ui, server)
