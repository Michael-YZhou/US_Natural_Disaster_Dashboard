# Load necessary libraries
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(scales)
library(maps)
library(plotly)
library(DT)
library(shinythemes)

# Read and preprocess the data
data <- read.csv("us_disaster_declarations.csv") %>%
  filter(
    fy_declared >= 2003 & 
      !state %in% c("AS", "FM", "HI", "MP", "GU")
  )

# Function to format large numbers
format_number <- function(count) {
  label_number(accuracy = 0.01, scale_cut = scales::cut_short_scale())(count)
}

# Prepare data for the map
disaster_by_state <- data %>%
  group_by(state) %>%
  summarise(count = n_distinct(disaster_number))

# Get US states map data
map_states <- map("state", fill = TRUE, plot = FALSE)

# Extract state names from the map data
map_names <- sapply(strsplit(map_states$names, ":"), function(x) x[1])

# Convert state abbreviations to full state names in the data
disaster_by_state$state_full <- tolower(state.name[match(disaster_by_state$state, state.abb)])

# Match disaster counts to the map order
total_disaster <- disaster_by_state$count[match(map_names, disaster_by_state$state_full)]

# Create a color palette for the heatmap
cpol <- colorNumeric("Reds", na.color = NA, domain = total_disaster)

# Format the disaster counts for display
formatted_total_disaster <- format_number(total_disaster)

# Get unique states for the dropdown (convert abbreviations to full state names)
states_abbr <- unique(data$state)
states_full <- state.name[match(states_abbr, state.abb)]
states_full <- states_full[!is.na(states_full)]  # Remove any NAs
states_full <- sort(states_full)

# Define the UI
ui <- navbarPage(
  theme = shinytheme("united"),
  title = "US Natural Disasters Dashboard",
  
  ################### Map Page ###################
  tabPanel(
    "Map",
    fluidRow(
      column(
        width = 6,
        leafletOutput("map", height = 600)
      ),
      column(
        width = 6,
        # Add the text above the bar chart
        h4("Click the map for more details.", style = "text-align:center;"),
        plotlyOutput("bar", height = 550)  # Adjusted height to accommodate the text
      )
    )
  ),
  
  ################### Analysis Page ###################
  tabPanel(
    "Analysis",
    fluidRow(
      # Left column: filters and data table
      column(
        width = 4,
        wellPanel(
          selectInput(
            inputId = "state",
            label = "Select State:",
            choices = c("All States", states_full),
            selected = "All States"
          ),
          sliderInput(
            inputId = "year_range",
            label = "Select Year Range:",
            min = min(data$fy_declared),
            max = max(data$fy_declared),
            value = c(min(data$fy_declared), max(data$fy_declared)),
            sep = "",
            step = 1
          ),
          br(),
          DT::dataTableOutput("data_table")
        )
      ),
      
      # Right column: pie chart
      column(
        width = 8,
        plotOutput("pie_chart", height = "480px")
      )
    )
  ),
  
  ################### Trend Page ###################
  tabPanel(
    "Trend",
    fluidRow(
      # Left column: filters
      column(
        width = 4,
        wellPanel(
          selectInput(
            inputId = "line_states",
            label = "Select States:",
            choices = states_full,
            selected = NULL,
            multiple = TRUE
          ),
          sliderInput(
            inputId = "line_year_range",
            label = "Select Year Range:",
            min = min(data$fy_declared),
            max = max(data$fy_declared),
            value = c(min(data$fy_declared), max(data$fy_declared)),
            sep = "",
            step = 1
          )
        )
      ),
      # Right column: line chart
      column(
        width = 8,
        plotOutput("line_chart")
      )
    )
  ),
  
  ################### About Page ###################
  tabPanel(
    "About",
    h2(HTML("<b>About the Dashboard</b>"), style = "text-align:center"),
    br(),
    div(
      h4("This dashboard is a high-level summary of natural disasters in the 
         United States between the years 2003 and 2023. It aims to help users 
         gather useful information about natural disasters at the state and 
         national levels by aggregating data and displaying trends.",
         style = "font-weight:normal; line-height: 2;"
      ),
      br(),
      a("Data Source: US Natural Disaster Declarations (click to download)", 
        href = "https://www.kaggle.com/datasets/headsortails/us-natural-disaster-declarations?select=us_disasters_m5.csv"),
      style = "margin-left:100px; margin-right:100px;"
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  ################### Map Page Logic ###################
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(map_states) %>%
      addTiles() %>%
      addPolygons(
        stroke = FALSE,
        fillOpacity = 1,
        fillColor = cpol(total_disaster),
        layerId = map_names,
        popup = paste("<b>State:</b>", tools::toTitleCase(map_names), "<br>",
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
        labs(
          x = "Incident Type",
          y = "Count",
          title = if (is.null(input$map_shape_click)) {
            "Total Disasters by Type (All States)"
          } else {
            paste("Total Disasters by Type in", tools::toTitleCase(input$map_shape_click$id))
          }
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    }
    
    ggplotly(plot)
  })
  
  ################### Analysis Page Logic ###################
  
  # Reactive expression to filter data based on the selected state and year range
  filtered_data <- reactive({
    temp_data <- data %>%
      filter(
        fy_declared >= input$year_range[1],
        fy_declared <= input$year_range[2]
      )
    
    if (input$state == "All States") {
      temp_data
    } else {
      # Convert state full name back to abbreviation
      state_abbr <- state.abb[match(input$state, state.name)]
      temp_data %>% filter(state == state_abbr)
    }
  })
  
  # Reactive expression for top incidents
  top_incidents <- reactive({
    # Calculate the counts of each incident type
    incident_counts <- filtered_data() %>%
      group_by(incident_type) %>%
      summarise(count = n_distinct(disaster_number)) %>%
      arrange(desc(count))
    
    # Get the top 5 incident types
    top_incidents <- head(incident_counts, 5)
    
    # Calculate percentages based on the total count of the top incidents
    total_count <- sum(top_incidents$count)
    
    top_incidents %>%
      mutate(percentage = count / total_count * 100)
  })
  
  # Render the data table
  output$data_table <- DT::renderDataTable({
    top_inc <- top_incidents()
    
    if (nrow(top_inc) == 0) {
      # Return an empty data frame or a message
      data.frame(Message = "No data available for the selected filters")
    } else {
      # Prepare the data table
      data_table <- top_inc %>%
        select(
          Disaster_Type = incident_type,
          Total_Count = count,
          Percentage = percentage
        ) %>%
        mutate(
          Percentage = round(Percentage, 1)
        )
      
      # Return the data table
      datatable(
        data_table,
        options = list(pageLength = 5, searching = FALSE, paging = FALSE),
        rownames = FALSE
      )
    }
  })
  
  # Render the pie chart
  output$pie_chart <- renderPlot({
    top_inc <- top_incidents()
    
    # Check if there is data to plot
    if (nrow(top_inc) == 0) {
      # Display message if no data
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      # Create the pie chart
      ggplot(top_inc, aes(x = "", y = percentage, fill = incident_type)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        theme_void() +
        labs(
          title = if (input$state == "All States") {
            "Top 5 Disaster Types Nationwide"
          } else {
            paste("Top 5 Disaster Types in", input$state)
          },
          fill = "Incident Type"
        ) +
        geom_text(
          aes(label = paste0(incident_type, " (", round(percentage, 1), "%)")),
          position = position_stack(vjust = 0.5),
          color = "white",
          size = 4
        )
    }
  })
  
  ################### Trend Page Logic ###################
  
  # Reactive expression to filter data based on the selected states and year range
  line_chart_data <- reactive({
    # Filter data based on year range
    temp_data <- data %>%
      filter(
        fy_declared >= input$line_year_range[1],
        fy_declared <= input$line_year_range[2]
      )
    
    if (is.null(input$line_states) || length(input$line_states) == 0) {
      # No state selected: compute average disaster count for all states
      # First, compute total disasters per state per year
      disasters_per_state_year <- temp_data %>%
        group_by(state, fy_declared) %>%
        summarise(count = n_distinct(disaster_number)) %>%
        ungroup()
      
      # Then, compute average count per year across all states
      average_disasters_per_year <- disasters_per_state_year %>%
        group_by(fy_declared) %>%
        summarise(count = mean(count)) %>%
        mutate(State = "Average")
      
      average_disasters_per_year
    } else {
      # States are selected
      # Convert selected state names to abbreviations
      selected_states_abbr <- state.abb[match(input$line_states, state.name)]
      # Filter data for selected states
      temp_data %>%
        filter(state %in% selected_states_abbr) %>%
        group_by(State = state.name[match(state, state.abb)], fy_declared) %>%
        summarise(count = n_distinct(disaster_number)) %>%
        ungroup()
    }
  })
  
  # Render the line chart
  output$line_chart <- renderPlot({
    chart_data <- line_chart_data()
    
    if (nrow(chart_data) == 0) {
      # No data to plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      # Plot the line chart
      ggplot(chart_data, aes(x = fy_declared, y = count, color = State, group = State)) +
        geom_line(size = 1) +
        geom_point() +
        labs(
          x = "Year",
          y = "Total Disasters",
          title = "Total Disasters Over Time"
        ) +
        theme_minimal()
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
