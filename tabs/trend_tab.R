library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Assuming the data has columns: state, fy_declared, and disaster_number
data <- read.csv("us_disaster_declarations.csv") %>% filter(
  fy_declared >= 2003 & state != "AS" & state != "FM" & state != "HI" 
  & state != "MP" & state != "GU"
)

# UI
ui <- fluidPage(
  titlePanel("Disaster Counts by State and Year"),
  
  # Layout with side panel and main panel
  sidebarLayout(
    # Sidebar for selecting states and year range
    sidebarPanel(
      # Dropdown for selecting states
      selectInput(
        inputId = "selected_states",
        label = "Select States:",
        choices = unique(data$state),
        selected = unique(data$state)[1],  # Default selection
        multiple = TRUE  # Allow multiple selections
      ),
      
      # Slider for selecting year range
      sliderInput(
        inputId = "year_range",
        label = "Select Year Range:",
        min = min(data$fy_declared),
        max = max(data$fy_declared),
        value = c(min(data$fy_declared), max(data$fy_declared)),  # Default range
        step = 1,
        sep = ""
      )
    ),
    
    # Main panel for displaying the line chart
    mainPanel(
      plotlyOutput(outputId = "disaster_line_chart")  # Output for the line chart
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected states and year range
  filtered_data <- reactive({
    data %>%
      filter(state %in% input$selected_states,  # Filter by selected states
             fy_declared >= input$year_range[1] & fy_declared <= input$year_range[2])  # Filter by selected year range
  })
  
  # Render the line chart
  output$disaster_line_chart <- renderPlotly({
    
    # Aggregate disaster counts by year for the selected states
    disaster_summary <- filtered_data() %>%
      group_by(fy_declared) %>%
      summarise(total_disasters = n_distinct(disaster_number))
    
    # Create the line chart
    p <- ggplot(disaster_summary, aes(x = fy_declared, y = total_disasters)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 2, color = "blue") +
      labs(title = "Total Disaster Count by Year",
           x = "Year",
           y = "Total Disasters") +
      theme_minimal()
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)


