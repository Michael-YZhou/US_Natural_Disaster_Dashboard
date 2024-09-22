# Import libraries
library(dplyr)
library(shiny)
library(ggplot2)
library(scales)
library(DT)

# Read data
data <- read.csv("us_disaster_declarations.csv") %>%
  filter(state != "AS" & state != "FM" & state != "HI" & state != "MP" & state != "GU")

# Get unique states for the dropdown (convert abbreviations to full state names)
states_abbr <- unique(data$state)
states_full <- state.name[match(states_abbr, state.abb)]
states_full <- states_full[!is.na(states_full)]  # Remove any NAs
states_full <- sort(states_full)

# UI
ui <- fluidPage(
  titlePanel("US Disaster Declarations Pie Chart"),
  
  # Create a fluid row with two columns
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
    
    # Right column: pie chart (made 20% smaller)
    column(
      width = 8,
      plotOutput("pie_chart", height = "480px")  # Reduced height from 600px to 480px
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on the selected state and year range
  filtered_data <- reactive({
    temp_data <- data %>%
      filter(fy_declared >= input$year_range[1],
             fy_declared <= input$year_range[2])
    
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
      # Prepare the data table without the Year Range
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
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
