library(dplyr)
library(fuzzyjoin)
library(sf)
library(rnaturalearth)
library(leaflet)
library(shiny)
library(plotly)
library(shinydashboard) 
library(DT)
library(shinythemes)

# About page
guide_summary = tabPanel(
  "About",
  h2(HTML("<b>About the Dashboard</b>"), style = "text-align:center"),
  br(),
  br(),
  div(
    h4("This dashboard is a high-level summary of natural disasters in the 
       United States between the years 2003 and 2023. It aims to help users 
       gather useful information about natural disasters at the state and 
       national levels by aggregating data and displaying trends.",
       
       # Change the margin and line height of the text
       style = "font-weight:normal; line-height: 2;" 
    ),
    
    br(),
    a("Data Source: US Natural Disaster Declarations (click to download)", 
      href = "https://www.kaggle.com/datasets/headsortails/us-natural-disaster-declarations?select=us_disasters_m5.csv"),
    
    style = "margin-left:100px; margin-right:100px;"  # Adjust left and right margins
  )
)

ui <- navbarPage(
  theme = shinytheme("united"),
  "Global Meat Production Dashboard",   
  navbarMenu("Guide", 
             guide_summary,
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)