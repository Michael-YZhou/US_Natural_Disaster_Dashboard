# US Natural Disasters Dashboard

## Summary

The **US Natural Disasters Dashboard** is an interactive Shiny application designed to provide users with comprehensive insights into natural disasters across the United States from 2003 to 2023. It aggregates data from the "US Natural Disaster Declarations" dataset and presents it through intuitive visualizations and interactive elements, allowing users to explore disaster trends at both state and national levels.

## Introduction

The dashboard aims to make disaster data accessible and easy to analyze. By utilizing an interactive map, charts, and tables, it provides users with the ability to explore disaster trends by state and year range, giving insights into the types and frequency of disasters. The visualizations are dynamically updated based on user inputs, fostering deeper understanding of disaster patterns in the US.

## Overview of the Dashboard

The dashboard consists of four main tabs, each designed for a specific type of analysis:

- **Map Tab**: Visualizes disaster counts across states using an interactive map and bar chart.
- **Analysis Tab**: Analyzes top disaster types for selected states and years with a pie chart and data table.
- **Trend Tab**: Shows disaster trends over time using a line chart for selected states.
- **About Tab**: Offers background information about the dashboard and data source.

## Detailed Description

### Map Tab

- **Interactive Map**: Displays a choropleth map of the United States, with states colored according to the total number of disasters.
- **User Interaction**: Click on a state to view more detailed disaster data.
- **Bar Chart**: Shows the distribution of disaster types for the selected state or all states if none is selected.
- **Instructional Text**: A prompt “Click the map for more details.” guides users to interact with the map.

### Analysis Tab

- **State Selection**: Choose a specific state or “All States” from a dropdown.
- **Year Range Slider**: Adjust the range of years for analysis.
- **Pie Chart**: Displays the top five disaster types for the selected filters.
- **Data Table**: Lists disaster types, total counts, and percentages as represented in the pie chart.

### Trend Tab

- **State Selection**: Select multiple states for trend comparison.
- **Year Range Slider**: Modify the year range to adjust the trend analysis.
- **Line Chart**: Plots disaster trends over time for selected states or averages for all states.

### About Tab

- **Overview**: Provides an introduction to the dashboard’s purpose.
- **Data Source**: Links to the dataset, allowing users to explore the data in-depth.

## Design Highlights

- **User-Friendly Interface**: The dashboard’s clean, intuitive layout ensures ease of use across all tabs.
- **Interactive Visualizations**: Uses `leaflet`, `ggplot2`, and `plotly` to provide real-time, interactive charts.
- **Responsive Design**: Built with `fluidPage` and `navbarPage`, ensuring accessibility on various devices.
- **Efficient Data Processing**: `dplyr` powers quick filtering and data aggregation, ensuring smooth app performance.

## User Instructions

- **Navigating Tabs**: Use the navigation bar at the top to switch between sections.
- **Interacting with Visualizations**:
  - _Map Tab_: Click on a state to update the bar chart with specific data.
  - _Analysis Tab_: Adjust the state and year range to update the pie chart and data table.
  - _Trend Tab_: Select one or more states to display their disaster trends.
- **Understanding the Data**: Hover over charts to see additional information.

## Conclusion

The **US Natural Disasters Dashboard** is a powerful tool for exploring and understanding the patterns and impacts of natural disasters across the United States. Its interactive and user-friendly design allows for in-depth analysis, making it a valuable resource for researchers, policymakers, and the general public interested in disaster trends.

## Data Source

The US Natural Disaster Declarations dataset is a summary of all federally declared disasters since 1953. The original data comes from the FEMA website. Only data from 2003 to 2023 has been used in the dashboard.  
Data source URL: [https://www.kaggle.com/datasets/headsortails/us-natural-disaster-declarations?select=us_disasters_m5.csv](https://www.kaggle.com/datasets/headsortails/us-natural-disaster-declarations?select=us_disasters_m5.csv)
