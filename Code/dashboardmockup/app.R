library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinyBS) 

# Define UI for the app
ui <- fluidPage(
  titlePanel("Interactive Map with Crash Probability"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select road type
      selectInput("roadType", "Select Road Type:", 
                  choices = c("All", "Motorway", "Primary", "Secondary", "Tertiary", "Trunk"), 
                  selected = "All"),
      
      # Slider to filter by crash probability
      sliderInput("crashProbRange", "Incident Probability:",
                  min = 0, max = 1, value = c(0, 1)),
      
      # Collapsible panel for Road Breakdown
      bsCollapse(
        bsCollapsePanel("Road Breakdown", plotlyOutput("roadBreakdown"), style = "info", open = FALSE)
      )
    ),
    
    mainPanel(
      # Leaflet map output
      leafletOutput("map"),
      
      # Count of selected road segments
      tags$hr(),  # Horizontal line for separation
      h4("Selected Road Segments Count"),  # Header for road segment count
      textOutput("roadSegmentCount"),  # The actual road segment count output
      
      # Text output for Average Crash Probability under the map
      h4("Average Crash Probability"),  # Header for average probability
      textOutput("avgCrashProb")  # The actual average probability output
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to load and process data
  full_road_data <- reactive({
    
    # Use a relative path for the shapefile
    shapefile_path <- "gis_osm_roads_free_1.shp"
    
    # Check if the shapefile exists
    if (!file.exists(shapefile_path)) {
      stop("The shapefile does not exist in the specified path.")
    }
    
    # Read the shapefile
    roads <- st_read(shapefile_path)
    
    # Add crash probability column with random values
    roads <- roads %>%
      mutate(crashprob = runif(nrow(roads), min = 0, max = 1))
    
    return(roads)
  })
  
  # Reactive function to get filtered data
  filtered_road_data <- reactive({
    req(full_road_data())
    
    # Filter by road type
    if (input$roadType == "All") {
      filtered_roads <- full_road_data() %>%
        filter(fclass %in% c("motorway", "primary", "secondary", "tertiary", "trunk"))
    } else {
      filtered_roads <- full_road_data() %>%
        filter(fclass == tolower(input$roadType))
    }
    
    # Filter by crash probability range
    filtered_roads <- filtered_roads %>%
      filter(crashprob >= input$crashProbRange[1], crashprob <= input$crashProbRange[2])
    
    return(filtered_roads)
  })
  
  # Render the map with filtered data and fixed color coding
  output$map <- renderLeaflet({
    req(filtered_road_data())
    
    # Define color palette based on the full dataset's crash probability range
    full_data <- full_road_data()
    pal <- colorNumeric(palette = "RdYlBu", domain = full_data$crashprob, reverse = TRUE)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Add OpenStreetMap tiles
      addPolylines(data = filtered_road_data(),
                   color = ~pal(crashprob),  # Color by crash probability
                   popup = ~paste("Road Name:", name, "<br>",
                                  "Road Type:", fclass, "<br>",
                                  "Crash Probability:", round(crashprob, 2), "<br>",
                                  "Maximum Speed (KPH):", maxspeed)) %>%  # Add popups with road type and crash probability
      addLegend("bottomright", 
                pal = pal, 
                values = full_data$crashprob,  # Use the full dataset's crash probability for legend
                title = "Crash Probability",
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
  })
  
  # Road Breakdown: Pie chart of percentage of roads in each fclass
  output$roadBreakdown <- renderPlotly({
    req(filtered_road_data())
    
    # Calculate the percentage of roads by fclass
    road_breakdown <- filtered_road_data() %>%
      group_by(fclass) %>%
      summarize(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100)
    
    # Create a pie chart for the road breakdown
    plot_ly(road_breakdown, labels = ~fclass, values = ~percentage, type = 'pie') %>%
      layout(title = 'Road Breakdown (%)', showlegend = TRUE)
  })
  
  # Road Segments Count of selected roads as text output
  output$roadSegmentCount <- renderText({
    req(filtered_road_data())
    
    # Calculate the count of road segments
    segment_count <- nrow(filtered_road_data())
    
    # Display the count of road segments as text
    paste("Total Selected Road Segments:", segment_count)
  })
  
  # Average Crash Probability of selected roads as text output
  output$avgCrashProb <- renderText({
    req(filtered_road_data())
    
    # Calculate the average crash probability
    avg_prob <- filtered_road_data() %>%
      summarize(avg_prob = mean(crashprob, na.rm = TRUE)) %>%
      pull(avg_prob)
    
    # Display the average crash probability as text
    paste("Average Crash Probability:", round(avg_prob, 3))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
