library(shiny)
library(leaflet)
library(sf)
library(tidyr)
library(tidyverse)
library(dplyr)
library(urbnmapr)


# Get California county boundaries
counties_sf <- get_urbn_map("counties", sf = TRUE)

sf::st_crs(counties_sf) <- 2163
counties_wgs84 <- sf::st_transform(counties_sf, 4326)
ca_counties <- counties_wgs84 %>% filter(state_name == "California")

# Load lookup table from CSV (create this file with county and choices)
lookup_table <- read.csv("https://raw.githubusercontent.com/pointblue/BioStimulantWebTool/main/Data/cacounties.csv")

firetype_table <- read.csv("https://raw.githubusercontent.com/pointblue/BioStimulantWebTool/main/Data/firetype.csv")

# Ensure the column names are consistent
colnames(firetype_table) <- c("Location", "FireType", "RecommendationFireType")
pdf_example<-"https://github.com/pointblue/BioStimulantWebTool/blob/main/www/soil.pdf?raw=true"


#############################Part 1/3 
#UI - contains information about the layout of the app as it appears in your web browser. fluidPage() defines a layout that will resize according to the size of the browser window. 
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #6f94e3; /* Light gray background */
      }
      
      .title-container {
        position: fixed;
        width: 100%;
        z-index: 1000;
        background-color: white;
        border-bottom: 1px solid #ccc;
      }
      
      .title-container .row {
        margin: 0;
      }
      
      .content-container {
        margin-top: 100px; /* Adjust this value to ensure content is below the title */
      }
      
      .map-container {
        border: 2px solid black;
        width: 70%;
        height: 400px;
        margin: 0 auto; /* Center the map */
      }
      
      .info-text {
        text-align: center;
        font-size: 18px;
        margin: 20px 0;
        font-weight: bold; /* Make the text bold */
      }
      
      
    "))
  ),
  div(
    class = "title-container",
    titlePanel(div(
      fluidRow(
        column(width = 6, tags$img(src = "https://github.com/pointblue/BioStimulantWebTool/blob/main/www/CoRenewal-Header-Logo-less-text.jpg?raw=true", height = "50px")),
        column(width = 6, h2("BioStimulant Exploration Tool", style = "margin-top: 10px;"))
      )
    ), windowTitle = "MyPage")
  ),
  div(
    class = "content-container",
    div(class = "info-text", "Choose your county of interest"),
    div(class = "map-container", leafletOutput("map")),
    uiOutput("dropdown"),
    uiOutput("firetype_dropdown"),
    textOutput("recommendation_text"),
    uiOutput("slider_ui"),
    uiOutput("downloadButton")
  )
)


#############################Part 2/3 
# Server - contains information about the computation of the app, creating plots, tables, maps etc. using information provided by the user. 
server <- function(input, output, session) {
  
  # Reactive value to store clicked county
  clicked_county <- reactiveVal(NULL)
  
  # Render map
  output$map <- renderLeaflet({
    leaflet(ca_counties) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(county_name == clicked_county(), "red", "lightblue"),
        weight = 1, 
        color = "black",
        highlightOptions = highlightOptions(weight = 3, color = "red"),
        label = ~county_name,
        layerId = ~county_name, 
        popup = ~county_name
      ) %>%
      setView(lng = -119.5, lat = 37.5, zoom = 5.5)
  })
  
  
  # Observe map clicks
  observeEvent(input$map_shape_click, {
    clicked_county(input$map_shape_click$id)
  })
  
  
  # Render dropdown menu dynamically
  output$dropdown <- renderUI({
    req(clicked_county())
    
    choices <- lookup_table %>%
      filter(County == clicked_county()) %>%
      pull(Choice)  
    
    if (length(choices) > 0) { 
      selectInput("location", "Select region within county:", choices = c("", choices))
    } else {
      NULL 
    }
  })
  
  # Render Fire Type dropdown menu dynamically
  output$firetype_dropdown <- renderUI({
    req(input$location)
    
    choices <- firetype_table %>%
      filter(Location == input$location) %>%
      pull(FireType)
    
    if (length(choices) > 0) { 
      selectInput("firetype", "Select Fire Type:", choices = c("", choices))
    } else {
      NULL 
    }
  })
  
  
  # Render the recommendation text dynamically
  output$recommendation_text_ui <- renderUI({
    req(input$firetype)
    
    recommendation <- firetype_table %>%
      filter(Location == input$location, FireType == input$firetype) %>%
      pull(RecommendationFireType)
    
    if (length(recommendation) > 0) {
      recommendation
    } else {
      NULL 
    }
  })
  
  output$recommendation_text <- renderText({
    req(input$firetype)
    
    recommendation <- firetype_table %>%
      filter(Location == input$location, FireType == input$firetype) %>%
      pull(RecommendationFireType)
    
    if (length(recommendation) > 0) {
      recommendation
    } else {
      ""
    }
  })
  
  
  # Render the slider dynamically
  output$slider_ui <- renderUI({
    req(input$firetype)  # Ensure that a fire type has been selected
    
    sliderInput(inputId = "slider", label = "Years since fire", value = 5, min = 1, max = 100)
  })
    
    

  
  
  
  # Render the download button dynamically
  output$downloadButton <- renderUI({
    req(clicked_county()) # Make sure a county is selected
    downloadButton("downloadPdf", "Download Recommendation")
  })
  
  
  # Download handler for the PDF
  output$downloadPdf <- downloadHandler(
    filename = function() {
      paste0(clicked_county(), ".pdf")
    },
    content = function(file) {
      pdf_url <-pdf_example
      download.file(pdf_url, file, mode = "wb") 
    }
  )
  
  
}#close server


#############################Part 3/3 
# ShinyApp - runs the app
shinyApp(ui, server)

