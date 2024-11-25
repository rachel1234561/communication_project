#Q1 with widgets added to answer first q

library(tidyverse)
library(shiny)
library(shinythemes)
library(ggwordcloud)
library(ggplot2)
library(gapminder)
library(leaflet)

bnb = read.csv("listings.csv")
#show the outliers happening with price to justify filtering
ggplot(bnb, aes(x=price)) + geom_boxplot()
#remove outliers
bnb2 = filter(bnb, price<=1000)
#take out the outliers of really expensive airbnbs
ggplot(bnb2, aes(x=price)) + geom_boxplot()
#more clearly showing distribution

ui <- fluidPage(
  titlePanel("Map Visualization of BnB Listings"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", "Select Price Range:",
                  min = min(bnb2$price, na.rm = TRUE),
                  max = max(bnb2$price, na.rm = TRUE),
                  value = c(min(bnb2$price, na.rm = TRUE), max(bnb2$price, na.rm = TRUE)),
                  step = 10),
      numericInput("size", "Choose a Point Size:", min=1, max=20,
                   value = 2, step=1), #add slider for size of dots
      selectInput("room", "Select Room Type:",
                  unique(bnb2$room_type),
                  "Entire home/apt"),
      selectInput("neighborhood", "Select Neighborhood Group:",
                  unique(bnb2$neighbourhood_group),
                  "Ciutat Vella"),
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

#color range changing at change price
server <- function(input, output) {
  filteredData <- reactive({
    req(input$priceRange)
    bnb2 %>%
      filter(price >= input$priceRange[1] & price <= input$priceRange[2] &
               room_type == input$room & neighbourhood_group == input$neighborhood)
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~colorBin("Spectral", filteredData()$price, bins = 5)(price),
        fillOpacity = 0.8,
        radius = input$size,
        popup = ~paste("Price:", price)
      ) %>%
      addLegend(
        "bottomright",
        pal = colorBin("Spectral", filteredData()$price, bins = 5),
        values = filteredData()$price,
        title = "Price",
        opacity = 1
      )
  })
}

shinyApp(ui,server)


#***** with all bnbs (not just less than 1000 per night) *******
#*
#*

library(shiny)
library(leaflet)
library(dplyr)

# Assuming `bnb` dataset is already loaded into the environment with `latitude`, `longitude`, and `price` columns.
#this is everything - including outliers

ui <- fluidPage(
  titlePanel("Map Visualization of BnB Listings"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", "Select Price Range:",
                  min = min(bnb$price, na.rm = TRUE),
                  max = max(bnb$price, na.rm = TRUE),
                  value = c(min(bnb$price, na.rm = TRUE), max(bnb$price, na.rm = TRUE)),
                  step = 10)
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    req(input$priceRange)
    bnb %>%
      filter(price >= input$priceRange[1] & price <= input$priceRange[2])
  })
  
  colorPalette <- reactive({
    colorBin("YlOrRd", filteredData()$price, bins = 5)
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~colorPalette()(price),
        fillOpacity = 0.8,
        radius = 5,
        popup = ~paste("Price:", price)
      ) %>%
      addLegend(
        "bottomright",
        pal = colorPalette(),
        values = filteredData()$price,
        title = "Price",
        opacity = 1
      )
  })
}

shinyApp(ui, server)
