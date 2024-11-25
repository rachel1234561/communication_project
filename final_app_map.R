library(tidyverse)
library(shiny)
library(shinythemes)
library(ggwordcloud)
library(ggplot2)
library(gapminder)
library(leaflet)

bnb = read.csv("C:/Users/Student/Downloads/listings.csv")
bnb2 = filter(bnb, price<=1000)

ui <- fluidPage(
  titlePanel("Map Visualization of BnB Listings"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", "Select Price Range:",
                  min = min(bnb2$price, na.rm = TRUE),
                  max = max(bnb2$price, na.rm = TRUE),
                  value = c(min(bnb2$price, na.rm = TRUE), max(bnb2$price, na.rm = TRUE)),
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
    bnb2 %>%
      filter(price >= input$priceRange[1] & price <= input$priceRange[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~colorBin("YlOrRd", bnb2$price, bins = 5)(price),
        fillOpacity = 0.8,
        radius = 5,
        popup = ~paste("Price:", price)
      ) %>%
      addLegend(
        "bottomright",
        pal = colorBin("YlOrRd", bnb2$price, bins = 5),
        values = bnb2$price,
        title = "Price",
        opacity = 1
      )
  })
}

shinyApp(ui,server)

library(shiny)
library(leaflet)
library(dplyr)

# Assuming `bnb` dataset is already loaded into the environment with `latitude`, `longitude`, and `price` columns.

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
zw
