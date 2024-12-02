#load libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggwordcloud)
library(ggplot2)
library(gapminder)
library(leaflet)

#load the CSV
bnb = read.csv("listings.csv")

#some initial data exploration
length(bnb)
dim(bnb)
summary(bnb)
class(bnb$id)
class(bnb$name)
class(bnb$host_id)
class(bnb$host_name)
class(bnb$neighbourhood_group)
class(bnb$neighbourhood)
class(bnb$latitude)
class(bnb$longitude)
class(bnb$room_type)
class(bnb$price)
class(bnb$minimum_nights)
class(bnb$number_of_reviews)
class(bnb$last_review)
class(bnb$reviews_per_month)
class(bnb$calculated_host_listings_count)
class(bnb$availability_365)
class(bnb$number_of_reviews_ltm)
class(bnb$license)

#convert the neighborhood, neighborhood group, license, and room_type to factor
bnb <- bnb %>%
  mutate_at(vars(neighbourhood, neighbourhood_group, license, room_type), as.factor)

#convert the last_review to month
bnb$last_review <- as.Date(bnb$last_review, format = "%Y-%m-%d")


#check var types after conversion
class(bnb$id)
class(bnb$name)
class(bnb$host_id)
class(bnb$host_name)
class(bnb$neighbourhood_group)
class(bnb$neighbourhood)
class(bnb$latitude)
class(bnb$longitude)
class(bnb$room_type)
class(bnb$price)
class(bnb$minimum_nights)
class(bnb$number_of_reviews)
class(bnb$last_review)
class(bnb$reviews_per_month)
class(bnb$calculated_host_listings_count)
class(bnb$availability_365)
class(bnb$number_of_reviews_ltm)
class(bnb$license)

#see the numeric vars
numeric_vars <- bnb %>% 
  select_if(is.numeric) %>% 
  names()
numeric_vars

#see the factor vars
factor_vars <- bnb %>% 
  select_if(is.factor) %>% 
  names()
factor_vars

coverted_vars_bnb = bnb
#export the converted variables
write.csv(coverted_vars_bnb, "bnb_output.csv", row.names = FALSE)

#summary of price without filtering out the outliers
summary(bnb$price)
#show the outliers happening with price to justify filtering
ggplot(bnb, aes(x=price)) + geom_boxplot()
#filter out the outliers
bnb2 = filter(bnb, price<=7500)
#summary of price after filtering outliers
summary(bnb2$price)

#length after filtering
dim(bnb2)

#summary of other vars
summary(bnb2$availability_365)
summary(bnb2$minimum_nights)
summary(bnb2$room_type)
summary(bnb2$neighbourhood_group)

# Create the bar chart
ggplot(bnb, aes(x = neighbourhood_group)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Number of Listings by Neighbourhood Group",
    x = "Neighbourhood Group",
    y = "Number of Listings"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################################################################
#SHINY APP
#create the question 1 dashboard: Which neighborhoods have the most affordable listings?
ui <- fluidPage(theme=shinytheme("cyborg"),
  titlePanel("Map Visualization of BnB Listings"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", "Select Price Range:",
                  min = min(bnb2$price, na.rm = TRUE),
                  max = max(bnb2$price, na.rm = TRUE),
                  value = c(min(bnb2$price, na.rm = TRUE), max(bnb2$price, na.rm = TRUE)),
                  step = 10), #user can select what price range they want to see
      numericInput("size", "Choose a Point Size:", min=1, max=20,
                   value = 2, step=1), #add slider for size of dots
      selectInput("room", "Select Room Type:",
                  unique(bnb2$room_type), #add ability to filter map by room type
                  "Entire home/apt"),
      selectInput("neighborhood", "Select Neighborhood Group:",
                  unique(bnb2$neighbourhood_group), #add ability to filter map by neighborhood group
                  "Ciutat Vella"),
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      # Add description below the map
      tags$hr(), # Horizontal line for visual separation
      tags$p("This map visualizes Airbnb listings by price range, room type, and neighborhood group."),
      tags$p("Use the filters in the sidebar to explore different subsets of the data. Adjust the point size and select specific room types or neighborhoods for filtered views."),
    )
  )
)


server <- function(input, output) {
  filteredData <- reactive({
    req(input$priceRange)
    bnb2 %>%
      filter(price >= input$priceRange[1] & price <= input$priceRange[2] &
               room_type == input$room & neighbourhood_group == input$neighborhood)
  }) #color range changing based on price range
  
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


#######################################################################################
#SHINY APP
#create the question 2 dashboard: Which neighborhoods have the most affordable listings?

data = bnb

#I used AI to build a Function to remove outliers based on IQR
remove_outliers <- function(df, var_name) {
  IQR_value <- IQR(df[[var_name]], na.rm = TRUE)
  lower <- quantile(df[[var_name]], 0.25, na.rm = TRUE) - 1.5 * IQR_value
  upper <- quantile(df[[var_name]], 0.75, na.rm = TRUE) + 1.5 * IQR_value
  df %>% filter(df[[var_name]] >= lower & df[[var_name]] <= upper)
}

ui <- fluidPage(theme=shinytheme("cyborg"),
                titlePanel("Visualizing Average Number of Reviews"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("var", "Select a variable:", 
                                c("price", "calculated_host_listings_count"), 
                                "price"),
                    sliderInput("bins", "Number of bins:", 
                                min = 5, max = 50, value = 30),
                    checkboxInput("remove_na", "Remove NA values", value = FALSE),
                    checkboxInput("remove_outliers", "Remove Outliers", value = FALSE)
                  ),
                  mainPanel(
                    plotOutput("hist", width = "500", height = "600"),
                    # Add description below the map
                    tags$hr(), # Horizontal line for visual separation
                    tags$p("This chart visualizes average number of reviews."),
                    tags$p("Use the filters in the sidebar to explore different subsets of the data. Choose the x-axis to be either price or number of listings. Select whether or not you want to see outliers and NA values"),
                  )
                )
)

server <- function(input, output) { 
  
  filtered_data <- reactive({
    req(input$var)  
    
    df <- data %>%
      filter(if (input$remove_na) !is.na(.data[[input$var]]) else TRUE)
    
    if (input$remove_outliers) {
      df <- remove_outliers(df, input$var)  # Remove outliers if the checkbox is checked
    }
    
    df  # Return the filtered dataframe
  })
  
  output$hist <- renderPlot({
    df <- filtered_data()
    
    df %>%
      mutate(bin = cut(.data[[input$var]], breaks = input$bins)) %>%
      group_by(bin, room_type) %>%
      summarise(avg_reviews = mean(number_of_reviews, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = bin, y = avg_reviews, fill = room_type)) +
      geom_bar(stat = "identity", position = "stack") +  # Stacked bars for room_type
      labs(x = input$var, y = "Average Reviews", 
           title = paste("Average Reviews by", input$var)) + #I learned the paste function from google
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) +
      scale_fill_brewer(palette = "Set3") 
  })
}

shinyApp(ui, server)

##########################
