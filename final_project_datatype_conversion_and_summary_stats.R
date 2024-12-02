#load libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggwordcloud)
library(ggplot2)
library(gapminder)
library(leaflet)

#load the CSV
bnb = read.csv("C:/Users/Student/Downloads/listings.csv")

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
#wxport the converted variables
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

