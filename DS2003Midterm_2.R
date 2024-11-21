library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(gganimate)
library(av)
library(gifski)
library(viridis)
library(ggmap)
library(gapminder)

airbnb<-read.csv("listings.csv")

#Drop NAs
cleaned_airbnb<-airbnb%>%
  drop_na(price, minimum_nights, number_of_reviews,room_type)

#Remove duplicate listings
cleaned_airbnb2 <- cleaned_airbnb %>%
  distinct()


#Visualization 1: Tile Plot
agg_data <- cleaned_airbnb2 %>%
  group_by(neighbourhood_group, room_type) %>%
  summarize(avg_price = mean(price, na.rm = TRUE), .groups='drop')

tile_plot<-ggplot(agg_data, aes(
  x=reorder(neighbourhood_group, avg_price),
  y = reorder(room_type, avg_price),
  fill = avg_price, 
  text=paste("Avg Price: $", round(avg_price, 2))
))+ 
  geom_tile(color = "white") + 
  scale_fill_viridis_c(option="viridis", name = "Avg Price", direction=-1) +
  labs(title = "Heatmap of Average Price by Neighborhood and Room Type",
       x = "Neighborhood",
       y = "Room Type") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tile_plot
interactive_tile_plot<-ggplotly(tile_plot, tooltip = "text")
interactive_tile_plot
htmlwidgets::saveWidget(interactive_tile_plot, "interactive_plot.html", selfcontained=TRUE)


#Visualization 2: Map Price Facet Visualization
Q1 <- quantile(cleaned_airbnb2$price, 0.25)
median_price <- median(cleaned_airbnb2$price)
Q3 <- quantile(cleaned_airbnb2$price, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

cleaned_airbnb_no_outliers <- cleaned_airbnb2 %>%
  filter(price <= upper_bound) %>%  
  mutate(price_category = case_when(
    price < Q1 ~ "Below 1st Quartile Price",
    price >= Q1 & price < median_price ~ "1st Quartile to Median Price",
    price >= median_price & price < Q3 ~ "Median to 3rd Quartile Price",
    price >= Q3 & price <= upper_bound ~ "3rd Quartile up to Outlier Range Price"
  )) %>%
  mutate(price_category = factor(price_category, 
                                 levels = c("Below 1st Quartile Price", 
                                            "1st Quartile to Median Price", 
                                            "Median to 3rd Quartile Price", 
                                            "3rd Quartile up to Outlier Range Price")))
upper_bound
no_outliers_cleaned_airbnb<- cleaned_airbnb2%>%filter(!is.na(last_review) & last_review != "")%>%select(neighbourhood_group, neighbourhood,latitude,longitude,room_type, price)
  
  
  


write.csv(no_outliers_cleaned_airbnb, "cleaned_airbnb_data.csv")



world <- map_data("world")
barcelona_map <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "lightblue") +
  coord_fixed(1.3) +  
  coord_cartesian(xlim = c(2.09, 2.217), ylim = c(41.353, 41.455)) + 
  labs(title = "Price and Popularity Differences of Barcelona Airbnb Locations") +
  theme_classic()+
  theme(panel.background = element_rect(fill = "lightblue", color=NA),
        legend.key = element_rect(fill = "white", color = NA))

barcelona_map + 
  geom_point(data = cleaned_airbnb_no_outliers , aes(x = longitude, y = latitude, color = neighbourhood_group), alpha = 0.8, size=1) +
  labs(size = "Price", color = "Neighborhood") +
  facet_wrap(~price_category)+guides(color = guide_legend(override.aes = list(size = 5)))




#Bar graph
neighborhood_count<-cleaned_airbnb2%>%
  group_by(neighbourhood_group)%>%
  summarize(count=n())
ggplot(neighborhood_count, aes(x = reorder(neighbourhood_group, -count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(
    title = "Number of Airbnb Listings per Neighborhood in Barcelona",
    x = "Neighborhood",
    y = "Number of Listings"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    plot.title = element_text(hjust = 0.5)
  )

neighborhood_average<-cleaned_airbnb2%>%
  group_by(neighbourhood_group)%>%
  summarize(avg_price=mean(price,na.rm=TRUE))
ggplot(neighborhood_average, aes(x = reorder(neighbourhood_group, -avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(
    title = "Average Price of Listings per Neighborhood in Barcelona",
    x = "Neighborhood",
    y = "Average Price"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    plot.title = element_text(hjust = 0.5)
  )
