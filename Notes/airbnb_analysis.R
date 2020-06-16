load("./Raw_Data/AirBnB.Rdata")
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rgdal)
library(leaflet)


### OBJECTIVES
# • Relationship between prices and appartment features,
# • Number of appartments per owner,
# • Renting price per city quarter (“arrondissements”),
# • Visit frequency of the different quarters acoording to time.


# 1. assign each table to named variables, explore data & Check the type.
airbnb_data <- L
supp_data <- R

########################################################
#• Relationship between prices and appartment features:#
########################################################

#### DATA CLEANING

# 1. Select variables related to appartment features
#   - property_type, room_type, accommodates, bathrooms
#     bedrooms, beds, square_feet, price

features_and_price <- airbnb_data %>%
  select(property_type, room_type, bathrooms, bedrooms, square_feet,
    accommodates, neighbourhood_cleansed, id, host_id,
    host_total_listings_count, price)

# 2. Calculate NA values

#   a. Design a function
print_count_missing_values <- function(columns, index, x, y) {
  message <- paste("Missing Values in Column ",
    columns[index], " is: ", x, ".\n",
    "This correspond to: ", y, "% of missing value.\n\n",
    sep = ""
  )
  cat(message)
}

count_missing_values <- function(data, list_column_names) {
  total <- nrow(data)
  len <- length(list_column_names)

  for (index in seq_len(len)) {
    count_na <- sum(is.na(data[, index]))
    freq_na <- round((count_na / total) * 100, 2)
    print_count_missing_values(
      list_column_names,
      index, count_na, freq_na
    )
  }
}

#   b. Perform the calculation
column_names <- colnames(features_and_price)
count_missing_values(features_and_price, column_names)


#   c. Conclusion
# Suppress the variable square_feet which has more than 95% missing values
features_and_price <- features_and_price %>%
  select(-square_feet) %>%
  filter(!is.na(bedrooms) & !is.na(bathrooms))


# 3. Have a look to property types and room type
features_and_price %>% count(property_type)
features_and_price %>% count(room_type)


#   a. Clean property_type suppressing some
#     - Build a list of wanted types
#     - use filter to seect rows only present in the list


list_property_types <- c("Apartment", "Bed & Breakfast", "Boat",
                         "Condominium", "Dorm", "House", "Loft",
                         "Townhouse", "Villa")


features_and_price <- features_and_price %>%
  filter(property_type %in% list_property_types)


# 3. Lets work with the price variable
# We saw previously that the price was of type fct. Read and quote the
# advanced R book here. Explain a bit what is factor type and tell that you
# are going to convert into char and then double. But we have a problem with
# the dollars sign then.

#   a. Design a function here to convert and then replace


clean_price_string <- function(price) {
  price %>%
  gsub("[$]", "", .) %>%
  gsub("[,]", "", .)
}


from_factor_to_decimal <- function(prices) {
  prices %>%
    map(., as.character) %>%
    map(., clean_price_string) %>%
    unlist() %>%
    as.double()
}

prices <- from_factor_to_decimal(features_and_price["price"])
features_and_price["price"] <- prices

# Let's summarize everything we have reagrding the price:

# A quick look on the website for Paris demonstrates that the maximum price
# in AirBnB is 1000$ and the minimum is 9:

glimpse(features_and_price)



#### DATA VIZ

# 1. Not in order:

### Room Type

### Proportion of room type by property type
ggplot(features_and_price, aes(x = "", fill = room_type)) +
  geom_bar(width = 3) +
  facet_wrap(~ property_type, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Room Type Proportion by Property Type\n",
       x = "", y = "# Count", fill = "Room Type")


### Relation Price and Property Type

## prices distribution
price_distr <- ggplot(features_and_price, aes(x = price)) +
    geom_histogram(bins = 80, aes(y = ..density..), fill = "#fb8072") +
    geom_density(alpha = 0.2, lty = 2, fill = "#1f78b4") +
    labs(x = "Price", y = "Density")

summary(features_and_price$price)

  features_and_price <- features_and_price %>%
filter(features_and_price$price >= 9 & features_and_price$price <= 1000)

ggplot(features_and_price, aes(x = price)) +
    geom_histogram(bins = 80, aes(y = ..density..), fill = "#fb8072") +
    geom_density(alpha = 0.2, lty = 2, fill = "#1f78b4") +
    labs(x = "Price", y = "Density")

# Prices distribution by property type
price_by_property <- ggplot(features_and_price, aes(x = price)) +
    geom_histogram(bins = 20, aes(y = ..density..), fill = "#fb8072") +
    geom_density(alpha = 0.2, lty = 2, fill = "#1f78b4") +
    facet_wrap(~ property_type, scales = "free_y") +
    labs(x = "Price", y = "Density")


box_price_by_property <- ggplot(features_and_price) +
    geom_boxplot(aes(x = property_type, y = price, fill = property_type)) +
    labs(x = "Property Type", y = "Price", fill = "Property Type") +
    coord_flip()

fig_property_price <- ggarrange(price_by_property, box_price_by_property,
                                nrow = 2, labels = c("A", "B"))
fig_property_price

summary(features_and_price$price)


#########################################
#Relation Features prices in appartement#
#########################################
apt_features_and_price <- features_and_price %>%
  filter(property_type == "Apartment")



######### BATHROOMS & BEDROOMS

apt_features_and_price["bathrooms"] <- apt_features_and_price["bathrooms"] %>%
  map(., floor)


bath_distr <- ggplot(apt_features_and_price, aes(x = price)) +
  geom_histogram(bins = 15, aes(y = ..density..), fill = "#fb8072") +
  geom_density(lty = 2, color = "#1f78b4") +
  labs(title = "Distribution of prices according\nto Bathroom numbers",
       x = "Price", y = "Density") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 7)) +
  facet_wrap(~ factor(bathrooms), scales = "free_y")


beds_distr <- ggplot(apt_features_and_price, aes(x = price)) +
  geom_histogram(bins = 15, aes(y = ..density..), fill = "#fb8072") +
  geom_density(lty = 2, color = "#1f78b4") +
  labs(title = "Distribution of prices according\nto Bedrooms numbers",
       x = "Price", y = "Density") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 7)) +
  facet_wrap(~ factor(bedrooms), scales = "free_y")


beds_box <- ggplot(apt_features_and_price) +
  geom_boxplot(aes(x = factor(bedrooms), y = price, fill = factor(bedrooms))) +
  labs(y = "Price", fill = "# of Bedrooms") +
  coord_flip()


## Select for bathrooom <= 5
apt_features_and_price_bath <- apt_features_and_price %>%
  filter(bathrooms <= 5)


bath_box <- ggplot(apt_features_and_price_bath) +
  geom_boxplot(aes(x = factor(bathrooms), y = price,
                   fill = factor(bathrooms))) +
  labs(y = "Price", fill = "# of Bathrooms") +
  coord_flip()


test <- ggplot(apt_features_and_price_bath) +
  geom_boxplot(aes(x = "", y = price, fill = factor(bedrooms))) +
  labs(x = "", y = "Price", fill = "# Bedrooms") +
  facet_grid(rows = vars(bathrooms), cols = vars(bedrooms))


ggarrange(bath_distr, beds_distr, bath_box, beds_box, test,
          nrow = 3, ncol = 2, labels = c("A", "B", "C", "D", "E"))



######## LOCATIONS AND ACOMMODATES

acommoda_box <- ggplot(apt_features_and_price) +
  geom_boxplot(aes(x = "", y = price), fill = "#e78c80") +
  labs(title = "Prices according to\n number ofpeople allowed", x = "",y = "Price") +
    facet_wrap(~ factor(accommodates))

location_box <- ggplot(apt_features_and_price) +
  geom_boxplot(aes(x = "", y = price), fill = "#67aad6") +
  labs(y = "Price", fill = "# of Bedrooms") +
  labs(title = "Prices according\n to the Location", x = "",y = "") +
    facet_wrap(~ factor(neighbourhood_cleansed))

ggarrange(acommoda_box, location_box, ncol = 2)




################
# TIME SERIES  #
################

apt_features_and_price <- apt_features_and_price %>%
  rename(listing_id = id)

table <- inner_join(apt_features_and_price, supp_data, by = "listing_id")
table["date"] <- table["date"] %>% map(., as.Date)
longitudinal  <- table %>%
  group_by(date, neighbourhood_cleansed) %>%
  summarise(count_obs = n())

time_location <- ggplot(longitudinal, aes(x = date, y = count_obs, group = 1)) +
  geom_line(size = 0.5, colour = "#67aad6") +
  scale_x_date(date_labels = "%Y") +
  stat_smooth( color = "#df5f4c", method = "loess") +
  labs(x = "Year", y = "# Rented Appartment") +
  facet_wrap(~ neighbourhood_cleansed)


##########
#  HOSTS #
##########

glimpse(apt_features_and_price)

count_by_host_1 <- apt_features_and_price %>%
  group_by(host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  ungroup() %>%
  mutate(groups = case_when(
    number_apt_by_host == 1 ~ "001",
    between(number_apt_by_host, 2, 5) ~ "002-005",
    number_apt_by_host > 5 ~ "006-152"
    )
  )

count_by_host_1 %>%
  group_by(groups) %>%
  summarise(counting = n())

(ggplot(count_by_host_3, aes(x = "", y = counting))
 +  geom_col(aes(fill = factor(groups)), color = "white")
 +  geom_text(aes(y = counting / 1.23, label = counting),
              color = "black",
              size = 5)
 + labs(x = "", y = "", fill = "Number of appartments\nby host")
 +  coord_polar(theta = "y"))


###################
## MAP MAP MAP ####
###################

map   <- readOGR("arrondissements.geojson")
paris <- leaflet(map) %>%
  addProviderTiles(providers$Stamen.TonerLite)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = map$median)

labels <- sprintf(
  "<strong>%s</strong><br/> Median Price: $%g",
  map$l_aroff, map$median
) %>% lapply(htmltools::HTML)

paris %>%
  addPolygons(dashArray = "2",
              color = "white",
              weight = 2,
              smoothFactor = 0.2,
              fillOpacity = 0.5,
              fillColor = ~pal(median),
              highlight = highlightOptions(weight = 5,
                                           color = "black",
                                           dashArray = "",
                                           fillOpacity = 0.6,
                                           bringToFront = TRUE),
              label = labels) %>%
  addLegend("bottomright",
             pal = pal,
             values = ~median,
             title = "Median Prices",
             labFormat = labelFormat(prefix = "$"),
             opacity = 1)






### Save files for Shiny.app
save(apt_features_and_price, file = "apt_data.Rdata")
save(count_by_host_1, file = "num_apt_by_host.Rdata")
save(longitudinal, file = "airbnb_time_serie.Rdata")
