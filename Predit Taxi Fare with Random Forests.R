# Loading the tidyverse
library(tidyverse)

# Reading in the taxi data
taxi <- read_csv("datasets/taxi.csv")

# Taking a look at the first couple of rows in taxi
head(taxi)

# Renaming the location variables,
# dropping any journeys with zero fares and zero tips,
# and creating the total variable as the log sum of fare and tip
taxi <- taxi %>%
  rename("lat"=pickup_latitude,"long"=pickup_longitude) %>%
  filter(fare_amount > 0 | tip_amount >0) %>%
  mutate(total=log(fare_amount + tip_amount))

# Reducing the data to taxi trips starting in Manhattan
# Manhattan is bounded by the rectangle with 
# latitude from 40.70 to 40.83 and 
# longitude from -74.025 to -73.93
taxi <- taxi  %>% 
  filter(between(lat,40.70,40.83) & between(long,-74.025,-73.93)) 

# Loading in ggmap and viridis for nice colors
library(ggmap)
library(viridis)

# Retrieving a stored map object which originally was created by
# manhattan <- get_map("manhattan", zoom = 12, color = "bw")
manhattan <- readRDS("datasets/manhattan.rds")

# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data=taxi, aes(x=long,y=lat),bins=60,alpha=0.6) +
  labs(x='Longitude',y='Latitude',fill='Journeys')

# Loading in the tree package
library(tree)

# Fitting a tree to lat and long
fitted_tree <- tree(total~lat+long,data=taxi)

# Draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)

