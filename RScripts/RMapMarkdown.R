load("/Data/geospatialData.Rda")


```{r setupMap, include= FALSE}
# map
myMap <- get_map(location="Manhattan", source="google", maptype="roadmap", crop=TRUE, zoom = 12, color = "bw")

#### Map with color representing roach ####
roachMap <- ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, color = roach, alpha = factor(roach)),
             data = list_lat_lon_2, size = 3) + 
  labs(title = "Distribution of Roaches on Manhattan Map", x = "Longitude", y = "Latitude")

#### Map with color representing rodent ####
rodentMap <- ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, color = rodent, alpha = factor(rodent)),
             data = list_lat_lon_2, size = 3) +
  labs(title = "Distribution of Rodents on Manhattan Map", x = "Longitude", y = "Latitude")

multiplot(roachMap, rodentMap)
```