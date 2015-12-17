#### Requirements loading ####
#install.packages("ggmap")
library(ggmap)
library(stringr)

#### Concatenate to get full address ####
df.full.address <- restaurant.clean.2
colnames(df.full.address)

summary(df.full.address$building)
summary((df.full.address$street))
summary((df.full.address$boro))
summary(df.full.address$zipcode)

#concatenate everything to get the full address at a camis
full.address <- paste(str_trim(df.full.address$building), str_trim(df.full.address$street), str_trim(df.full.address$boro), str_trim(df.full.address$zipcode))

#### Geocoding: Define Locations ####
myLocation <- cbind(full.address, as.character(df.full.address$camis), as.character(df.full.address$score), as.character(df.full.address$grade), df.full.address$roach_list, df.full.address$rodent_list)
list_lat_lon <- c()
limit <- 1
for( time in 1 : 5 )
{
  for( i in limit : (limit+2000))
  {
    latlonLocation <- cbind(myLocation[i,1], myLocation[i,2], myLocation[i,3], (geocode(myLocation[i], output = "latlon", source = "google")), myLocation[i,5], myLocation[i,6]) 
    # currently having problem due to Google throttle, would take 4 days 
    # FIXED by having a for loop running
    
    if( as.numeric(myLocation[i,3]) < 10 )
    {
      latlonLocation <- cbind(grade = "Below 10", latlonLocation)
    }
    else if( as.numeric(myLocation[i,3]) < 20 )
    {
      latlonLocation <- cbind(grade = "Below 20", latlonLocation)
    }
    else if( as.numeric(myLocation[i,3]) < 30 )
    {
      latlonLocation <- cbind(grade = "Below 30", latlonLocation)
    }
    else
    {
      latlonLocation <- cbind(grade = "Else", latlonLocation)
    }
    
    list_lat_lon <- rbind(list_lat_lon, latlonLocation)
  }
  limit <- limit + 2001
}

#distQueryCheck() # check limit so far
list_lat_lon_2 <- cbind(list_lat_lon, df.full.address$roach_list[1:10005], df.full.address$rodent_list[1:10005])
head(list_lat_lon_2)
#colnames(list_lat_lon) <- c("grade", "zipcode", "camis", "score", "lon", "lat", "roach", "rodent")
colnames(list_lat_lon_2) <- c("grade", "address", "camis", "score", "lon", "lat", "roach", "rodent")

#Save the dataframe
list_lat_lon_2 <- as.data.frame(list_lat_lon_2)
save(list_lat_lon_2,file="Data/geospatialData.Rda")

#### Geocoding: Define Map & Attributes ####
myMap <- get_map(location="Manhattan", source="google", maptype="roadmap", crop=TRUE, zoom = 12, color = "bw")
# zoom = integer from 3-21
# 3 = continent, 10=city, 21=building
# , color = "bw"

#### Map with size representing score ####
ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, size=score),
             data = list_lat_lon)

#### Map with colour representing score ####
ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, colour = as.numeric(score)),
             data = list_lat_lon, size = 2) + 
  scale_colour_gradient2(limit = c(0, 30), low="white", mid="cyan", high="black", midpoint=median(as.numeric(list_lat_lon$score))) +
  labs(title = "Distribution of Score on Manhattan Map", x = "Longitude", y = "Latitude")

#### Map with color representing grades that are factor from score ####
ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, color = factor(grade)),
             data = list_lat_lon, size = 3)

#### Map with color representing roach ####
ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, color = roach, alpha = factor(roach)),
             data = list_lat_lon_2, size = 3) + 
  labs(title = "Distribution of Roaches on Manhattan Map", x = "Longitude", y = "Latitude")

#### Map with color representing rodent ####
ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, color = rodent, alpha = factor(rodent)),
             data = list_lat_lon_2, size = 3) +
  labs(title = "Distribution of Rodents on Manhattan Map", x = "Longitude", y = "Latitude")