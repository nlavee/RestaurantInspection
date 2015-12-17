# initial plots

## Fix char type to factor type so that R knows it's different factors
## Put in new column that is the same data but factor type
restaurant.clean$grade_factor <- as.factor(restaurant.clean$grade)
restaurant.clean$boro_factor <- as.factor(restaurant.clean$boro)
restaurant.clean$cuisine_description_factor <- as.factor(restaurant.clean$cuisine_description)


## Summary
summary(restaurant.clean$grade_factor)
summary(restaurant.clean$boro_factor)
summary(restaurant.clean$cuisine_description_factor)


## Plots
# 1) historgram of grades
histogram( ~ grade, data = restaurant.clean)

# 2) bar graph of different boroughs distribution in a grade group
bargraph(~ grade, data = restaurant.clean, group = boro, auto.key=TRUE) # cool graph

# 3) show you the distribution of restaurants in different boroughs
bargraph(~ boro, data = restaurant.clean) 

# 4) show you the distribution of restaurants in different boroughs
histogram(~ boro, data = restaurant.clean) 

# 5) this graph looks pretty cool too
bargraph(~cuisine_group_factor, data = restaurant.clean, horizontal = TRUE) 
## ^^^ Might have to group all these cuisine into bigger categories

# 6) Looking at the density plot of the score 
# Seems like a sharp decrease at the point between A and B grades (13 points)
densityplot(~score, data =restaurant.clean, v = 13,
            panel=function(x,...){
              panel.densityplot(x,...)
              median.values <- median(x) 
              panel.abline(v=13,col=4,lty=2)
            }, main = "Density Plot for Score of Restaurant in NYC"
            , xlab = "Score")


## Table
# Table of grade as a factor for different boroughs
tally( restaurant.clean$grade ~ restaurant.clean$boro)

# Table of grade as a cuisin for different boroughs
tally( restaurant.clean$cuisine_group_factor ~ restaurant.clean$boro)

# Get count for cuisine
summary(restaurant.clean$cuisine_group_factor)
