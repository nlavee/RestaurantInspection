res <- restaurant.clean


xyplot(score ~ log(mean_income.list) | cuisine_description, data = res)
densityplot(score ~ log(mean_income.list) | cuisine_group_factor, data = res)


xyplot(res$score ~ log(res$median_income.list))
densityplot(score ~ log(median_income.list) | cuisine_group_factor, data = res)

favstats(res$mean_income.list)

bargraph(~rodent_list|cuisine_group_factor, data = res)
xyplot(log(median_income.list) ~ rodent_list, data = res)

tally(~rodent_list|cuisine_group_factor,format = "proportion", data = res)

#mean of median_income with rodent and w/o rodent
rodent <- subset(res, select = median_income.list, subset = rodent_list == "TRUE", drop = T)
no.rodent <- subset(res, select = median_income.list, subset = rodent_list == "FALSE", drop = T)
rodent <- na.omit(rodent)
summary(no.rodent)
no.rodent <- na.omit(no.rodent)
mean(rodent)
mean(no.rodent)

#mean of median_income with roach and w/o roach
roach <- subset(res, select = median_income.list, subset = roach_list == "TRUE", drop = T)
no.roach <- subset(res, select = median_income.list, subset = roach_list == "FALSE", drop = T)
roach <- na.omit(roach)
summary(no.roach)
no.roach <- na.omit(no.roach)
mean(roach)
mean(no.roach)



plot <- ggplot(data=restaurant.clean.2,aes(x=cuisine_group_factor, fill = cuisine_group_factor)) + 
  geom_bar() + 
  geom_text(stat='bin',aes(label=..count..),vjust=-1) + 
  scale_fill_brewer(palette="Set1")
plot + labs(title = "Count of Different Type of Cuisine in Dataset", x = "Cuisine")


