#### SET SEED ####
set.seed(2015)

####Score by group, zoomed in####
densityplot(~score, group = cuisine_group_factor, data = restaurant.clean, auto.key = TRUE, xlim = c(0,20),
            xlab = "Score", main = "Density Plot for Score Among Different Groups")
summary(restaurant.clean)

####Tally for rodent/roach by cuisine group####
fav_stats(restaurant.clean$score)
summary(restaurant.clean$rodent_list)
table(restaurant.clean$rodent_list, restaurant.clean$cuisine_group_factor)
tally(restaurant.clean$rodent_list~ restaurant.clean$cuisine_group_factor, format = "proportion")
tally(restaurant.clean$roach_list~ restaurant.clean$cuisine_group_factor, format = "proportion")

####Pairs of different variables for fun####
pairs(~score + rodent_list + roach_list + total.list + white.list + log(mean_income.list) + log(median_income.list), data = restaurant.clean)

####Score to income####
xyplot(score~log(median_income.list), data = restaurant.clean, pch = 16)

####Rodents/Roaches by log income####
boxplot(log(restaurant.clean$median_income.list) ~ restaurant.clean$rodent_list, main = "Boxplot of Rodent Presence With Log of Median Income", xlab = "Rodent Presence", ylab = "Log(Median Income)")
boxplot(log(restaurant.clean$median_income.list) ~ restaurant.clean$roach_list, main = "Boxplot of Roach Presence With Log of Median Income", xlab = "Roach Presence", ylab = "Log(Median Income)")

####STUFF THAT LOPEZ DID####
chisq.test(table(restaurant.clean$rodent_list, restaurant.clean$cuisine_group_factor)) # test of independence bw rodent & cuisine
chisq.test(table(restaurant.clean$roach_list, restaurant.clean$cuisine_group_factor)) # test of independence bw roach & cuisine

fit<- lm(sqrt(score)~cuisine_group_factor + median_income.list + white.list, data=restaurant.clean)

#model fitting sqrt of score with cuisine group and median income
fit<- lm(sqrt(score)~cuisine_group_factor + median_income.list, data=restaurant.clean)
summary(fit)
qqnorm(residuals(fit))


#####LOGISTIC REGRESSION ON RODENT AND INCOME & Cuisine Group####
modlog <- glm(rodent_list ~ log(median_income.list) + cuisine_group_factor, data = restaurant.clean, family = binomial)
summary(modlog)
exp(confint(modlog))
modlog2 <- glm(roach_list ~ log(median_income.list) + cuisine_group_factor, data = restaurant.clean, family = binomial)
summary(modlog2)
exp(confint(modlog2))

# NOTE
# Regression is the way to life.
# Have to fix model to make sure that residuals are normally distributed by taking sqrt or log of scores.

# Observations
## Cuisine seems to matter for score and pests
## Income does not seem to matter
## Unsure about demographics (Seems like asian people tend towards higher scores, black people lower scores)

