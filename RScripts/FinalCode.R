#Clean#
restaurant.clean <- subset(restaurant.clean, score != -1, drop = T)
#restaurant.clean <- subset(restaurant.clean, !is.na(grade) | grade != "NA")
set.seed(2015)

#### Chinese Restaurants (V) ####

# General Statistics
bwplot((restaurant.clean$score) ~ restaurant.clean$cuisine_group_factor, main = "Boxplot for Score and Cuisine Group", xlab = "Cuisine Group", ylab = "Score")

#Permutation Test of Chinese Restaurants vs all of restaurants
N <- 10^4-1
chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
other.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor != "Chinese", drop = T)

histogram(chinese.restaurant, xlab = "Score", v = mean(chinese.restaurant))
histogram(other.restaurant, xlab = "Score", v = mean(other.restaurant))

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.other <- mean(other.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.other
#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.other <- mean(other.restaurant, trim = 0.25)
observed.trmean.diff <- observed.trmean.score.chinese - observed.trmean.score.other

m <- length(chinese.restaurant)
total <- length(chinese.restaurant) + length(other.restaurant)

meanDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((restaurant.clean$score[index])) - mean((restaurant.clean$score[-index]))
  trmeanDiff[i] <- mean((restaurant.clean$score[index]), trim = 0.25) - mean((restaurant.clean$score[-index]), trim = 0.25)
}

histogram(meanDiff, xlab = "Chinese Avg Score - Other Avg Score", main="Perm. Test b/w Chinese & Other cuisine", v = observed.mean.diff)
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val
# Seems like Chinese ave score is truly higher than other cuisine --> not as clean

histogram(trmeanDiff, xlab = "Chinese Trimmed Mean Score - Other Trimmed Mean Score", main="Perm. Test b/w Chinese & Other cuisine", v = observed.trmean.diff)
(sum(trmeanDiff > observed.trmean.diff) +1) / (N+1) # p-val
# Seems like Chinese trimmed mean score is truly higher than other cuisine, but not as extreme as above --> not as clean

#Confidence Interval of Chinese Restaurants Score

chinese.rows <- cbind( score = as.numeric(chinese.restaurant), cat = FALSE)
other.rows <- cbind( score = as.numeric(other.restaurant), cat = TRUE)
chinese.and.other <- as.data.frame(rbind( chinese.rows, other.rows))

favstats(score~cat, data = chinese.and.other)

t.test(score ~ cat, data = chinese.and.other, alt = "greater")
t.test(chinese.restaurant, conf.level = 0.95)$conf

#### Asian Restaurants (V) ####

#Permutation Test of Asian Restaurants vs Chinese restaursnts
#
# Null hyphothesis: Asian and Chinese restaurants have the same score on average
# Alternative hypothesis: Mean score of Chinese restaurants < those of Asian restaurants
#
# Result: p-value = 0.0505 (mean) and = 0.008 (trmean), 
# supporting the previous test between Chinese and others 
# that there are some extremely bad Chinese restaurants

chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
asian.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Asian", drop = T)
combined.chinese.asian <- c(asian.restaurant, chinese.restaurant)

histogram(chinese.restaurant, xlab = "Score", v = mean(chinese.restaurant))
histogram(asian.restaurant, xlab = "Score", v = mean(asian.restaurant))

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.asian <- mean(asian.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.asian # -0.33

#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.asian <- mean(asian.restaurant, trim = 0.25)
observed.trmean.score.diff <- observed.trmean.score.chinese - observed.trmean.score.asian # -0.26

m <- length(chinese.restaurant)
total <- length(chinese.restaurant) + length(asian.restaurant)

meanDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((combined.chinese.asian[index])) - mean((combined.chinese.asian[-index]))
  trmeanDiff[i] <- mean((combined.chinese.asian[index]), trim = 0.25) - mean((combined.chinese.asian[-index]), trim = 0.25)
}

histogram(meanDiff, xlab = "Chinese Avg Score - Asian Avg Score", main = "Diff. in Mean Score b/w Chinese and Asian Restaurants", v = observed.mean.diff)
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val

histogram(trmeanDiff, xlab = "Chinese Trimmed Mean Score - Asian Trimmed Mean Score", main = "Diff. in Trimmed Mean Score b/w Chinese and Asian Restaurants", v = observed.trmean.score.diff)
(sum(trmeanDiff > observed.trmean.score.diff) +1) / (N+1) # p-val

#Confidence Interval of Asian Restaurants
t.test(chinese.restaurant, asian.restaurant)$conf

favstats(~chinese.restaurant)
favstats(~asian.restaurant)

#### Latin Restaurants (A) ####

#Permutation Test of Latin Restaurants vs Chinese restaurants
#
# Null hypothesis: Mean score of Latin restaurants = mean score of Chinese restaurants
# Alternative hypothesis: Latin restaurants have higher mean score
# Result: p-value = 0.03 (mean) & = 0.016 (trmean), moderate evidence to reject null
#

chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
latin.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Latin/Spanish", drop = T)
combined.chinese.latin <- c(chinese.restaurant, latin.restaurant)

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.latin <- mean(latin.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.latin # -0.35

#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.latin <- mean(latin.restaurant, trim = 0.25)
observed.trmean.score.diff <- observed.trmean.score.chinese - observed.trmean.score.latin # -0.23

m <- length(chinese.restaurant)
total <- length(chinese.restaurant) + length(latin.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((combined.chinese.latin[index])) - mean((combined.chinese.latin[-index]))
  medianDiff[i] <- median((combined.chinese.latin[index])) - median((combined.chinese.latin[-index]))
  trmeanDiff[i] <- mean((combined.chinese.latin[index]), trim = 0.25) - mean((combined.chinese.latin[-index]), trim = 0.25)
}

histogram(meanDiff, xlab = "Chinese Avg Score - Latin Avg Score", v = observed.mean.diff)
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val

histogram(trmeanDiff, xlab = "Chinese Trimmed Median Score - Latin Trimmed Median Score", v = observed.trmean.score.diff)
(sum(trmeanDiff > observed.trmean.score.diff) +1) / (N+1) # p-val

# Confidence Interval of Latin Restaurants
t.test(latin.restaurant, chinese.restaurant)$conf

#### Latin vs Asian Restaurants (A) ####
#
# Null hypothesis: Latin and Asian restaurants are equally bad
# Alternative hypothesis: Latin & Asian restaurants are not equally bad
# Result: cannot reject null. 
#

asian.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Asian", drop = T)
latin.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Latin/Spanish", drop = T)
combined.asian.latin <- c(asian.restaurant, latin.restaurant)

#mean
observed.avg.score.asian <- mean(asian.restaurant)
observed.avg.score.latin <- mean(latin.restaurant)
observed.mean.diff <- observed.avg.score.asian - observed.avg.score.latin

#trimmed mean
observed.trmean.score.asian <- mean(asian.restaurant, trim = 0.25)
observed.trmean.score.latin <- mean(latin.restaurant, trim = 0.25)
observed.trmean.score.diff <- observed.trmean.score.asian - observed.trmean.score.latin

m <- length(asian.restaurant)
total <- length(asian.restaurant) + length(latin.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((combined.asian.latin[index])) - mean((combined.asian.latin[-index]))
  medianDiff[i] <- median((combined.asian.latin[index])) - median((combined.asian.latin[-index]))
  trmeanDiff[i] <- mean((combined.asian.latin[index]), trim = 0.25) - mean((combined.asian.latin[-index]), trim = 0.25)
}

histogram(meanDiff, xlab = "Asian Avg Score - Latin Avg Score", v = observed.mean.diff)
((sum(meanDiff < observed.mean.diff) +1) / (N+1)) * 2 # p-val

histogram(trmeanDiff, xlab = "Asian Trimmed Median Score - Latin Trimmed Median Score", v = observed.trmean.score.diff)
((sum(trmeanDiff < observed.trmean.score.diff) +1) / (N+1)) * 2 # p-val

#Confidence Interval of Latin Restaurants
t.test(latin.restaurant, asian.restaurant)$conf

####Regression Model of Cuisine on Score####

xyplot(sqrt(score)~median_income.list | cuisine_group_factor, data = restaurant.clean)
xyplot(sqrt(score)~ log(median_income.list) | cuisine_group_factor, data = restaurant.clean)

fit<- lm(sqrt(score)~cuisine_group_factor + median_income.list, data=restaurant.clean)
summary(fit)
# Seems like the things that matter are being an Asian, Chinese, Italian, Latin/Spanish and Pizza place.
# All these places have an influence to increase the score
# Median income does not matter at all

xyplot(sqrt(score)~log(median_income.list), data = restaurant.clean)
xyplot(sqrt(score)~median_income.list, data = restaurant.clean)
cor(score, median_income.list, data = restaurant.clean, use="complete")
# tally(~restaurant.clean$median_income.list)

qqnorm(residuals(fit))
qqline(residuals(fit))
histogram(residuals(fit))

fit_2<- lm(sqrt(score)~median_income.list, data=restaurant.clean)
summary(fit_2)
# Seems like the things that matter are being an Asian, Chinese, Italian, Latin/Spanish and Pizza place.
# All these places have an influence to increase the score
# Median income does not matter at all

xyplot(residuals(fit_2) ~ fit_2$fitted.values, data = restaurant.clean, type = c("p", "r"), 
       lty = 3, main = "Scatterplot for Residuals and Fitted Value", 
       xlab = "Fitted Values", ylab = "Residual")
histogram(~residuals(fit_2))

qqnorm(residuals(fit_2))
qqline(residuals(fit_2))
histogram(residuals(fit_2))

aic <- c(AIC(fit), AIC(fit_2))
aic

#### Rodents (C) ####

# General statistics for rodents

# Boostrap for rodents
rodent_list <- subset((restaurant.clean), select = rodent_list, drop = T)
boot.rodent <- numeric(N)
for(i in 1 : N)
{
  rodent <- sample(rodent_list, length(rodent_list), replace = T)
  boot.rodent[i] <- mean(rodent) # TRUE / (TRUE + FALSE)
}
histogram(boot.rodent, v = mean(boot.rodent), main = "Bootstrap distribution of rodent", xlab = "Percent TRUE for rodent")
mean(boot.rodent)
sd(boot.rodent)
qqnorm(boot.rodent)
qqline(boot.rodent)

# Income effect on rodents

# Cuisine effect on rodents??????





#### Roaches (C) ####

# General statistics for roaches

# Boostrap for roaches
roach_list <- subset((restaurant.clean), select = roach_list, drop = T)
boot.roach <- numeric(N)
for(i in 1 : N)
{
  roach <- sample(roach_list, length(roach_list), replace = T)
  boot.roach[i] <- mean(roach) # TRUE / (TRUE + FALSE)
}
histogram(boot.roach, v = mean(boot.roach), main = "Bootstrap distribution of roach", xlab = "Percent TRUE for roach")
mean(boot.roach)
sd(boot.roach)
qqnorm(boot.roach)
qqline(boot.roach)

# Income effect on roaches

# Cuisine effect on roaches??????
####Rodents/Roaches by log income####
boxplot(log(restaurant.clean$median_income.list) ~ restaurant.clean$rodent_list, main = "Boxplot of Rodent Presence With Log of Median Income", xlab = "Rodent Presence", ylab = "Log(Median Income)")
boxplot(log(restaurant.clean$median_income.list) ~ restaurant.clean$roach_list, main = "Boxplot of Roach Presence With Log of Median Income", xlab = "Roach Presence", ylab = "Log(Median Income)")
