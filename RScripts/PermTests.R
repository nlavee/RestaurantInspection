#### SET SEED ####
set.seed(2015)
restaurant.clean <- subset(restaurant.clean, !is.na(grade) | grade = 'NA')
tally(~restaurant.clean$score)

histogram(restaurant.clean$score)

####Permutation test for grades of b/w Chinese & Other cuisine####
# Assumption is that Chinese restaurant is more likely to be dirty, we can test this with permutation test,
# see whether the grades for Chinese restaurant is higher than that of other cuisine
# Null hypothesis: avg score are the same
# Alt hypothesis: avg score for chinese restaurant is higher
favstats(score~cuisine_group_factor, data = restaurant.clean)

N <- 10^4-1
chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
other.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor != "Chinese", drop = T)

histogram(chinese.restaurant, xlab = "Score", v = mean(chinese.restaurant))
histogram(other.restaurant, xlab = "Score", v = mean(other.restaurant))

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.other <- mean(other.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.other
#median
observed.median.score.chinese <- median(chinese.restaurant)
observed.median.score.other <- median(other.restaurant)
observed.median.diff <- observed.median.score.chinese - observed.median.score.other
#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.other <- mean(other.restaurant, trim = 0.25)
observed.trmean.diff <- observed.trmean.score.chinese - observed.trmean.score.other

m <- length(chinese.restaurant)
total <- length(chinese.restaurant) + length(other.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((restaurant.clean$score[index])) - mean((restaurant.clean$score[-index]))
  medianDiff[i] <- median((restaurant.clean$score[index])) - median((restaurant.clean$score[-index]))
  trmeanDiff[i] <- mean((restaurant.clean$score[index]), trim = 0.25) - mean((restaurant.clean$score[-index]), trim = 0.25)

  }

histogram(meanDiff, xlab = "Chinese Avg Score - Other Avg Score", main="Perm. Test b/w Chinese & Other cuisine", v = observed.mean.diff)
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val
# Seems like Chinese ave score is truly higher than other cuisine --> not as clean

histogram(medianDiff, xlab = "Chinese Median Score - Other Median Score", main="Perm. Test b/w Chinese & Other cuisine", v = observed.median.diff)
# Median score is the same

histogram(trmeanDiff, xlab = "Chinese Trimmed Median Score - Other Trimmed Median Score", main="Perm. Test b/w Chinese & Other cuisine", v = observed.trmean.diff)
(sum(trmeanDiff > observed.trmean.diff) +1) / (N+1) # p-val
# Seems like Chinese trimmed mean score is truly higher than other cuisine, but not as extreme as above --> not as clean

####Permutation test for grades of b/w Asian & Other cuisine####
# Assumption is that Chinese restaurant is more likely to be dirty, we can test this with permutation test,
# see whether the grades for Chinese restaurant is higher than that of other cuisine
# Null hypothesis: avg score are the same
# Alt hypothesis: avg score for chinese restaurant is higher
favstats(score~cuisine_group_factor, data = restaurant.clean)

N <- 10^4-1
asian.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Asian", drop = T)
other.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor != "Asian", drop = T)

histogram(asian.restaurant, xlab = "Score", v = mean(asian.restaurant))
histogram(other.restaurant, xlab = "Score", v = mean(other.restaurant))

#mean
observed.avg.score.asian <- mean(asian.restaurant)
observed.avg.score.other <- mean(other.restaurant)
observed.mean.diff <- observed.avg.score.asian - observed.avg.score.other
#median
observed.median.score.asian <- median(asian.restaurant)
observed.median.score.american <- median(other.restaurant)
observed.median.diff <- observed.median.score.asian - observed.median.score.american
#trimmed mean
observed.trmean.score.asian <- mean(asian.restaurant, trim = 0.25)
observed.trmean.score.american <- mean(other.restaurant, trim = 0.25)
observed.trmean.diff <- observed.median.score.asian - observed.trmean.score.american

m <- length(asian.restaurant)
total <- length(asian.restaurant) + length(other.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((restaurant.clean$score[index])) - mean((restaurant.clean$score[-index]))
  medianDiff[i] <- median((restaurant.clean$score[index])) - median((restaurant.clean$score[-index]))
  trmeanDiff[i] <- mean((restaurant.clean$score[index]), trim = 0.25) - mean((restaurant.clean$score[-index]), trim = 0.25)
  
}

histogram(meanDiff, xlab = "Asian Avg Score - Other Avg Score", main="Perm. Test b/w Asian & Other cuisine", v = observed.mean.diff, xlim = c(-0.4, 2))
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val
# Seems like Chinese ave score is truly higher than other cuisine --> not as clean

histogram(medianDiff, xlab = "Asian Median Score - Other Median Score", main="Perm. Test b/w Asian & Other cuisine", v = observed.median.diff)
# Median score is the same

histogram(trmeanDiff, xlab = "Asian Trimmed Median Score - Other Trimmed Median Score", main="Perm. Test b/w Asian & Other cuisine", v = observed.trmean.diff, xlim = c(-0.4, 2))
(sum(trmeanDiff > observed.trmean.diff) +1) / (N+1) # p-val
# Seems like Chinese trimmed mean score is truly higher than other cuisine, but not as extreme as above --> not as clean

####Permutation test for grades of b/w Asian + Chinese & Other cuisine####
# Assumption is that Chinese restaurant is more likely to be dirty, we can test this with permutation test,
# see whether the grades for Chinese restaurant is higher than that of other cuisine
# Null hypothesis: avg score are the same
# Alt hypothesis: avg score for chinese restaurant is higher
favstats(score~cuisine_group_factor, data = restaurant.clean)
summary(restaurant.clean$cuisine_group_factor)

N <- 10^4-1
asian.chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Asian" | cuisine_group_factor == "Chinese", drop = T)
other.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor != "Asian" && cuisine_group_factor != "Chinese", drop = T)

histogram(asian.restaurant, xlab = "Score", v = mean(asian.chinese.restaurant))
histogram(other.restaurant, xlab = "Score", v = mean(other.restaurant))

#mean
observed.avg.score.asian <- mean(asian.chinese.restaurant)
observed.avg.score.other <- mean(other.restaurant)
observed.mean.diff <- observed.avg.score.asian - observed.avg.score.other
#median
observed.median.score.asian <- median(asian.chinese.restaurant)
observed.median.score.american <- median(other.restaurant)
observed.median.diff <- observed.median.score.asian - observed.median.score.american
#trimmed mean
observed.trmean.score.asian <- mean(asian.chinese.restaurant, trim = 0.25)
observed.trmean.score.american <- mean(other.restaurant, trim = 0.25)
observed.trmean.diff <- observed.trmean.score.asian - observed.trmean.score.american

m <- length(asian.chinese.restaurant)
total <- length(asian.chinese.restaurant) + length(other.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((restaurant.clean$score[index])) - mean((restaurant.clean$score[-index]))
  medianDiff[i] <- median((restaurant.clean$score[index])) - median((restaurant.clean$score[-index]))
  trmeanDiff[i] <- mean((restaurant.clean$score[index]), trim = 0.25) - mean((restaurant.clean$score[-index]), trim = 0.25)
  
}

histogram(meanDiff, xlab = "Asian Avg Score - Other Avg Score", main="Perm. Test b/w Asian & Other cuisine", v = observed.mean.diff)
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val
# Seems like Chinese ave score is truly higher than other cuisine --> not as clean

histogram(medianDiff, xlab = "Asian Median Score - Other Median Score", main="Perm. Test b/w Asian & Other cuisine", v = observed.median.diff)
# Median score is the same

histogram(trmeanDiff, xlab = "Asian Trimmed Median Score - Other Trimmed Median Score", main="Perm. Test b/w Asian & Other cuisine", v = observed.trmean.diff)
(sum(trmeanDiff > observed.trmean.diff) +1) / (N+1) # p-val
# Seems like Chinese trimmed mean score is truly higher than other cuisine, but not as extreme as above --> not as clean


#### Calculate the confident intervals ####
chinese.rows <- cbind( score = as.numeric(chinese.restaurant), cat = FALSE)
other.rows <- cbind( score = as.numeric(other.restaurant), cat = TRUE)
chinese.and.other <- as.data.frame(rbind( chinese.rows, other.rows))

favstats(score~cat, data = chinese.and.other)

t.test(score ~ cat, data = chinese.and.other, alt = "greater", var.equal = T)
t.test(chinese.restaurant, conf.level = 0.99)$conf

#### 
asian.rows <- cbind(score = as.numeric(asian.restaurant), cat = FALSE)
other.rows <- cbind( score = as.numeric(other.restaurant), cat = TRUE)
asian.and.other <- as.data.frame(rbind(asian.rows, other.rows))

favstats(score~cat, data = asian.and.other)

t.test(score ~ cat, data = asian.and.other, alt = "greater", var.equal = T)

####Permutation test for grades of b/w Chinese & American####
# Assumption is that Chinese restaurant is more likely to be dirty, we can test this with permutation test,
# see whether the grades for Chinese restaurant is smaller than that of American cuisine
# Null hypothesis: avg score are the same
# Alt hypothesis: avg score for chinese restaurant is higher

chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
american.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "American", drop = T)
combined.chinese.american <- c(chinese.restaurant, american.restaurant)

histogram(chinese.restaurant, xlab = "Score", v = mean(chinese.restaurant))
histogram(american.restaurant, xlab = "Score", v = mean(american.restaurant))

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.american <- mean(american.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.american
#median
observed.median.score.chinese <- median(chinese.restaurant)
observed.median.score.american <- median(american.restaurant)
observed.median.score.diff <- observed.median.score.chinese - observed.median.score.american
#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.american <- mean(american.restaurant, trim = 0.25)
observed.trmean.score.diff <- observed.trmean.score.chinese - observed.trmean.score.american

m <- length(chinese.restaurant)
total <- length(chinese.restaurant) + length(american.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((combined.chinese.american[index])) - mean((combined.chinese.american[-index]))
  medianDiff[i] <- median((combined.chinese.american[index])) - median((combined.chinese.american[-index]))
  trmeanDiff[i] <- mean((combined.chinese.american[index]), trim = 0.25) - mean((combined.chinese.american[-index]), trim = 0.25)
}

histogram(meanDiff, xlab = "Chinese Avg Score - America Avg Score", v = observed.mean.diff, xlim = c(-2,2))
(sum(meanDiff > observed.mean.diff) +1) / (N+1) # p-val
# Seems like Chinese ave score is truly higher than american cuisine in a very extreme way--> not as clean

histogram(medianDiff, xlab = "Chinese Median Score - American Median Score", v = observed.median.diff)
# Median score is the same, cannot even show up as a graph since everything is zero

histogram(trmeanDiff, xlab = "Chinese Trimmed Median Score - American Trimmed Median Score", v = observed.trmean.diff)
(sum(meanDiff > observed.trmean.score.diff) +1) / (N+1) # p-val
# Seems like Chinese ave score is truly higher than american cuisine, not as extreme --> not as clean & there are outliers that skew the results heavily at first

####Permutation test for grades of b/w Chinese & Latin####
#
#
#
chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
latin.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Latin/Spanish", drop = T)
combined.chinese.latin <- c(chinese.restaurant, latin.restaurant)

histogram(chinese.restaurant, xlab = "Score", v = mean(chinese.restaurant))
histogram(latin.restaurant, xlab = "Score", v = mean(latin.restaurant))

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.latin <- mean(latin.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.latin
#median
observed.median.score.chinese <- median(chinese.restaurant)
observed.median.score.latin <- median(latin.restaurant)
observed.median.score.diff <- observed.median.score.chinese - observed.median.score.latin
#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.latin <- mean(latin.restaurant, trim = 0.25)
observed.trmean.score.diff <- observed.trmean.score.chinese - observed.trmean.score.latin

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
(sum(meanDiff < observed.mean.diff) +1) / (N+1) # p-val

histogram(medianDiff, xlab = "Chinese Median Score - Latin Median Score", v = observed.median.diff)

histogram(trmeanDiff, xlab = "Chinese Trimmed Median Score - Latin Trimmed Median Score", v = observed.trmean.score.diff)
(sum(trmeanDiff < observed.trmean.score.diff) +1) / (N+1) # p-val

####Permutation test for grades of b/w Chinese & Asian####
# Assumption is that Chinese restaurant is more likely to be dirty, we can test this with permutation test,
# see whether the grades for Chinese restaurant is smaller than that of American cuisine
# Null hypothesis: avg score are the same
# Alt hypothesis: avg score for chinese restaurant is higher

chinese.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Chinese", drop = T)
asian.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Asian", drop = T)
combined.chinese.asian <- c(asian.restaurant, chinese.restaurant)

histogram(chinese.restaurant, xlab = "Score", v = mean(chinese.restaurant))
histogram(asian.restaurant, xlab = "Score", v = mean(asian.restaurant))

#mean
observed.avg.score.chinese <- mean(chinese.restaurant)
observed.avg.score.asian <- mean(asian.restaurant)
observed.mean.diff <- observed.avg.score.chinese - observed.avg.score.asian # -0.33
#median
observed.median.score.chinese <- median(chinese.restaurant)
observed.median.score.asian <- median(asian.restaurant)
observed.median.score.diff <- observed.median.score.chinese - observed.median.score.asian
#trimmed mean
observed.trmean.score.chinese <- mean(chinese.restaurant, trim = 0.25)
observed.trmean.score.asian <- mean(american.restaurant, trim = 0.25)
observed.trmean.score.diff <- observed.trmean.score.chinese - observed.trmean.score.asian # -0.26

m <- length(chinese.restaurant)
total <- length(chinese.restaurant) + length(asian.restaurant)

meanDiff <- numeric(N)
medianDiff <- numeric(N)
trmeanDiff <- numeric(N)

for(i in 1 : N)
{
  index <- sample(total, m, replace = FALSE)
  meanDiff[i] <- mean((combined.chinese.asian[index])) - mean((combined.chinese.asian[-index]))
  medianDiff[i] <- median((combined.chinese.asian[index])) - median((combined.chinese.asian[-index]))
  trmeanDiff[i] <- mean((combined.chinese.asian[index]), trim = 0.25) - mean((combined.chinese.asian[-index]), trim = 0.25)
}

histogram(meanDiff, xlab = "Chinese Avg Score - Asian Avg Score", v = observed.mean.diff)
(sum(meanDiff < observed.mean.diff) +1) / (N+1) # p-val

histogram(medianDiff, xlab = "Chinese Median Score - Asian Median Score", v = observed.median.diff)

histogram(trmeanDiff, xlab = "Chinese Trimmed Median Score - Asian Trimmed Median Score", v = observed.trmean.score.diff)
(sum(trmeanDiff < observed.trmean.score.diff) +1) / (N+1) # p-val


####Permutation test for grades of b/w Latin & Asian####
#
#
#
#
asian.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Asian", drop = T)
latin.restaurant <- subset((restaurant.clean), select = score, cuisine_group_factor == "Latin/Spanish", drop = T)
combined.asian.latin <- c(asian.restaurant, latin.restaurant)

histogram(asian.restaurant, xlab = "Score", v = mean(asian.restaurant))
histogram(latin.restaurant, xlab = "Score", v = mean(latin.restaurant))

#mean
observed.avg.score.asian <- mean(asian.restaurant)
observed.avg.score.latin <- mean(latin.restaurant)
observed.mean.diff <- observed.avg.score.asian - observed.avg.score.latin
#median
observed.median.score.asian <- median(asian.restaurant)
observed.median.score.latin <- median(latin.restaurant)
observed.median.score.diff <- observed.median.score.asian - observed.median.score.latin
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
(sum(meanDiff < observed.mean.diff) +1) / (N+1) # p-val

histogram(medianDiff, xlab = "Chinese Median Score - Latin Median Score", v = observed.median.diff)

histogram(trmeanDiff, xlab = "Asian Trimmed Median Score - Latin Trimmed Median Score", v = observed.trmean.score.diff)
(sum(trmeanDiff < observed.trmean.score.diff) +1) / (N+1) # p-val

#####Cuisine independence from borough#####
table.boro.cuisine <- table((restaurant.clean)$boro, (restaurant.clean)$cuisine_group_factor) # remove one NA in Asian restaurant
colSums(table.boro.cuisine)
chisq.test(table.boro.cuisine) # test of independence bw borough & cuisine
# p value smaller than .05. 
# We reject the null hypothesis & we have overwhelming evidence that boroughs is correlated with cuisine

#####Score independence from cuisine#####
table.score.cuisine <- table((restaurant.clean)$score, (restaurant.clean)$cuisine_group_factor) # remove one NA in Asian restaurant
colSums(table.score.cuisine)
chisq.test(table.score.cuisine) # test of independence bw borough & cuisine
# There are slots with values < 5 so this might not be a good test
# p value smaller than .05. 
# We reject the null hypothesis & we have overwhelming evidence that boroughs is correlated with cuisine

####Bootstrap for rodent####
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
# the distribution looks very normal

####Bootstrap for roach####
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
# the distribution is slightly skew right

