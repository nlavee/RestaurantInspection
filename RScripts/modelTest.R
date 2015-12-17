#### FIT MODELS TO SCORE ~ CUISINE + MEDIAN INCOME ####
restaurant.clean.2 <- subset(restaurant.clean, score != -1, drop = T) # Get rid of restaurants with score = -1 -- there are 5

fit<- lm(sqrt(score)~cuisine_group_factor + median_income.list, data=restaurant.clean.2)
fit2<- lm(sqrt(score)~ median_income.list, data=restaurant.clean.2)
summary(fit)
summary(fit2)
# Income is confounded by cuisine type
# Seems like the things that matter are being an Asian, Chinese, Italian, Latin/Spanish and Pizza place.
# All these places have an influence to increase the score
# Median income does not matter at all

qqnorm(residuals(fit))
qqline(residuals(fit))

#### FIT MODELS TO SCORE ~ CUISINE + MEAN INCOME ####
fit2<- lm(sqrt(score)~cuisine_group_factor + mean_income.list, data=restaurant.clean.2)
summary(fit2)
# Same as with median income

qqnorm(residuals(fit2))
qqline(residuals(fit2))


#### OVERKILL, DON'T EVEN THINK ABOUT USING THIS ####
#### LOGISTIC REGRESSION ON RODENT AND (INCOME & CUISINE GROUP) ####
modlog <- glm(rodent_list ~ log(median_income.list) + cuisine_group_factor, data = restaurant.clean, family = binomial)
summary(modlog)
exp(confint(modlog))

modlog2 <- glm(roach_list ~ log(median_income.list) + cuisine_group_factor, data = restaurant.clean, family = binomial)
summary(modlog2)
exp(confint(modlog2))

#### LOGISTIC REGRESSION ON RODENT AND (BOROUGH & CUISINE GROUP) ####
modlog3 <- glm(rodent_list ~ boro + cuisine_group_factor, data = na.omit(restaurant.clean), family = binomial)
summary(modlog3)
exp(confint(modlog3))

