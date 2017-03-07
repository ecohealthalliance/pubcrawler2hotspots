# library(pubcrawler2hotspots)
load_all() 

library(dismo)
library(ggplot2)
library(dplyr)


data(predictors)
data(pubs_df)
data(pubs_df_all)
predicted <- predictors %>%
  left_join(dplyr::select(pubs_df_all, gridid, lon, lat, w1b.response))

# The code below generates the final model we use in the paper.

f5 <- w1b ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban
gbm_data <- as.data.frame(inner_join(predictors,
                                     dplyr::select(pubs_df, lon, lat, w1b)))
# We do this here because we only want to na.omit for the variables of interest.
gbm_data <- gbm_data[, which(colnames(gbm_data) %in% c(all.vars(f5)))]
gbm_data <- na.omit(gbm_data)

x_vars <- which(colnames(gbm_data) %in% all.vars(f5[[3]]))
y_var <- which(colnames(gbm_data) %in% all.vars(f5[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.


# Here we're loading the publications model
m5 <- pubs_model

#########################################################
# Look at the distribution to check for overdispersion. #
#########################################################


# Look for overdispersion in w1b.
pubs <- na.omit(pubs_df$w1b)
mean(pubs) # 7.46
var(pubs) # 4919.211

mean(pubs_fit$pubs_fit) # 7.9
var(pubs_fit$pubs_fit) # 3232.2


# There *are* lots of zeroes.
ggplot(gbm_data, aes(x = w1b)) +
  geom_histogram() +
  xlim(-1, 10)

p1 <- glm(f5, data = gbm_data, family = "poisson")
p1
summary(p1)

# The `pscl` package contains functions for odTest (overdispersion) and neg bin.
library(pscl)
startvals <- coef(glm1)
glm2 <- glm.nb(f5, data = gbm_data, link = log)
# I cannot get the negative binomial GLM to run.

glm2 <- glm(f5, data = gbm_data, family = "quasipoisson")
summary(glm2) # This has a HUGE dispersion parameter.

# ZIP
library(boot)

# Without inflation
zip1 <- zeroinfl(w1b ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban,
                 data = gbm_data)

# With simple inflation
zip1 <- zeroinfl(w1b ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban | 1,
                 data = gbm_data)
zinb1 <- zeroinfl(w1b ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban | 1,
                  data = gbm_data,
                  dist = "negbin")

# Complex inflation

zip1 <- zeroinfl(w1b ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban | geonames,
                 data = model_df)


##############################
# Idea: Offset with GeoNames #
##############################

data(geonames_df)
quickmap(geonames_df, geonames)

model_df <- predictors %>%
  dplyr::select(lon, lat, pop, dalys, health_exp, gdp, acc_50k, earth9_urban) %>%
  inner_join(dplyr::select(pubs_df, lon, lat, pubs = w1b)) %>%
  inner_join(geonames_df)
# We do this here because we only want to na.omit for the variables of interest.
model_df <- na.omit(model_df)
model_df$pubs <- round(model_df$pubs)

p1 <- glm(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban, data = model_df, family = "poisson")
p2 <- glm(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban, data = model_df, offset = geonames, family = "poisson")
po1 <- glm(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban, data = model_df, offset = geonames, family = "poisson")
nb1 <- glm.nb(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban + offset(geonames), data = model_df, link = log)
qp1 <- glm(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban, data = model_df, family = "quasipoisson")
qp2 <- glm(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban + geonames, data = model_df, family = "quasipoisson")
qp3 <- glm(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban + offset(geonames), data = model_df, family = "quasipoisson")


zip1 <- zeroinfl(pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban | geonames,
                 data = model_df)

zip1


# If you run the predict function of qp1 on predicted then plot this, it shows
# how far off qp1 is from w1b.response. In other words, in this case, the more
# flexible modeling framework of the boosted regression tree better recreates
# the overdispersed surface presented by the aggregated publication mention
# data.
qplot(w1b.response, exp(qp1), data = predicted)
mean(exp(predict(qp1)))
var(exp(predict(qp1)))





###########################
# BRT Model with GeoNames #
###########################

f6 <- pubs ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban + geonames
gbm_data <- as.data.frame(model_df)

x_vars <- which(colnames(gbm_data) %in% all.vars(f6[[3]]))
gbm_data <- na.omit(gbm_data)
y_var <- which(colnames(gbm_data) %in% all.vars(f6[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.

m6 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 3,
               learning.rate = 0.025,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "poisson")
summary(m6)
gbm.plot(m6)

# What's interesting here is that the geonames variable isn't a strong predictor
# in the BRT model. This implies that what we're seeing ISN'T just bias caused
# by the distribution of GeoNames.

predicted <- left_join(predicted, geonames_df)

predicted$m6.response <- predict(m6, predicted, m6$n.trees, type = "response")
predicted$m6.link <- predict(m6, predicted, m6$n.trees, type = "link")
quickmap(predicted, m6.response)
quickmap(predicted, m6.link)

data(pubs_fit)
predicted <- left_join(predicted, pubs_fit)

qplot(pubs_fit, m6.response, data = predicted)
summary(lm(m6.response ~ pubs_fit, data = predicted))
# Adjusted R2 is 99.13.
# N.B. Using it as an offset doesn't work.

# Compare to a model fit from old vars, and other models
qplot(m4.response, m6.response, data = predicted)
# This is interesting. There are only a few grid cells that are quite a lot
# different. The urban land measure seems to correlate to a few highly-matched
# grid cells.

qplot(m4.link, m6.link, data = predicted)



qplot(log(w1b.response), m6.link, data = predicted)
l6 <- lm(m6.link ~ log(w1b.response), data = predicted)
summary(l6)
quickmap(predicted, l6$residuals)
l6 <- lm(m6.link ~ m4.link, data = predicted)
summary(l6)
quickmap(predicted, l6$residuals)
quickmap(predicted, m6.link - m4.link)
quickmap(predicted, m6.response - m4.response)



# I guess I'd better compare our candidates to the actual outcome data.
names(predicted)


predicted <- left_join(predicted, select(pubs_df, lon, lat, w1b))
qplot(log(w1b), m6.link, data = predicted)
p6 <- glm(w1b ~ m6.link, data = predicted, family = "poisson")
summary(p6)
# McFadden's Pseudo R^2 is 0.7296462
1 - p6$deviance / p6$null.deviance
quickmap(predicted, p6$residuals)
quickmap(predicted, w1b - m6.response)
quickmap(predicted, log(w1b) - log(m6.response))

qplot(log(log(w1b)), m6.link, data = predicted)
p6 <- glm(w1b ~ m6.link, data = predicted, family = "poisson")
summary(p6)

quickmap(predicted, p6$residuals)
quickmap(predicted, log(w1b) - log(m6.response))

qplot(w1b - m4.response, w1b - m6.response, data = predicted)
qplot(log(w1b) - log(m4.response), log(w1b) - log(m6.response), data = predicted)

pubs_fit <- predicted %>%
  select(gridid, lon, lat, pubs_fit = m6.response)