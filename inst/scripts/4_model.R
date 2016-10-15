library(pubcrawler2hotspots)

library(dismo)
library(ggplot2)
library(dplyr)

data(predictors)
data(pubs_df)
data(pubs_df_all)
predicted <- predictors %>%
  left_join(select(pubs_df_all, gridid, lon, lat, w1b.response))

# We'll be fitting a model to w1b.


f1 <- w1b ~ pop + dalys + health_exp + gdp + acc_50k + diseases
gbm_data <- as.data.frame(inner_join(predictors,
                                     select(pubs_df, lon, lat, w1b)))
# We do this here because we only want to na.omit for the variables of interest.
gbm_data <- gbm_data[, which(colnames(gbm_data) %in% c(all.vars(f1)))]
gbm_data <- na.omit(gbm_data)

x_vars <- which(colnames(gbm_data) %in% all.vars(f1[[3]]))
y_var <- which(colnames(gbm_data) %in% all.vars(f1[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.
# With the above settings, the holdout deviance never really increases.

m1 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 3,
               learning.rate = 0.01,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "poisson")
summary(m1)
gbm.plot(m1)

predicted$m1.response <- predict(m1, predicted, m1$n.trees, type = "response")
predicted$m1.link <- predict(m1, predicted, m1$n.trees, type = "link")
quickmap(predicted, m1.response)
quickmap(predicted, m1.link)

# Compare to a model fit from old vars.
quickmap(predicted, w1b.response)
quickmap(predicted, log(w1b.response))

qplot(log(w1b.response), m1.link, data = predicted)
l1 <- lm(m1.link ~ log(w1b.response), data = predicted)
summary(l1)
quickmap(predicted, l1$residuals)



# Second model: integrating urban land measures. Two new models; one for old, one for new.
data(predictors_old)

predictors <- predictors_old %>%
  select(gridid, lon, lat, urban_land) %>%
  right_join(predictors)

data(hotspots_drivers)

predictors <- hotspots_drivers %>%
  select(gridid, earth9_urban) %>%
  right_join(predictors)

predicted <- predictors %>%
  select(which(!names(predictors) %in% names(predicted)), gridid) %>%
  right_join(predicted)



# Model two, with the old urban_land var.

f2 <- w1b ~ pop + dalys + health_exp + gdp + acc_50k + diseases + urban_land
gbm_data <- as.data.frame(inner_join(predictors,
                                     select(pubs_df, lon, lat, w1b)))
# We do this here because we only want to na.omit for the variables of interest.
gbm_data <- gbm_data[, which(colnames(gbm_data) %in% c(all.vars(f2)))]
gbm_data <- na.omit(gbm_data)

x_vars <- which(colnames(gbm_data) %in% all.vars(f2[[3]]))
y_var <- which(colnames(gbm_data) %in% all.vars(f2[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.
# With the above settings, the holdout deviance never really increases.

m2 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 3,
               learning.rate = 0.01,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "poisson")
summary(m2)
gbm.plot(m2)

predicted$m2.response <- predict(m2, predicted, m2$n.trees, type = "response")
predicted$m2.link <- predict(m2, predicted, m2$n.trees, type = "link")
quickmap(predicted, m2.response)
quickmap(predicted, m2.link)

# Compare to a model fit from old vars, and other models
qplot(m1.response, m2.response, data = predicted)
qplot(m1.link, m2.link, data = predicted)
qplot(log(w1b.response), m2.link, data = predicted)
l2 <- lm(m2.link ~ log(w1b.response), data = predicted)
summary(l2)
quickmap(predicted, l2$residuals)
l2 <- lm(m2.link ~ m1.link, data = predicted)
summary(l2)
quickmap(predicted, l2$residuals)
# Basically no difference.


# Trying with the new urban land variable, since it might be better and more
# consistent to use it than to use the old measure.

f3 <- w1b ~ pop + dalys + health_exp + gdp + acc_50k + diseases + earth9_urban
gbm_data <- as.data.frame(inner_join(predictors,
                                     select(pubs_df, lon, lat, w1b)))
# We do this here because we only want to na.omit for the variables of interest.
gbm_data <- gbm_data[, which(colnames(gbm_data) %in% c(all.vars(f3)))]
gbm_data <- na.omit(gbm_data)

x_vars <- which(colnames(gbm_data) %in% all.vars(f3[[3]]))
y_var <- which(colnames(gbm_data) %in% all.vars(f3[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.
# With the above settings, the holdout deviance never really increases.

m3 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 3,
               learning.rate = 0.01,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "poisson")
summary(m3)
gbm.plot(m3)

# Huh, this urban variable is much more correlated. That's what I would have expected.

predicted$m3.response <- predict(m3, predicted, m3$n.trees, type = "response")
predicted$m3.link <- predict(m3, predicted, m3$n.trees, type = "link")
quickmap(predicted, m3.response)
quickmap(predicted, m3.link)

# Compare to a model fit from old vars, and other models
qplot(m2.response, m3.response, data = predicted)
# This is interesting. There are only a few grid cells that are quite a lot
# different. The urban land measure seems to correlate to a few highly-matched
# grid cells.

qplot(m2.link, m3.link, data = predicted)
qplot(log(w1b.response), m3.link, data = predicted)
l3 <- lm(m3.link ~ log(w1b.response), data = predicted)
summary(l3)
quickmap(predicted, l3$residuals)
l3 <- lm(m3.link ~ m2.link, data = predicted)
summary(l3)
quickmap(predicted, l3$residuals)
quickmap(predicted, m3.link - m2.link)
quickmap(predicted, m3.response - m2.response)

# This is what I will use.







# Previously we had tree complexity 3. We should do that.


pubs_fit <- predicted %>%
  select(gridid, lon, lat, pubs_fit = m3.response)

save(pubs_fit, file = "data/pubs_fit.RData")
