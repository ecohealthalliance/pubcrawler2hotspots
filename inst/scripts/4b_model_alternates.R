# library(pubcrawler2hotspots)
load_all() 

library(dismo)
library(ggplot2)
library(dplyr)

data(predictors)
data(pubs_df)
data(pubs_df_all)
predicted <- predictors %>%
  left_join(select(pubs_df_all, gridid, lon, lat, w1b.response))

# The code below generates the final model we use in the paper.

f5 <- w1b ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban
gbm_data <- as.data.frame(inner_join(predictors,
                                     select(pubs_df, lon, lat, w1b)))
# We do this here because we only want to na.omit for the variables of interest.
gbm_data <- gbm_data[, which(colnames(gbm_data) %in% c(all.vars(f5)))]
gbm_data <- na.omit(gbm_data)

x_vars <- which(colnames(gbm_data) %in% all.vars(f5[[3]]))
y_var <- which(colnames(gbm_data) %in% all.vars(f5[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.

# Here we're loading the publications model
data(pubs_model)
m5 <- pubs_model

data(pubs_model)
summary(m5)
gbm.plot(m5)

# Huh, this urban variable is much more correlated. That's what I would have expected.

predicted$m5.response <- predict(m5, predicted, m5$n.trees, type = "response")
predicted$m5.link <- predict(m5, predicted, m5$n.trees, type = "link")
quickmap(predicted, m5.response)
quickmap(predicted, m5.link)

# Compare to a model fit from old vars, and other models
qplot(m4.response, m5.response, data = predicted)
# This is interesting. There are only a few grid cells that are quite a lot
# different. The urban land measure seems to correlate to a few highly-matched
# grid cells.

qplot(m4.link, m5.link, data = predicted)



qplot(log(w1b.response), m5.link, data = predicted)
l5 <- lm(m5.link ~ log(w1b.response), data = predicted)
summary(l5)
quickmap(predicted, l5$residuals)
l5 <- lm(m5.link ~ m4.link, data = predicted)
summary(l5)
quickmap(predicted, l5$residuals)
quickmap(predicted, m5.link - m4.link)
quickmap(predicted, m5.response - m4.response)



# I guess I'd better compare our candidates to the actual outcome data.
names(predicted)


predicted <- left_join(predicted, select(pubs_df, lon, lat, w1b))
qplot(log(w1b), m5.link, data = predicted)
p5 <- glm(w1b ~ m5.link, data = predicted, family = "poisson")
summary(p5)
# McFadden's Pseudo R^2 is 0.7296462
1 - p5$deviance / p5$null.deviance
quickmap(predicted, p5$residuals)
quickmap(predicted, w1b - m5.response)
quickmap(predicted, log(w1b) - log(m5.response))

qplot(log(log(w1b)), m5.link, data = predicted)
p5 <- glm(w1b ~ m5.link, data = predicted, family = "poisson")
summary(p5)

quickmap(predicted, p5$residuals)
quickmap(predicted, log(w1b) - log(m5.response))

qplot(w1b - m4.response, w1b - m5.response, data = predicted)
qplot(log(w1b) - log(m4.response), log(w1b) - log(m5.response), data = predicted)

pubs_fit <- predicted %>%
  select(gridid, lon, lat, pubs_fit = m5.response)
