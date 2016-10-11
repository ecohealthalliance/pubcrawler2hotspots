library(dplyr)
library(dismo)
library(ggplot2)

data(predictors)
data(pubs_df)
pubs_df <- rename(pubs_df, pubs_count = count,
                           pubs_w1a = w1a,
                           pubs_w1b = w1b,
                           pubs_w2a = w2a,
                           pubs_w2b = w2b,
                           pubs_w3 = w3,
                           pubs_w4 = w4)

f <- pubs_w2b ~ pop + dalys + health_exp + gdp + acc_50k + diseases

gbm_data <- as.data.frame(inner_join(predictors, pubs_df))
gbm_data <- gbm_data[, which(colnames(gbm_data) %in% c(all.vars(f)))]
gbm_data <- na.omit(gbm_data)

x_vars <- which(colnames(gbm_data) %in% all.vars(f[[3]]))
y_var <- which(colnames(gbm_data) %in% all.vars(f[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var])

m1 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 1,
               learning.rate = 0.01,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "poisson")

predictors$m1.predict <- predict(m1, predictors, m1$n.trees, type = "response")
quickmap(predictors, m1.predict)
quickmap(pubs_df, log(pubs_w2a))

pubs_fit <- predictors %>%
  select(lon, lat, pubs_fit = m1.predict) %>%
  left_join(pubs_df)

qplot(exp(pubs_fit), pubs_w2b, data = pubs_fit)

# I'm pretty sure I need to exponentiate the predict method to get the expected value.
# Or put type = "response" into the predict call, as I just did.

pubs_fit

save(pubs_fit, file = "data/pubs_fit.RData")
