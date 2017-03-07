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
data(geonames_df)
quickmap(geonames_df, geonames)

# Following
# http://etd.lsu.edu/docs/available/etd-11042009-152651/unrestricted/Abeare_thesis.pdf,
# we will convert to a zero-truncated lognormal distribution

model_df <- predictors %>%
  dplyr::select(lon, lat, pop, dalys, health_exp, gdp, acc_50k, earth9_urban) %>%
  inner_join(dplyr::select(pubs_df, lon, lat, pubs = w1b)) %>%
  inner_join(geonames_df) %>%
  mutate_each(funs(log = log(.)), -lon, -lat)

# We do this here because we only want to na.omit for the variables of interest.
model_df <- na.omit(model_df)
# model_df <- model_df[is.finite(model_df$pubs_log), ] # If we just want to omit infinite from this

# To clean all infinite values:
is.na(model_df) <- sapply(model_df, is.infinite)
model_df <- na.omit(model_df)


quickmap(model_df, pubs_log)
qqnorm(model_df$pubs_log)
qplot(model_df$pubs_log)

# Now we'll fit a Gaussian model using the transformed zero-truncated Poisson.

f7 <- pubs_log ~ pop + dalys + health_exp + gdp + acc_50k + earth9_urban + geonames
gbm_data <- as.data.frame(model_df)

x_vars <- which(colnames(gbm_data) %in% all.vars(f7[[3]]))
gbm_data <- na.omit(gbm_data)
y_var <- which(colnames(gbm_data) %in% all.vars(f7[[2]]))

gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.

m7 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 3,
               learning.rate = 0.025,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "gaussian")
summary(m7)
gbm.plot(m7)



# Now we'll fit a Gaussian model using the transformed zero-truncated Poisson.

f7 <- pubs_log ~ pop_log + dalys_log + health_exp_log + gdp_log + acc_50k_log + earth9_urban_log + geonames_log
gbm_data <- as.data.frame(model_df)

x_vars <- which(colnames(gbm_data) %in% all.vars(f7[[3]]))
gbm_data <- na.omit(gbm_data)
y_var <- which(colnames(gbm_data) %in% all.vars(f7[[2]]))

# gbm_data[y_var] <- round(gbm_data[y_var]) # For Poisson.

m7 <- gbm.step(gbm_data, x_vars, y_var,
               tree.complexity = 3,
               learning.rate = 0.025,
               bag.fraction = 0.75, 
               n.trees = 50,
               family = "gaussian")
summary(m7)
gbm.plot(m7)
# With a log-transformed model, the relative influence of the variables stays about the same.
# var   rel.inf
# gdp_log                   gdp_log 41.294593
# earth9_urban_log earth9_urban_log 26.201425
# dalys_log               dalys_log 11.019791
# pop_log                   pop_log  7.331912
# geonames_log         geonames_log  6.019461
# health_exp_log     health_exp_log  4.321108
# acc_50k_log           acc_50k_log  3.811711


# Now we'll try a regular GLM with this.
glm1 <- glm(f7, data = gbm_data)
summary(glm1)
# Most strongly associated with… Geonames, urban land, GDP. Less with Health Expenditure and less with accessibility.