load_all()

load(file = "inst/scripts/pc1_comparison/old_pubs_model.RData")

require(foreach)
require(doParallel)
registerDoParallel(4)

options(parallel = FALSE)
options(sample_iter = 5)
outdir = ("inst/out/models with new pubs/")

load_data("drivers")
load_data("decadal")
load_data("change")

###############################
# Fitting a model to new_pubs #
###############################

# We're going to run a boosted regression tree model. We'll use Jane Elith's function gbm.step() from the dismo package. This means that we must spend some time getting our data arranged correctly.

# Since this isn't a decade-sliced outcome, we're just going to use the 1990 decadal layer and the 1985 change layer. These are already in the drivers data frame except for pasture and crop change.
d3.1 <- merge(drivers, change[change$year == 1985, c("gridid", "past_change", "crop_change")], by = "gridid")

# Though you use gbm.step with the indices of the variables, I'm going to just write out a formula and extract the correct indices from it. The ts_brt function, by the way, does this automatically.
f3.1 <- new_pubs ~ pop + crop + past + pop_change + crop_change + past_change + mamdiv + urban_land
# Subset d3.1 so it contains only things we're interested in.
d3.1 <- d3.1[, names(d3.1) %in% c("gridid", "lon", "lat", all.vars(f3.1))]

x3.1 <- which(colnames(d3.1) %in% all.vars(f3.1[[3]]))
y3.1 <- which(colnames(d3.1) %in% all.vars(f3.1[[2]]))

m3.1 <- gbm.step(data = d3.1, gbm.x = x3.1, gbm.y = y3.1,
                 family = "gaussian",
                 tree.complexity = 1,
                 learning.rate = 0.01,
                 n.trees = 50)
sink(paste0(outdir, "m3.1.variable_importance.txt"))
summary(m3.1)
sink()
png(file = paste0(outdir, "m3.1.partial_dependence.png"), height = 2100, width = 2100, res = 288)
gbm.plot(m3.1)
dev.off()
# This is weird. It just fit a model *immediately*, without even going through more trees. And urban_land is *entirely* responsible.

m3.2 <- gbm.step(data = d3.1, gbm.x = x3.1, gbm.y = y3.1,
                 family = "gaussian",
                 tree.complexity = 1,
                 learning.rate = 0.001,
                 n.trees = 50)

summary(m3.2)
gbm.plot(m3.2)
# Same thing happens here. Perhaps this is because I'm using "gaussian" and really I should be log transforming it because it's more like Poisson *really*, even though I artificially introduced decimals with "weighted_match".

summary(d3.1$new_pubs)

# I'm gonna try rounding it and doing Poisson, I think. Especially because the mean is 100 and the max is 36,000, so we won't be losing too much information.

d3.1$new_pubs_round <- round(d3.1$new_pubs, digits = 0)
qplot(d3.1$new_pubs, d3.1$new_pubs_round) # To check how much noise was introduced.
ggsave(paste0(outdir, "m3.3.new_pubs_round.png"))
qplot(log(d3.1$new_pubs), log(d3.1$new_pubs_round))
ggsave(paste0(outdir, "m3.3.log(new_pubs_round).png"))

f3.3 <- new_pubs_round ~ pop + crop + past + pop_change + crop_change + past_change + mamdiv + urban_land
x3.3 <- which(colnames(d3.1) %in% all.vars(f3.3[[3]]))
y3.3 <- which(colnames(d3.1) %in% all.vars(f3.3[[2]]))
m3.3 <- gbm.step(data = d3.1, gbm.x = x3.3, gbm.y = y3.3,
                 family = "poisson",
                 tree.complexity = 1,
                 learning.rate = 0.01,
                 n.trees = 50)
sink(paste0(outdir, "m3.3.variable_importance.txt"))
summary(m3.3)
sink()
png(file = paste0(outdir, "m3.3.partial_dependence.png"), height = 2100, width = 2100, res = 288)
gbm.plot(m3.3)
dev.off()

# I'm going to try it now with tree.complexity = 3, so we can account for interactions.

m3.4 <- gbm.step(data = d3.1, gbm.x = x3.3, gbm.y = y3.3,
                 family = "poisson",
                 tree.complexity = 3,
                 learning.rate = 0.01,
                 n.trees = 50)
sink(paste0(outdir, "m3.4.variable_importance.txt"))
summary(m3.4)
sink()
png(file = paste0(outdir, "m3.4.partial_dependence.png"), height = 2100, width = 2100, res = 288)
gbm.plot(m3.4)
dev.off()

# Let's examine interactions.
sink(paste0(outdir, "m3.4.interactions.txt"))
i3.4 <- gbm.interactions(m3.4)
i3.4[[1]]
i3.4[[2]]
sink()

p3.4 <- pred
p3.4$predict.link <- predict.gbm(m3.4, newdata = p3.4, n.trees = m3.4$n.trees, type = "link")
p3.4$predict.response <- predict.gbm(m3.4, newdata = p3.4, n.trees = m3.4$n.trees, type = "response")


quickmap(p3.4, predict.link) + nolegend + nolabs
ggsave(paste0(outdir, "m3.4.predict.link.png"), height = 3, width = 7)

quickmap(p3.4, log(new_pubs + 1)) + nolegend + nolabs
ggsave(paste0(outdir, "m3.4.log(new_pubs).png"), height = 3, width = 7)

quickmap(p3.4, predict.response) + nolegend + nolabs
ggsave(paste0(outdir, "m3.4.predict.response.png"), height = 3, width = 7)

quickmap(p3.4, new_pubs) + nolegend + nolabs
ggsave(paste0(outdir, "m3.4.new_pubs.png"), height = 3, width = 7)

quickmap(p3.4, quantvar(new_pubs)) + nolegend + nolabs
ggsave(paste0(outdir, "m3.4.quantvar_new_pubs.png"), height = 3, width = 7)

quickmap(p3.4, quantvar(predict.response)) + nolegend + nolabs
ggsave(paste0(outdir, "m3.4.quantvar_predict_link.png"), height = 3, width = 7)


qplot(x = log(new_pubs), y = predict.link, data = p3.4, xlim = c(0, 10), ylim = c(0, 10)) # Fitted vs. Actual
ggsave(paste0(outdir, "m3.4.new_pubs_vs_fitted.png"))
qplot(sample = m3.4$residuals) # Fitted vs. Actual
ggsave(paste0(outdir, "m3.4.Q-Q.png"))
qplot(x = predict.response, y = new_pubs, data = p3.4) # Q-Q plot
str(m3.4)
quickmap(p3.4, m3.4$residuals)
p3.4$fitted <- m3.4$fitted
p3.4$residuals <- m3.4$residuals
qplot(log(p3.4$fitted), p3.4$residuals)

p3.4$cv.statistics$deviance.mean
p3.4$cv.statistics$correlation.sep3.4$cv.statistics$deviance.se
p3.4$cv.statistics$correlation.mean

png(file = paste0(outdir, "m3.4.int.pop.urban_land.png"), height = 2100, width = 2100, res = 288)
gbm.perspec(m3.4, 1, 3, x.range = c(0, 1.5e+07))
dev.off()

png(file = paste0(outdir, "m3.4.int.urban_land.crop.png"), height = 2100, width = 2100, res = 288)
gbm.perspec(m3.4, 3, 5)
dev.off()

png(file = paste0(outdir, "m3.4.int.pop_change.crop.png"), height = 2100, width = 2100, res = 288)
gbm.perspec(m3.4, 6, 5)
dev.off()

save.image(file = "inst/cache/3. fitting a model to new_pubs.RData")

