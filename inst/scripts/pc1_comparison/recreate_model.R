load_all()

library(dismo)
library(gbm)
library(dplyr)

load(file = "inst/scripts/pc1_comparison/old_pubs_model.RData")

d.old <- d3.1

f.old <- new_pubs_round ~ pop + crop + past + pop_change + crop_change + past_change + mamdiv + urban_land
x.old <- which(colnames(d.old) %in% all.vars(f.old[[3]]))
y.old <- which(colnames(d.old) %in% all.vars(f.old[[2]]))



# Let's see if refitting it now gives the same thing
m.old <- gbm.step(data = d.old, gbm.x = x.old, gbm.y = y.old,
                  family = "poisson",
                  tree.complexity = 3,
                  learning.rate = 0.01,
                  n.trees = 50)


summary(m.old)
i.old <- gbm.interactions(m.old)
i.old[[1]]
i.old[[2]]
gbm.plot(m.old)

summary(m3.4)
i3.4 <- gbm.interactions(m3.4)
i3.4[[1]]
i3.4[[2]]
gbm.plot(m3.4)

p.old <- p3.4
p.old$predict.link <- predict.gbm(m.old, newdata = p.old, n.trees = m.old$n.trees, type = "link")
p.old$predict.response <- predict.gbm(m.old, newdata = p.old, n.trees = m3.4$n.trees, type = "response")



# Here, we have the new publication layer.

data(pubs_df)

d.new <- d.old %>%
    left_join(pubs_df)

m.old <- gbm.step(data = d.old, gbm.x = x.old, gbm.y = y.old,
                  family = "poisson",
                  tree.complexity = 3,
                  learning.rate = 0.01,
                  n.trees = 50)


summary(m.old)
i.old <- gbm.interactions(m.old)
i.old[[1]]
i.old[[2]]
gbm.plot(m.old)

summary(m3.4)
i3.4 <- gbm.interactions(m3.4)
i3.4[[1]]
i3.4[[2]]
gbm.plot(m3.4)

p.old <- p3.4
p.old$predict.link <- predict.gbm(m.old, newdata = p.old, n.trees = m.old$n.trees, type = "link")
p.old$predict.response <- predict.gbm(m.old, newdata = p.old, n.trees = m3.4$n.trees, type = "response")