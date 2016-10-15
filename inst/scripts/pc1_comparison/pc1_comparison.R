# pc1 is a data frame containing the old version of the PubCrawler layers;
# both the raw count version and the modeled verison.

library(dplyr)

load_all()

data(pc1)
data(pubs_df)
data(pubs_fit)

str(pc1)
str(pubs_df)

pc <- pubs_fit %>%
    rename(pubs2_fit = pubs_fit) %>%
    full_join(pubs_df) %>%
    full_join(pc1)


# A log-log plot shows that the old and new variants are roughly linearly correlated:

qplot(log(w2b), log(pubs_raw), data = pc)

qplot(log(pubs_fit), pubs2_fit, data = pc)
qplot(pubs_fit, exp(pubs2_fit), data = pc)

quickmap(pc, pubs_fit)
quickmap(pc, exp(pubs2_fit))

qplot(pc$pubs2_fit)

summary(lm((pubs_fit) ~ w2b, data = pc))