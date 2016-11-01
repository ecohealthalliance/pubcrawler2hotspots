load_all()

data(pc1)
data(pubs_model)
data(pubs_fit)
data(pubs_df_all)

pc <- select(pubs_fit, gridid, lon, lat, pf2 = pubs_fit) %>%
  left_join(select(pc1, gridid, pr1 = pubs_raw, pf1 = pubs_fit)) %>%
  left_join(select(pubs_df, lon, lat, pr2 = w1b))



