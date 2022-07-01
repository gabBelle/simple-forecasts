
# table_test

table_test = snaive(df, drift = T, nmeans = 1)

table_test %>% glimpse()

# exploratory analysis

ggplot(data = table_test)+
  aes(x = date)+
  geom_line(aes(y = vl, col = "SNaive"))+
  geom_line(aes(y = drift, col = "Drift"))+
  geom_line(aes(y = vl_drift, col = "+Drift"))+
  labs(y = "value", x = "")+
  theme_bw()+
  theme(legend.position = "bottom")

# ----------------------------------------------------------------------------------------



