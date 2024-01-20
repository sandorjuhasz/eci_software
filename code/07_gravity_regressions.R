### gravity models
### by sandorjuhasz


library(data.table)
library(dplyr)
library(MASS)
library(pscl)
library(fixest)
library(stargazer)
library(lmtest)
library(sandwich)
library(MazamaSpatialUtils)



### data from CEPII -- http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
trade_df <- fread("../data/Gravity_V202211.csv")
trade_df <- subset(trade_df, year == 2020)
trade_df$country_id_o <- gsub('.2', '', trade_df$country_id_o)
trade_df$country_id_d <- gsub('.2', '', trade_df$country_id_d)



### github data
gh_df <- fread("../data/economy_collaborators.csv")
gh_df$iso3_o <- iso2ToIso3(gh_df$source)
gh_df$iso3_d <- iso2ToIso3(gh_df$destination)
gh_df <- subset(gh_df, is.na(iso3_o) == FALSE)
gh_df <- subset(gh_df, is.na(iso3_d) == FALSE)
gh_df <- gh_df %>%
  group_by(year, iso3_o, iso3_d) %>%
  summarise(gh_vol = mean(weight)) %>%
  rename(country_id_o = iso3_o, country_id_d = iso3_d) %>%
  mutate(log_gh_vol = log10(gh_vol)) %>%
  data.table()



### join
df <- merge(
  trade_df,
  gh_df,
  by = c("year","country_id_o", "country_id_d"),
  all.x = TRUE,
  all.y = FALSE
)
  


### data preparation following JW
df$tz_dist <- abs(df$gmt_offset_2020_o - df$gmt_offset_2020_d)
df$log_pop_o <- log10(df$pop_o)
df$log_pop_d <- log10(df$pop_d)
df$log_sci <- log10(df$scaled_sci_2021)
df$log_baci <- log10(df$tradeflow_baci)
df$log_dist <- log10(df$distw_harmonic)



###  final setting -- drop rows with NAs
df <- subset(df, is.na(dist) == FALSE)
df <- subset(df, is.na(scaled_sci_2021) == FALSE)
df <- subset(df, is.na(tradeflow_baci) == FALSE)
df$log_gh_vol[is.na(df$log_gh_vol)==1] <- 0



# baci and github are UNDIRECTED
ud_baci1 <- trade_df %>%
  dplyr::select(country_id_o, country_id_d, tradeflow_baci) %>%
  data.table()
ud_baci2 <- ud_baci1 %>%
  rename(
    country_id_d =country_id_o, country_id_o = country_id_d
  ) %>%
  dplyr::select(country_id_o, country_id_d, tradeflow_baci) %>%
  data.table()

ud_baci <- rbind(ud_baci1, ud_baci2)
ud_baci <- ud_baci %>%
  dplyr::filter(country_id_o < country_id_d) %>%
  group_by(country_id_o, country_id_d) %>%
  summarise(tradeflow_baci = mean(tradeflow_baci, na.rm = TRUE)) %>%
  data.table()






# models  
summary(sci_m1 <- lm(log_sci ~ log_dist + log_pop_o + log_pop_d, data = df))
summary(trade_m1 <- lm(log_baci ~ log_dist + log_pop_o + log_pop_d, data = df))
summary(gh_m1 <- lm(log_gh_vol ~ log_dist + log_pop_o + log_pop_d, data = df))

summary(sci_m2 <- lm(log_sci ~ log_dist + log_pop_o + log_pop_d + as.factor(country_id_o) + as.factor(country_id_d), data = df))
summary(trade_m2 <- lm(log_baci ~ log_dist + log_pop_o + log_pop_d + as.factor(country_id_o) + as.factor(country_id_d), data = df))
summary(gh_m2 <- lm(log_gh_vol ~ log_dist + log_pop_o + log_pop_d + as.factor(country_id_o) + as.factor(country_id_d), data = df))

stargazer(
  sci_m1,
  sci_m2,
  trade_m1,
  trade_m2,
  gh_m1,
  gh_m2,
  omit.stat = c("f", "ser", "aic"),
  covariate.labels = c("Distance (log)", "Population1 (log)", "Population2 (log)"),
  omit = c("country_id_o", "country_id_d"),
  add.lines=list(
    c("Twoway FE", "No", "Yes", "No", "Yes", "No", "Yes")
  ),
  #type="text"
  out = "../outputs/gravity_ols_v1.html"
)






