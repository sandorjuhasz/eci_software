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


# data prepared in 02_data_prep_gravity.ipynb
df <- fread("../outputs/data_gravity_regressions.csv")

### data preparation following JW
df$tz_dist <- abs(df$gmt_offset_2020_o - df$gmt_offset_2020_d)
df$log_pop_o <- log10(df$pop_o)
df$log_pop_d <- log10(df$pop_d)
df$log_gdpcap_o <- log10(df$gdpcap_o)
df$log_gdpcap_d <- log10(df$gdpcap_d)
df$log_sci <- log10(df$scaled_sci_2021)
df$log_baci <- log10(df$tradeflow_baci)
df$log_dist <- log10(df$distw_harmonic)
df$gh_vol[is.na(df$gh_vol)==1] <- 0
df$log_gh_vol <- log10(df$gh_vol + 1)


###  final setting -- drop rows with NAs
df <- subset(df, is.na(dist) == FALSE)
df <- subset(df, is.na(scaled_sci_2021) == FALSE)
df <- subset(df, is.na(tradeflow_baci) == FALSE)



### models  
summary(sci_m1 <- lm(log_sci ~ log_dist + log_gdpcap_o + log_gdpcap_d + log_pop_o + log_pop_d, data = df))
summary(trade_m1 <- lm(log_baci ~ log_dist + log_gdpcap_o + log_gdpcap_d + log_pop_o + log_pop_d, data = df))
summary(gh_m1 <- lm(log_gh_vol ~ log_dist + log_gdpcap_o + log_gdpcap_d + log_pop_o + log_pop_d, data = df))

summary(sci_m2 <- lm(log_sci ~ log_dist + log_gdpcap_o + log_gdpcap_d + log_pop_o + log_pop_d + as.factor(country_id_o) + as.factor(country_id_d), data = df))
summary(trade_m2 <- lm(log_baci ~ log_dist + log_gdpcap_o + log_gdpcap_d + log_pop_o + log_pop_d + as.factor(country_id_o) + as.factor(country_id_d), data = df))
summary(gh_m2 <- lm(log_gh_vol ~ log_dist + log_gdpcap_o + log_gdpcap_d + log_pop_o + log_pop_d + as.factor(country_id_o) + as.factor(country_id_d), data = df))

stargazer(
  gh_m1,
  gh_m2,
  sci_m1,
  sci_m2,
  trade_m1,
  trade_m2,
  omit.stat = c("f", "ser", "aic"),
  dep.var.caption = "",
  dep.var.labels = c("Software collab", "Facebook", "Trade"),
  covariate.labels = c("Distance (log)", "GDP per cap1 (log)", "GDP per cap2 (log)", "Population1 (log)", "Population2 (log)"),
  omit = c("country_id_o", "country_id_d"),
  add.lines=list(
    c("Twoway FE", "No", "Yes", "No", "Yes", "No", "Yes")
  ),
  #type="text"
  out = "../outputs/table4_gravity_models.html"
)






