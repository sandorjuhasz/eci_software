### ECI software / trade / technology / research regressions
### by sandorjuhasz


library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)


# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/eci_comparisons_2020.csv")


# manipulation
df$log_gpd_pc <- log10(df$gdpcap_o)
df$gini_norm <- scale(df$gini_mean)
df$emission_norm <- scale(df$embodied_emissions)
df$software_eci_norm <- scale(df$software_eci_2020)
df$trade_eci_norm <- scale(df$trade_eci_2020)
df$tech_eci_norm <- scale(df$tech_eci_2020)
df$research_eci_norm <- scale(df$research_eci_2020)
df$log_pop <- log10(df$pop_o)


### GPD vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop")
reg_df <- df[complete.cases(df[, ..key_columns]), ]

# models
summary(m1 <- lm(log_gpd_pc ~ software_eci_norm + log_pop, data = reg_df))
summary(m2 <- lm(log_gpd_pc ~ trade_eci_norm + log_pop, data = reg_df))
summary(m3 <- lm(log_gpd_pc ~ tech_eci_norm + log_pop, data = reg_df))
summary(m4 <- lm(log_gpd_pc ~ research_eci_norm + log_pop, data = reg_df))
summary(m5a <- lm(log_gpd_pc ~ software_eci_norm + trade_eci_norm + log_pop, data = reg_df))
summary(m5b <- lm(log_gpd_pc ~ software_eci_norm + tech_eci_norm + log_pop, data = reg_df))
summary(m5c <- lm(log_gpd_pc ~ software_eci_norm + research_eci_norm + log_pop, data = reg_df))
summary(m6 <- lm(log_gpd_pc ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop, data = reg_df))

stargazer(
  m1,
  m2,
  m3,
  m4,
  m5a,
  m5b,
  m5c,
  m6,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("GPD per capita 2020 (log)"),
  covariate.labels = c("ECI software", "ECI trade", "ECI technology", "ECI research", "Population (log)"),
  out = "../outputs/table1_gdp_eci_regressions.html"
)




### Gini vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("gini_norm", "log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop")
reg_df <- df[complete.cases(df[, ..key_columns]), ]

# models
summary(m1 <- lm(gini_norm ~ software_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m2 <- lm(gini_norm ~ trade_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m3 <- lm(gini_norm ~ tech_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m4 <- lm(gini_norm ~ research_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m5a <- lm(gini_norm ~ software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m5b <- lm(gini_norm ~ software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m5c <- lm(gini_norm ~ software_eci_norm + research_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m6 <- lm(gini_norm ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop, data = reg_df))

stargazer(
  m1,
  m2,
  m3,
  m4,
  m5a,
  m5b,
  m5c,
  m6,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("Gini"),
  covariate.labels = c("ECI software", "ECI trade", "ECI technology", "ECI research", "GPD per capita (log)", "Population (log)"),
  out = "../outputs/table2_gini_eci_regressions.html"
)




### emissions vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("emission_norm", "log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop")
reg_df <- df[complete.cases(df[, ..key_columns]), ]

# models
summary(m1 <- lm(emission_norm ~ software_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m2 <- lm(emission_norm ~ trade_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m3 <- lm(emission_norm ~ tech_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m4 <- lm(emission_norm ~ research_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m5a <- lm(emission_norm ~ software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m5b <- lm(emission_norm ~ software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m5c <- lm(emission_norm ~ software_eci_norm + research_eci_norm + log_gpd_pc + log_pop, data = reg_df))
summary(m6 <- lm(emission_norm ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop, data = reg_df))

stargazer(
  m1,
  m2,
  m3,
  m4,
  m5a,
  m5b,
  m5c,
  m6,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("Emission"),
  covariate.labels = c("ECI software", "ECI trade", "ECI technology", "ECI research", "GPD per capita (log)", "Population (log)"),
  out = "../outputs/table3_emission_eci_regressions.html"
)

