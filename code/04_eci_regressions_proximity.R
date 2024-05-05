### ECI software / trade / technology / research regressions
### by sandorjuhasz

# Make path of R script the working directory (to easily load *.csv)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)

# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/eci_comparisons_2020.csv")

# Load the synthetic control data from 
proximity_control <- fread("../outputs/proximity_control.csv")
# By using year, it automatically only considers 2020
df <- merge(df, proximity_control, by = c("iso2_code", "year"), all.x = TRUE)
#write.csv(df,"filename.csv", row.names = FALSE)

# manipulation
df$log_gpd_pc <- log10(df$gdpcap_o)
df$gini_norm <- scale(df$gini_mean)
#df$emission_norm <- scale(df$embodied_emissions)
df$emission_norm <- scale(df$emissions)
df$software_eci_norm <- scale(df$software_eci_2020)
df$trade_eci_norm <- scale(df$trade_eci_2020)
df$tech_eci_norm <- scale(df$tech_eci_2020)
df$research_eci_norm <- scale(df$research_eci_2020)
df$log_pop <- log10(df$pop_o)
df$log_nat_res <- log10(df$nat_res)
df$proximity_control_norm <- scale(df$proximity_control)

### GPD vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res", "proximity_control_norm")
df <- df[complete.cases(df[, ..key_columns]), ]
# models
summary(m1 <- lm(log_gpd_pc ~ software_eci_norm + log_pop + log_nat_res, data = df))
summary(m2 <- lm(log_gpd_pc ~ trade_eci_norm + log_pop + log_nat_res, data = df))
summary(m3 <- lm(log_gpd_pc ~ tech_eci_norm + log_pop + log_nat_res, data = df))
summary(m4 <- lm(log_gpd_pc ~ research_eci_norm + log_pop + log_nat_res, data = df))
summary(m5a <- lm(log_gpd_pc ~ software_eci_norm + trade_eci_norm + log_pop + log_nat_res, data = df))
summary(m5b <- lm(log_gpd_pc ~ software_eci_norm + tech_eci_norm + log_pop + log_nat_res, data = df))
summary(m5c <- lm(log_gpd_pc ~ software_eci_norm + research_eci_norm + log_pop + log_nat_res, data = df))
summary(m6 <- lm(log_gpd_pc ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, data = df))
summary(m7 <- lm(log_gpd_pc ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res + proximity_control_norm, data = df))

stargazer(
  m1, m2, m3, m4, m5a, m5b, m5c, m6, m7,
  type = "html",
  out = "../outputs/table1_gdp_eci_regressions.html",
  title = "Regression Results: GDP per Capita and ECI Indicators",
  align = TRUE,
  header = FALSE,
  digits = 3,
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  covariate.labels = c("ECI Software", "ECI Trade", "ECI Technology", "ECI Research", "Population (log)", "Natural Resources (log)", "Proximity Control"),
  omit.stat = c("f", "ser"),
  notes.append = FALSE,
  notes = "Standard errors in parentheses. *p<0.1; **p<0.05; ***p<0.01"
)