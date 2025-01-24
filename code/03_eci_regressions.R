### main regression models
### by sandorjuhasz



# --- Table 1 -- GDP per capita vs ECI software
# --- Table 2 -- Gini vs ECI software
# --- Table 3 -- Emission vs ECI software
# --- Table 4 -- Entry models
# --- Table 5 -- Exit models



library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)



# baseline dataframe from 01_data_prep_complexity.ipynb
df <- fread("../outputs/eci_regression_table.csv")

# IVs from 01_data_prep_complexity.ipynb
df_iv <- fread("../outputs/si_eci_software_2020_2023_ivreg.csv") %>%
  select(iso2_code, year, avg_eci_similar_spec) %>%
  unique() %>%
  filter(year == 2020)
df <- merge(
  df,
  df_iv,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)

# manipulation
df <- df %>%
  group_by(year) %>%
  mutate(
    log_gdp_usd = log10(gdp_current_USD),
    log_gdp_ppp = log10(gdp_ppp),
    gdp_ppp_pc = gdp_ppp / population,
    log_gdp_ppp_pc = log10(gdp_ppp_pc),
    log_gdp_ppp_pc2 = log_gdp_ppp_pc^2,
    gini_norm = scale(gini_mean),
    gini_2020_2022_norm = scale(gini_mean_2020_2022),
    log_emission = log10(total_ghg_emissions),
    emission_per_gdp = (total_ghg_emissions / gdp_ppp),
    log_emission_per_gdp = log10(total_ghg_emissions / gdp_ppp),
    eci_software_norm = scale(eci_software),
    eci_trade_norm = scale(eci_trade),
    eci_tech_norm = scale(eci_tech),
    eci_research_norm = scale(eci_research),
    log_pop = log10(population),
    log_nat_res = log10(natural_resources)
  ) %>%
  data.table()






# --- Table 1 -- GDP per capita vs ECI software
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp_ppp_pc ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m02 <- feols(log_gdp_ppp_pc ~ eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m03 <- feols(log_gdp_ppp_pc ~ eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m04 <- feols(log_gdp_ppp_pc ~ eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m05 <- feols(log_gdp_ppp_pc ~ eci_software_norm + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m06 <- feols(log_gdp_ppp_pc ~ eci_software_norm + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m07 <- feols(log_gdp_ppp_pc ~ eci_software_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_ppp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm01 <- feols(log_gdp_ppp_pc ~ 1 | eci_software_norm + log_pop + log_nat_res ~ sim_eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm08 <- feols(log_gdp_ppp_pc ~ 1 | eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res ~ sim_eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable(
  gdp_m01, gdp_m02, gdp_m03, gdp_m04, gdp_m05, gdp_m06, gdp_m07, gdp_m08, gdp_ivm01, gdp_ivm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- Table 2 -- Gini vs ECI software
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("gini_norm", "log_gdp_ppp_pc", "log_gdp_ppp_pc2", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m02 <- feols(gini_norm ~ eci_trade_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m03 <- feols(gini_norm ~ eci_tech_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m04 <- feols(gini_norm ~ eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m05 <- feols(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m06 <- feols(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m07 <- feols(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm01 <- feols(gini_norm ~ 1 | eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res ~ sim_eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm08 <- feols(gini_norm ~ 1 | eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res ~ sim_eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable(
  gini_m01, gini_m02, gini_m03, gini_m04, gini_m05, gini_m06, gini_m07, gini_m08, gini_ivm01, gini_ivm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- Table 3 -- Emission vs ECI software
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_emission_per_gdp", "log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(log_emission_per_gdp ~ eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m02 <- feols(log_emission_per_gdp ~ eci_trade_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m03 <- feols(log_emission_per_gdp ~ eci_tech_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m04 <- feols(log_emission_per_gdp ~ eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m05 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m06 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_tech_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m07 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm01 <- feols(log_emission_per_gdp ~ 1 | eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res ~ sim_eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm08 <- feols(log_emission_per_gdp ~ 1 | eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res ~ sim_eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable(
  em_m01, em_m02, em_m03, em_m04, em_m05, em_m06, em_m07, em_m08, em_ivm01, em_ivm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






