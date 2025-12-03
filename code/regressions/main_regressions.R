### main regression models
### by sandorjuhasz



# --- Table 1 -- GDP per capita vs ECI software
# --- Table 2 -- Gini vs ECI software
# --- Table 3 -- Emission vs ECI software
# --- Table 4 -- Entry models 0011 fashion
# --- Table 5 -- Exit models 1100 fashion



library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)
source("../utils/functions.R")



# baseline dataframe from 01_data_prep_complexity.ipynb
# language based
df <- create_baseline_table(
  main_input_path = "../../data/outputs/eci_regression_table.csv",
  iv_input_path = "../../data/outputs/si_eci_software_2020_2023_ivreg.csv"
)
# clusters based
df <- add_clusters_cooc_variables(
  main_input_path = "../../data/outputs/eci_clusters_cooc_2020_2023.csv",
  iv_input_path = "../../data/outputs/si_eci_clusters_cooc_2020_2023_ivreg.csv"
)


# --- Table 1 -- GDP per capita vs ECI software
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_gdp_ppp_pc", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res", "sim_eci_clusters_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m02 <- feols(log_gdp_ppp_pc ~ eci_trade_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m03 <- feols(log_gdp_ppp_pc ~ eci_tech_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m04 <- feols(log_gdp_ppp_pc ~ eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m05 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + eci_trade_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m06 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + eci_tech_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m07 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm01 <- feols(
  log_gdp_ppp_pc ~ ln_pop + ln_nat_res | eci_clusters_norm ~ sim_eci_clusters_norm,
  vcov = "HC1",
  data = reg_df
)
gdp_ivm08 <- feols(
  log_gdp_ppp_pc ~ eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res | eci_clusters_norm ~ sim_eci_clusters_norm,
  vcov = "HC1",
  data = reg_df
)

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
key_columns <- c("gini_2020_2022_norm", "ln_gdp_ppp_pc", "ln_gdp_ppp_pc2", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res", "sim_eci_clusters_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m02 <- feols(gini_2020_2022_norm ~ eci_trade_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m03 <- feols(gini_2020_2022_norm ~ eci_tech_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m04 <- feols(gini_2020_2022_norm ~ eci_research_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m05 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_trade_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m06 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_tech_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m07 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_ivm01 <- feols(gini_2020_2022_norm ~ ln_gdp_ppp_pc + ln_pop + ln_nat_res | eci_clusters_norm ~ sim_eci_clusters_norm, vcov = "HC1", data = reg_df)
gini_ivm08 <-
  feols(
    gini_2020_2022_norm ~ ln_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res | eci_clusters_norm ~ sim_eci_clusters_norm,
    vcov = "HC1",
    data = reg_df
  )

gini_2020_2022_etable <- etable(
  gini_m01, gini_m02, gini_m03, gini_m04, gini_m05, gini_m06, gini_m07, gini_m08, gini_ivm01, gini_ivm08,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)
print(gini_2020_2022_etable)
#save_etable_to_word(gini_2020_2022_etable)






# --- Table 3 -- Emission vs ECI software
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_emission_per_gdp", "ln_gdp_ppp_pc", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res", "sim_eci_clusters_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(log_emission_per_gdp ~ eci_clusters_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m02 <- feols(log_emission_per_gdp ~ eci_trade_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m03 <- feols(log_emission_per_gdp ~ eci_tech_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m04 <- feols(log_emission_per_gdp ~ eci_research_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m05 <- feols(log_emission_per_gdp ~ eci_clusters_norm + eci_trade_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m06 <- feols(log_emission_per_gdp ~ eci_clusters_norm + eci_tech_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m07 <- feols(log_emission_per_gdp ~ eci_clusters_norm + eci_research_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission_per_gdp ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_ivm01 <- feols(log_emission_per_gdp ~ ln_gdp_ppp_pc + ln_pop + ln_nat_res | eci_clusters_norm ~ sim_eci_clusters_norm, vcov = "HC1", data = reg_df)
em_ivm08 <-
  feols(
    log_emission_per_gdp ~ eci_trade_norm + eci_tech_norm + eci_research_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res | eci_clusters_norm ~ sim_eci_clusters_norm,
    vcov = "HC1",
    data = reg_df
  )

etable(
  em_m01, em_m02, em_m03, em_m04, em_m05, em_m06, em_m07, em_m08, em_ivm01, em_ivm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- Table 4 -- Entry models 0011 fashion

# table from 01_data_prep_complexity.ipynb
#en_df <- fread("../outputs/data_entry_regressions_0011.csv")
en_df <- fread("../../data/outputs/data_entry_regressions_0011_clusters_cooc.csv")

# normalize
en_df$rel_density <- scale(en_df$density)
en_df$pci <- scale(en_df$pci)
en_df$ubiquity <- scale(en_df$ubiquity)

ent_m1 <- feols(entry01 ~ density, cluster = "iso2_code", data = en_df)
ent_m2 <- feols(entry01 ~ density | iso2_code, cluster = "iso2_code", data = en_df)
ent_m3 <- feols(entry01 ~ density | cluster_id, cluster = "iso2_code", data = en_df)
ent_m4 <- feols(entry01 ~ density | iso2_code + cluster_id, cluster = "iso2_code", data = en_df)
ent_m5 <- feols(entry01 ~ ubiquity, cluster = "iso2_code", data = en_df)
ent_m6 <- feols(entry01 ~ density + ubiquity, cluster = "iso2_code", data = en_df)
ent_m7 <- feols(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = en_df)

etable(
  ent_m1, ent_m2, ent_m3, ent_m4, ent_m5, ent_m6, ent_m7,
  #fitstat = ~ r2,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- Table 5 -- Exit models 1100 fashion

# table from 01_data_prep_complexity.ipynb
#ex_df <- fread("../outputs/data_exit_regressions_1100.csv")
ex_df <- fread("../../data/outputs/data_exit_regressions_1100_clusters_cooc.csv")

# normalize
ex_df$rel_density <- scale(ex_df$density)
ex_df$pci <- scale(ex_df$pci)
ex_df$ubiquity <- scale(ex_df$ubiquity)

ex_m1 <- feols(exit01 ~ density, cluster = "iso2_code", data = ex_df)
ex_m2 <- feols(exit01 ~ density | iso2_code, cluster = "iso2_code", data = ex_df)
ex_m3 <- feols(exit01 ~ density | cluster_id, cluster = "iso2_code", data = ex_df)
ex_m4 <- feols(exit01 ~ density | iso2_code + cluster_id, cluster = "iso2_code", data = ex_df)
ex_m5 <- feols(exit01 ~ ubiquity, cluster = "iso2_code", data = ex_df)
ex_m6 <- feols(exit01 ~ density + ubiquity, cluster = "iso2_code", data = ex_df)
ex_m7 <- feols(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = ex_df)

etable(
  ex_m1, ex_m2, ex_m3, ex_m4, ex_m5, ex_m6, ex_m7,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)

