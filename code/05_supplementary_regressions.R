### main regression models
### by sandorjuhasz



# --- SI Entry logit
# --- SI Exit logit
# --- SI different RCA thresholds
# --- SI growth regressions
# --- SI Gini and emission regressions w/ GDP + GDP**2
# --- SI Gini 2020-2022 table
# --- SI Identical samples regressions
# --- SI No mathematical dependencies tables
# --- SI VIF tables for main regressions
# --- SI stepwise IV regressions
# --- SI language clusters

# Mean, median, percentiles descriptive tables



library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)
library(flextable)
library(officer)
source("../code/functions.R")



# --- SI Entry logit
en_df <- fread("../outputs/data_entry_regressions_0011_clusters_cooc.csv")

# normalize
en_df$rel_density <- scale(en_df$density)
en_df$pci <- scale(en_df$pci)
en_df$ubiquity <- scale(en_df$ubiquity)

logit_ent_m1 <- feglm(entry01 ~ density, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m2 <- feglm(entry01 ~ density | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m3 <- feglm(entry01 ~ density | cluster_id, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m4 <- feglm(entry01 ~ density | iso2_code + cluster_id, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m5 <- feglm(entry01 ~ ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m6 <- feglm(entry01 ~ density + ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m7 <- feglm(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)

etable(
  logit_ent_m1, logit_ent_m2, logit_ent_m3, logit_ent_m4, logit_ent_m5, logit_ent_m6, logit_ent_m7,
  #fitstat = ~ r2,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- SI Exit logit
ex_df <- fread("../outputs/data_exit_regressions_1100_clusters_cooc.csv")

# normalize
ex_df$rel_density <- scale(ex_df$density)
ex_df$pci <- scale(ex_df$pci)
ex_df$ubiquity <- scale(ex_df$ubiquity)

logit_ex_m1 <- feglm(exit01 ~ density, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m2 <- feglm(exit01 ~ density | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m3 <- feglm(exit01 ~ density | cluster_id, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m4 <- feglm(exit01 ~ density | iso2_code + cluster_id, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m5 <- feglm(exit01 ~ ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m6 <- feglm(exit01 ~ density + ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m7 <- feglm(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)

etable(
  logit_ex_m1, logit_ex_m2, logit_ex_m3, logit_ex_m4, logit_ex_m5, logit_ex_m6, logit_ex_m7,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- SI different RCA thresholds

# baseline -- RCA >= 1
df <- fread("../outputs/eci_regression_table.csv")

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

# threshold 0.75
cdf075 <- fread("../outputs/eci_software_2020_2023_threshold_075.csv") %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(iso2_code, year, eci) %>%
  mutate(eci_software075_norm = scale(eci)) %>%
  dplyr::select(-eci) %>%
  unique()

# threshold 1.25
cdf125 <- fread("../outputs/eci_software_2020_2023_threshold_125.csv") %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(iso2_code, year, eci) %>%
  mutate(eci_software125_norm = scale(eci)) %>%
  dplyr::select(-eci) %>%
  unique()

df <- merge(
  df,
  cdf075,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)
df <- merge(
  df,
  cdf125,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)


# GDP per capita
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_ppp_pc", "eci_software_norm", "eci_software075_norm", "eci_software125_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp_ppp_pc ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_ppp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m01_075 <- feols(log_gdp_ppp_pc ~ eci_software075_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08_075 <- feols(log_gdp_ppp_pc ~ eci_software075_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m01_125 <- feols(log_gdp_ppp_pc ~ eci_software125_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08_125 <- feols(log_gdp_ppp_pc ~ eci_software125_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_m01, gdp_m08, gdp_m01_075, gdp_m08_075, gdp_m01_125, gdp_m08_125,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)


# Gini regressions
reg_df <- subset(df, year==2020)
key_columns <- c("gini_2020_2022_norm", "log_gdp_ppp_pc", "eci_software_norm", "eci_software075_norm", "eci_software125_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m01_075 <- feols(gini_2020_2022_norm ~ eci_software075_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08_075 <- feols(gini_2020_2022_norm ~ eci_software075_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m01_125 <- feols(gini_2020_2022_norm ~ eci_software125_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08_125 <- feols(gini_2020_2022_norm ~ eci_software125_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gini_m01, gini_m08, gini_m01_075, gini_m08_075, gini_m01_125, gini_m08_125,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)


# Emissions regressions
reg_df <- subset(df, year==2020)
key_columns <- c("log_emission_per_gdp", "log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(log_emission_per_gdp ~ eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m01_075 <- feols(log_emission_per_gdp ~ eci_software075_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08_075 <- feols(log_emission_per_gdp ~ eci_software075_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m01_125 <- feols(log_emission_per_gdp ~ eci_software125_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08_125 <- feols(log_emission_per_gdp ~ eci_software125_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable(
  em_m01, em_m08, em_m01_075, em_m08_075, em_m01_125, em_m08_125, 
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)


# ENTRY regressions
en_df <- fread("../outputs/data_entry_regressions_0011.csv")
en_df$rel_density <- scale(en_df$density)
en_df$pci <- scale(en_df$pci)
en_df$ubiquity <- scale(en_df$ubiquity)

en_df075 <- fread("../outputs/data_entry_regressions_0011_threshold_075.csv")
en_df075$rel_density <- scale(en_df075$density)
en_df075$pci <- scale(en_df075$pci)
en_df075$ubiquity <- scale(en_df075$ubiquity)

en_df125 <- fread("../outputs/data_entry_regressions_0011_threshold_125.csv")
en_df125$rel_density <- scale(en_df125$density)
en_df125$pci <- scale(en_df125$pci)
en_df125$ubiquity <- scale(en_df125$ubiquity)

ent_m1 <- feols(entry01 ~ density, cluster = "iso2_code", data = en_df)
ent_m6 <- feols(entry01 ~ density + ubiquity, cluster = "iso2_code", data = en_df)
ent_m7 <- feols(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = en_df)

ent_m1_075 <- feols(entry01 ~ density, cluster = "iso2_code", data = en_df075)
ent_m6_075 <- feols(entry01 ~ density + ubiquity, cluster = "iso2_code", data = en_df075)
ent_m7_075 <- feols(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = en_df075)

ent_m1_125 <- feols(entry01 ~ density, cluster = "iso2_code", data = en_df125)
ent_m6_125 <- feols(entry01 ~ density + ubiquity, cluster = "iso2_code", data = en_df125)
ent_m7_125 <- feols(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = en_df125)


etable(
  ent_m1, ent_m6, ent_m7, ent_m1_075, ent_m6_075, ent_m7_075, ent_m1_125, ent_m6_125, ent_m7_125,
  #fitstat = ~ r2,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)


# EXIT regressions
ex_df <- fread("../outputs/data_exit_regressions_1100.csv")
ex_df$rel_density <- scale(ex_df$density)
ex_df$pci <- scale(ex_df$pci)
ex_df$ubiquity <- scale(ex_df$ubiquity)

ex_df075 <- fread("../outputs/data_exit_regressions_1100_threshold_075.csv")
ex_df075$rel_density <- scale(ex_df075$density)
ex_df075$pci <- scale(ex_df075$pci)
ex_df075$ubiquity <- scale(ex_df075$ubiquity)

ex_df125 <- fread("../outputs/data_exit_regressions_1100_threshold_125.csv")
ex_df125$rel_density <- scale(ex_df125$density)
ex_df125$pci <- scale(ex_df125$pci)
ex_df125$ubiquity <- scale(ex_df125$ubiquity)

ex_m1 <- feols(exit01 ~ density, cluster = "iso2_code", data = ex_df)
ex_m6 <- feols(exit01 ~ density + ubiquity, cluster = "iso2_code", data = ex_df)
ex_m7 <- feols(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = ex_df)

ex_m1_075 <- feols(exit01 ~ density, cluster = "iso2_code", data = ex_df075)
ex_m6_075 <- feols(exit01 ~ density + ubiquity, cluster = "iso2_code", data = ex_df075)
ex_m7_075 <- feols(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = ex_df075)

ex_m1_125 <- feols(exit01 ~ density, cluster = "iso2_code", data = ex_df125)
ex_m6_125 <- feols(exit01 ~ density + ubiquity, cluster = "iso2_code", data = ex_df125)
ex_m7_125 <- feols(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = ex_df125)


etable(
  ex_m1, ex_m6, ex_m7, ex_m1_075, ex_m6_075, ex_m7_075, ex_m1_125, ex_m6_125, ex_m7_125,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- SI growth regressions


# dataframe from 01_data_prep.ipynb
#df <- fread("../outputs/eci_regression_table.csv")
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)

# ECI table using clusters of languages
eci_clusters <- fread("../outputs/eci_clusters_cooc_2020_2023.csv") %>%
  dplyr::select(iso2_code, year, eci) %>%
  unique() %>%
  rename(eci_clusters = eci) %>%
  group_by(year) %>%
  mutate(eci_clusters_norm = scale(eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  eci_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)

iv_clusters <- fread("../outputs/si_eci_clusters_cooc_2020_2023_ivreg.csv") %>%
  dplyr::select(iso2_code, year, avg_eci_similar_spec) %>%
  unique() %>%
  rename(sim_eci_clusters = avg_eci_similar_spec) %>%
  group_by(year) %>%
  mutate(sim_eci_clusters_norm = scale(sim_eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  iv_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)


# calculate growth for cross sectional growth models
t1 <- 2020
t2 <- 2023

# GDP in USD -- 2020-2023
# PPP version does not work AT ALL -- PPP data only available until 2022
gdp_t1 <- subset(df, year == t1, select = c(iso3_code, gdp_current_USD))
gdp_t2 <- subset(df, year == t2, select = c(iso3_code, gdp_current_USD))
colnames(gdp_t1)[2] <- "gdp_t1"
colnames(gdp_t2)[2] <- "gdp_t2"

# merge the GDP data for 2020 and 2022 by iso3_code
gdp_growth_data <- merge(
  gdp_t1,
  gdp_t2,
  by = "iso3_code",
  all = TRUE
)

# calculate GDP growth (2020 to 2022) as a percentage
gdp_growth_data$gdp_growth_t12 <- with(
  gdp_growth_data, 
  ifelse(!is.na(gdp_t1) & !is.na(gdp_t2), (gdp_t2 / gdp_t1), NA)
)

# merge the calculated growth back into the original data
df <- merge(
  df,
  unique(gdp_growth_data[, c("iso3_code", "gdp_growth_t12")]), 
  by = "iso3_code",
  all.x = TRUE,
  all.y = FALSE
)


# manipulation
df <- df %>%
  group_by(year) %>%
  mutate(
    log_gdp = log10(gdp_current_USD),
    log_gdp_growth_t12 = log10(gdp_growth_t12)
    #eci_software_norm = scale(eci_software),
    #eci_trade_norm = scale(eci_trade),
    #eci_tech_norm = scale(eci_tech),
    #eci_research_norm = scale(eci_research),
    #log_pop = log10(population),
    #log_nat_res = log10(natural_resources)
  ) %>%
  data.table()


### GDP growth vs ECI -- cross sectional growth from 2020-2023
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_growth_t12", "log_gdp", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

grm00 <- feols(log_gdp_growth_t12 ~ log_gdp, vcov = "HC1", data = reg_df)
grm01 <- feols(log_gdp_growth_t12 ~ eci_clusters_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm02 <- feols(log_gdp_growth_t12 ~ eci_trade_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm03 <- feols(log_gdp_growth_t12 ~ eci_tech_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm04 <- feols(log_gdp_growth_t12 ~ eci_research_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm05 <- feols(log_gdp_growth_t12 ~ eci_clusters_norm + eci_trade_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm06 <- feols(log_gdp_growth_t12 ~ eci_clusters_norm + eci_tech_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm07 <- feols(log_gdp_growth_t12 ~ eci_clusters_norm + eci_research_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
grm08 <- feols(log_gdp_growth_t12 ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)


etable(
  grm00, grm01, grm02, grm03, grm04, grm05, grm06, grm07, grm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






# --- SI Gini and emission regressions w/ GDP + GDP**2


# baseline dataframe from 01_data_prep_complexity.ipynb
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)



# Gini vs ECI software -- ** 2
reg_df <- subset(df, year==2020)
key_columns <- c("gini_2020_2022_norm", "log_gdp_ppp_pc", "log_gdp_ppp_pc2", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m02 <- feols(gini_2020_2022_norm ~ eci_trade_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m03 <- feols(gini_2020_2022_norm ~ eci_tech_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m04 <- feols(gini_2020_2022_norm ~ eci_research_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m05 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m06 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m07 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable_gini <- etable(
  gini_m01, gini_m02, gini_m03, gini_m04, gini_m05, gini_m06, gini_m07, gini_m08,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)
print(etable_gini)
save_etable_to_word(etable_gini)


# Emission vs ECI software -- ** 2
reg_df <- subset(df, year==2020)
key_columns <- c("log_emission_per_gdp", "log_gdp_ppp_pc", "log_gdp_ppp_pc2", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(log_emission_per_gdp ~ eci_software_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m02 <- feols(log_emission_per_gdp ~ eci_trade_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m03 <- feols(log_emission_per_gdp ~ eci_tech_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m04 <- feols(log_emission_per_gdp ~ eci_research_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m05 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m06 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_tech_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m07 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_research_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_gdp_ppp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

emission_etable <- etable(
  em_m01, em_m02, em_m03, em_m04, em_m05, em_m06, em_m07, em_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)
print(emission_etable)
save_etable_to_word(emission_etable)







# --- SI Identical samples regressions

# baseline dataframe from 01_data_prep_complexity.ipynb
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)

# ECI table using clusters of languages
eci_clusters <- fread("../outputs/eci_clusters_cooc_2020_2023.csv") %>%
  dplyr::select(iso2_code, year, eci) %>%
  unique() %>%
  rename(eci_clusters = eci) %>%
  group_by(year) %>%
  mutate(eci_clusters_norm = scale(eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  eci_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)

iv_clusters <- fread("../outputs/si_eci_clusters_cooc_2020_2023_ivreg.csv") %>%
  dplyr::select(iso2_code, year, avg_eci_similar_spec) %>%
  unique() %>%
  rename(sim_eci_clusters = avg_eci_similar_spec) %>%
  group_by(year) %>%
  mutate(sim_eci_clusters_norm = scale(sim_eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  iv_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)



reg_df <- subset(df, year==2020)
#key_columns <- c("log_gdp_ppp_pc", "gini_norm", "log_emission_per_gdp",  "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
key_columns <- c("log_gdp_ppp_pc", "gini_2020_2022_norm", "log_emission_per_gdp",  "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_gdp_ppp_pc", "ln_pop", "ln_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m01 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_2020_2022_norm ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)
em_m01 <- feols(log_emission_per_gdp ~ eci_clusters_norm + ln_gdp_ppp_pc + ln_pop+ ln_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission_per_gdp ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, vcov = "HC1", data = reg_df)



identical_sample_etable <- etable(
  gdp_m01, gdp_m08, gini_m01, gini_m08, em_m01, em_m08,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)
print(identical_sample_etable)
save_etable_to_word(identical_sample_etable)






# --- SI No mathematical dependencies tables

# baseline table
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)


# GDP per capita
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_ppp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_nat_res", "log_pop")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp_ppp ~ eci_software_norm + log_nat_res + log_pop, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_ppp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_nat_res + log_pop, vcov = "HC1", data = reg_df)

# base reg version for VIF
gdp_bm01 <- lm(log_gdp_ppp ~ eci_software_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm08 <- lm(log_gdp_ppp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)


### Gini
reg_df <- subset(df, year==2020)
key_columns <- c("gini_2020_2022_norm", "log_gdp_ppp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

# base reg version for VIF
gini_bm01 <- lm(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp + log_pop + log_nat_res, data = reg_df)
gini_bm08 <- lm(gini_2020_2022_norm ~ eci_software_norm + log_gdp_ppp + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)



# Emissions
reg_df <- subset(df, year==2020)
key_columns <- c("log_emission", "log_gdp_ppp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(log_emission ~ eci_software_norm + log_gdp_ppp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

# base reg version for VIF
em_bm01 <- lm(log_emission ~ eci_software_norm + log_gdp_ppp + log_pop + log_nat_res, data = reg_df)
em_bm08 <- lm(log_emission ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp + log_pop + log_nat_res, data = reg_df)


no_math_dep_etable <- etable(
  gdp_m01, gdp_m08, gini_m01, gini_m08, em_m01, em_m08,
  digits = 5,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)
print(no_math_dep_etable)
save_etable_to_word(no_math_dep_etable)


# VIF values for no mathematical dependencies versions 
car::vif(gdp_bm01)
car::vif(gdp_bm08)
car::vif(gini_bm01)
car::vif(gini_bm08)
car::vif(em_bm01)
car::vif(em_bm08)










# --- SI VIF tables for main regressions

# baseline dataframe from 01_data_prep_complexity.ipynb
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)

# GDP per capita models 2020
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_bm01 <- lm(log_gdp_ppp_pc ~ eci_software_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm02 <- lm(log_gdp_ppp_pc ~ eci_trade_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm03 <- lm(log_gdp_ppp_pc ~ eci_tech_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm04 <- lm(log_gdp_ppp_pc ~ eci_research_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm05 <- lm(log_gdp_ppp_pc ~ eci_software_norm + eci_trade_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm06 <- lm(log_gdp_ppp_pc ~ eci_software_norm + eci_tech_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm07 <- lm(log_gdp_ppp_pc ~ eci_software_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm08 <- lm(log_gdp_ppp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)

car::vif(gdp_bm01)
car::vif(gdp_bm02)
car::vif(gdp_bm03)
car::vif(gdp_bm04)
car::vif(gdp_bm05)
car::vif(gdp_bm06)
car::vif(gdp_bm07)
car::vif(gdp_bm08)


# Gini models for 2020
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("gini_2020_2022_norm", "log_gdp_ppp_pc", "log_gdp_ppp_pc2", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

# change to lm()
gini_bm01 <- lm(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
gini_bm02 <- lm(gini_norm ~ eci_trade_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
gini_bm03 <- lm(gini_norm ~ eci_tech_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
gini_bm04 <- lm(gini_norm ~ eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
gini_bm05 <- lm(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + log_pop + log_nat_res, data = reg_df)
gini_bm06 <- lm(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_tech_norm + log_pop + log_nat_res, data = reg_df)
gini_bm07 <- lm(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_research_norm + log_pop + log_nat_res, data = reg_df)
gini_bm08 <- lm(gini_norm ~ eci_software_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)

car::vif(gini_bm01)
car::vif(gini_bm02)
car::vif(gini_bm03)
car::vif(gini_bm04)
car::vif(gini_bm05)
car::vif(gini_bm06)
car::vif(gini_bm07)
car::vif(gini_bm08)


### emissions vs ECI
reg_df <- subset(df, year==2020)
key_columns <- c("log_emission_per_gdp", "log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


em_bm01 <- lm(log_emission_per_gdp ~ eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm02 <- lm(log_emission_per_gdp ~ eci_trade_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm03 <- lm(log_emission_per_gdp ~ eci_tech_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm04 <- lm(log_emission_per_gdp ~ eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm05 <- lm(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm06 <- lm(log_emission_per_gdp ~ eci_software_norm + eci_tech_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm07 <- lm(log_emission_per_gdp ~ eci_software_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)
em_bm08 <- lm(log_emission_per_gdp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, data = reg_df)


car::vif(em_bm01)
car::vif(em_bm02)
car::vif(em_bm03)
car::vif(em_bm04)
car::vif(em_bm05)
car::vif(em_bm06)
car::vif(em_bm07)
car::vif(em_bm08)






# --- SI stepwise IV regressions

# baseline dataframe from 01_data_prep_complexity.ipynb
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)


# GDP per capita -- first stage
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

summary(fs_gdp <- feols(
  eci_software_norm ~ sim_eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
)
reg_df$first_stage_pred <- predict(fs_gdp, newdata = reg_df)
#print(cor(reg_df$eci_software_norm, reg_df$first_stage_pred))
print(cor(reg_df$eci_software_norm, reg_df$sim_eci_software_norm))


# Gini -- first stage
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("gini_norm", "log_gdp_ppp_pc", "log_gdp_ppp_pc2", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

summary(fs_gini <- feols(
  eci_software_norm ~ sim_eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
)
reg_df$first_stage_pred <- predict(fs_gini, newdata = reg_df)
print(cor(reg_df$eci_software_norm, reg_df$first_stage_pred))
print(cor(reg_df$eci_software_norm, reg_df$sim_eci_software_norm))

# Emission -- first stage
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_emission_per_gdp", "log_gdp_ppp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

summary(fs_em <- feols(
  eci_software_norm ~ sim_eci_software_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
)
reg_df$first_stage_pred <- predict(fs_em, newdata = reg_df)
print(cor(reg_df$eci_software_norm, reg_df$first_stage_pred))
print(cor(reg_df$eci_software_norm, reg_df$sim_eci_software_norm))

etable(
  fs_gdp, fs_gini, fs_em,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)



# --- SI language clusters

# baseline table

# baseline dataframe from 01_data_prep_complexity.ipynb
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)

# ECI table using clusters of languages
eci_clusters <- fread("../outputs/eci_clusters_cooc_2020_2023.csv") %>%
  dplyr::select(iso2_code, year, eci) %>%
  unique() %>%
  rename(eci_clusters = eci) %>%
  group_by(year) %>%
  mutate(eci_clusters_norm = scale(eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  eci_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)


# GDP regressions
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_ppp_pc", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

# --- SI language clusters
gdp_m01 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_ppp_pc ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

# Gini regressions
reg_df <- subset(df, year==2020)
key_columns <- c("gini_norm", "log_gdp_ppp_pc", "log_gdp_ppp_pc2", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_norm ~ eci_clusters_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ eci_clusters_norm + log_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

# emission regressions
reg_df <- subset(df, year==2020)
key_columns <- c("log_emission_per_gdp", "log_gdp_ppp_pc", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(log_emission_per_gdp ~ eci_clusters_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(log_emission_per_gdp ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp_ppp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

cluster_etable <- etable(
  gdp_m01, gdp_m08, gini_m01, gini_m08, em_m01, em_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)
print(cluster_etable)
save_etable_to_word(cluster_etable)







### TOBIT models -- test for revision


# baseline dataframe from 01_data_prep_complexity.ipynb
df <- create_baseline_table(
  main_input_path = "../outputs/eci_regression_table.csv",
  iv_input_path = "../outputs/si_eci_software_2020_2023_ivreg.csv"
)

# ECI table using clusters of languages
eci_clusters <- fread("../outputs/eci_clusters_cooc_2020_2023.csv") %>%
  dplyr::select(iso2_code, year, eci) %>%
  unique() %>%
  rename(eci_clusters = eci) %>%
  group_by(year) %>%
  mutate(eci_clusters_norm = scale(eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  eci_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)

iv_clusters <- fread("../outputs/si_eci_clusters_cooc_2020_2023_ivreg.csv") %>%
  dplyr::select(iso2_code, year, avg_eci_similar_spec) %>%
  unique() %>%
  rename(sim_eci_clusters = avg_eci_similar_spec) %>%
  group_by(year) %>%
  mutate(sim_eci_clusters_norm = scale(sim_eci_clusters)) %>%
  data.table()

df <- merge(
  df,
  iv_clusters,
  by = c("iso2_code", "year"),
  all.x = TRUE,
  all.y = FALSE
)


# Tobit -- GDP per capita vs ECI software
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_ppp_pc", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res", "sim_eci_clusters_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

summary(gdp_tobit01 <- tobit(log_gdp_ppp_pc ~ eci_clusters_norm + ln_pop + ln_nat_res, left = 0, data = reg_df))
summary(gdp_tobit08 <- tobit(log_gdp_ppp_pc ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, left = 0, data = reg_df))
stargazer(gdp_tobit01, gdp_tobit08, type = "text")


# Tobit -- Gini vs ECI software
#df$gini_pc <- df$gini_mean / 100
df$gini_pc <- df$gini_mean_2020_2022 / 100
df$logit_gini <- log10(df$gini_pc / (1 - df$gini_pc))
df$logit_gini <- as.numeric(df$logit_gini)
reg_df <- subset(df, year==2020)
key_columns <- c("logit_gini", "ln_gdp_ppp_pc", "ln_gdp_ppp_pc2", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

summary(gini_tobit01 <- tobit(logit_gini ~ eci_clusters_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, left = 0, data = reg_df))
summary(gini_tobit08 <- tobit(logit_gini ~ eci_clusters_norm + ln_gdp_ppp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_pop + ln_nat_res, left = 0, data = reg_df))
stargazer(gini_tobit01, gini_tobit08, type = "text")
vcovHC(gini_tobit01, type = "HC1")

# Tobit -- Emission vs ECI software
reg_df <- subset(df, year==2020)
key_columns <- c("log_emission_per_gdp", "ln_gdp_ppp_pc", "eci_clusters_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "ln_pop", "ln_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

summary(em_tobit01 <- tobit(log_emission ~ eci_clusters_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, left = -Inf, data = reg_df))
summary(em_tobit08 <- tobit(log_emission ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + ln_gdp_ppp_pc + ln_pop + ln_nat_res, left = -Inf, data = reg_df))
stargazer(em_tobit01, em_tobit08, type = "text")

stargazer(gdp_tobit01, gdp_tobit08, gini_tobit01, gini_tobit08, em_tobit01, em_tobit08)


