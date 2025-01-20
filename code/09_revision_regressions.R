### ECI software / trade / technology / research regressions
### by sandorjuhasz


library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)
library(car)



# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/eci_regression_table.csv")



# calculate growth for cross sectional growth models
t1 <- 2020
t2 <- 2023

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
  #ifelse(!is.na(gdp_t1) & !is.na(gdp_t2), (gdp_t2 - gdp_t1 / gdp_t1), NA)
)

# merge the calculated growth back into the original data
df <- merge(
  df,
  unique(gdp_growth_data[, c("iso3_code", "gdp_growth_t12")]), 
  by = "iso3_code",
  all.x = TRUE,
  all.y = FALSE
)


# IVs
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
    log_gdp = log10(gdp_current_USD),
    log_gdp2 = (log_gdp ** 2),
    log_gdp_growth_t12 = log10(gdp_growth_t12),
    log_gdp_pc = log10(gdp_per_capita),
    log_gdp_pc2 = (log_gdp_pc ** 2),
    gini_norm = scale(gini_mean),
    #emission_norm = scale(embodied_emissions),
    emission_norm = scale(emissions),
    human_cap_norm = scale(human_capital_index),
    eci_software_norm = scale(eci_software),
    eci_trade_norm = scale(eci_trade),
    eci_tech_norm = scale(eci_tech),
    eci_research_norm = scale(eci_research),
    log_pop = log10(population),
    log_nat_res = log10(natural_resources)
  ) %>%
  data.table()






### Table 1 -- GPD vs ECI -- for replication on new World Bank Data
reg_df <- subset(df, year==2020)
reg_df$sim_eci_software_norm <- scale(reg_df$avg_eci_similar_spec)
key_columns <- c("log_gdp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res", "sim_eci_software_norm")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


gdp_m01 <- feols(log_gdp_pc ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m02 <- feols(log_gdp_pc ~ eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m03 <- feols(log_gdp_pc ~ eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m04 <- feols(log_gdp_pc ~ eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m05 <- feols(log_gdp_pc ~ eci_software_norm + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m06 <- feols(log_gdp_pc ~ eci_software_norm + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m07 <- feols(log_gdp_pc ~ eci_software_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm01 <- feols(log_gdp_pc ~ 1 | eci_software_norm + log_pop + log_nat_res ~ sim_eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm08 <- feols(log_gdp_pc ~ 1 | eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res ~ sim_eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_m01, gdp_m02, gdp_m03, gdp_m04, gdp_m05, gdp_m06, gdp_m07, gdp_m08, gdp_ivm01, gdp_ivm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)



### GDP growth vs ECI -- cross sectional growth from 2020-2023
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_growth_t12", "log_gdp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


#grm01 <- feols(log_gdp_growth ~ eci_software + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm02 <- feols(log_gdp_growth ~ eci_trade_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm03 <- feols(log_gdp_growth ~ eci_tech_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm04 <- feols(log_gdp_growth ~ eci_research_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm05 <- feols(log_gdp_growth ~ eci_software_norm + eci_trade_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm06 <- feols(log_gdp_growth ~ eci_software_norm + eci_tech_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm07 <- feols(log_gdp_growth ~ eci_software_norm + eci_research_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#grm08 <- feols(log_gdp_growth ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

grm00 <- feols(log_gdp_growth_t12 ~ log_gdp, vcov = "HC1", data = reg_df)
grm01 <- feols(log_gdp_growth_t12 ~ eci_software_norm + log_gdp, vcov = "HC1", data = reg_df)
grm02 <- feols(log_gdp_growth_t12 ~ eci_trade_norm + log_gdp, vcov = "HC1", data = reg_df)
grm03 <- feols(log_gdp_growth_t12 ~ eci_tech_norm + log_gdp, vcov = "HC1", data = reg_df)
grm04 <- feols(log_gdp_growth_t12 ~ eci_research_norm + log_gdp, vcov = "HC1", data = reg_df)
grm05 <- feols(log_gdp_growth_t12 ~ eci_software_norm + eci_trade_norm + log_gdp, vcov = "HC1", data = reg_df)
grm06 <- feols(log_gdp_growth_t12 ~ eci_software_norm + eci_tech_norm + log_gdp, vcov = "HC1", data = reg_df)
grm07 <- feols(log_gdp_growth_t12 ~ eci_software_norm + eci_research_norm + log_gdp, vcov = "HC1", data = reg_df)
grm08 <- feols(log_gdp_growth_t12 ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_gdp, vcov = "HC1", data = reg_df)


etable(
  grm00, grm01, grm02, grm03, grm04, grm05, grm06, grm07, grm08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)











### Gini vs ECI -- cross sectional 2020
reg_df <- subset(df, year==2020)
key_columns <- c("gini_norm", "log_gdp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


gini_m01 <- feols(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m02 <- feols(gini_norm ~ eci_trade_norm + log_gdp + log_gdp2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m03 <- feols(gini_norm ~ eci_tech_norm + log_gdp + log_gdp2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m04 <- feols(gini_norm ~ eci_research_norm + log_gdp + log_gdp2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m05 <- feols(gini_norm ~ eci_software_norm + log_gdp + log_gdp2 + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m06 <- feols(gini_norm ~ eci_software_norm + log_gdp + log_gdp2 + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m07 <- feols(gini_norm ~ eci_software_norm + log_gdp + log_gdp2 + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ eci_software_norm + log_gdp + log_gdp2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gini_m01, gini_m02, gini_m03, gini_m04, gini_m05, gini_m06, gini_m07, gini_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)




### emissions vs ECI
# drop rows w/ NAs in key columns
reg_df <- subset(df, year==2020)
key_columns <- c("emission_norm", "log_gdp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


em_m01 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + log_nat_res, vcov = "HC1", data = reg_df)
em_m02 <- feols(emission_norm ~ eci_trade_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m03 <- feols(emission_norm ~ eci_tech_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m04 <- feols(emission_norm ~ eci_research_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m05 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m06 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m07 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  em_m01, em_m02, em_m03, em_m04, em_m05, em_m06, em_m07, em_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)




### emissions vs ECI -- example left here for stargazer output
# drop rows w/ NAs in key columns
key_columns <- c("emission_norm", "log_gdp", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res")
reg_df <- df[complete.cases(df[, ..key_columns]), ]

# models
summary(m1 <- lm(emission_norm ~ software_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m2 <- lm(emission_norm ~ trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m3 <- lm(emission_norm ~ tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m4 <- lm(emission_norm ~ research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m5a <- lm(emission_norm ~ software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m5b <- lm(emission_norm ~ software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m5c <- lm(emission_norm ~ software_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m6 <- lm(emission_norm ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))

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
  covariate.labels = c("ECI software", "ECI trade", "ECI technology", "ECI research", "GPD per capita (log)", "Population (log)", "Natural resources (log)"),
  out = "../outputs/table3_emission_eci_regressions.html"
)





### RCA thresholds
cdf075 <- fread("../outputs/eci_software_2020_2023_threshold_075.csv") %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(iso2_code, year, eci) %>%
  mutate(eci_software075_norm = scale(eci)) %>%
  dplyr::select(-eci) %>%
  unique()

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



# GDP pc regressions
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_pc", "eci_software_norm", "eci_software075_norm", "eci_software125_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp_pc ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m01_075 <- feols(log_gdp_pc ~ eci_software075_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08_075 <- feols(log_gdp_pc ~ eci_software075_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m01_125 <- feols(log_gdp_pc ~ eci_software125_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08_125 <- feols(log_gdp_pc ~ eci_software125_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable(
  gdp_m01, gdp_m08, gdp_m01_075, gdp_m08_075, gdp_m01_125, gdp_m08_125, 
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)



# Gini regressions
reg_df <- subset(df, year==2020)
key_columns <- c("gini_norm", "log_gdp_pc", "log_gdp_pc2", "eci_software_norm", "eci_software075_norm", "eci_software125_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


gini_m01 <- feols(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m01_075 <- feols(gini_norm ~ eci_software075_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08_075 <- feols(gini_norm ~ eci_software075_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m01_125 <- feols(gini_norm ~ eci_software125_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08_125 <- feols(gini_norm ~ eci_software125_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gini_m01, gini_m08, gini_m01_075, gini_m08_075, gini_m01_125, gini_m08_125,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)



# Emissions regressions
reg_df <- subset(df, year==2020)
key_columns <- c("emission_norm", "eci_software_norm", "eci_software075_norm", "eci_software125_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(emission_norm ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(emission_norm ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m01_075 <- feols(emission_norm ~ eci_software075_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08_075 <- feols(emission_norm ~ eci_software075_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m01_125 <- feols(emission_norm ~ eci_software125_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08_125 <- feols(emission_norm ~ eci_software125_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)

etable(
  em_m01, em_m08, em_m01_075, em_m08_075, em_m01_125, em_m08_125, 
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
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
  tex = TRUE
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
  tex = TRUE
)






### ECI based on clusters
df <- fread("../outputs/eci_regression_table.csv")
df <- df %>%
  group_by(year) %>%
  mutate(
    log_gdp = log10(gdp_current_USD),
    log_gdp2 = (log_gdp ** 2),
    log_gdp_pc = log10(gdp_per_capita),
    log_gdp_pc2 = (log_gdp_pc ** 2),
    gini_norm = scale(gini_mean),
    #emission_norm = scale(embodied_emissions),
    emission_norm = scale(emissions),
    human_cap_norm = scale(human_capital_index),
    eci_software_norm = scale(eci_software),
    eci_trade_norm = scale(eci_trade),
    eci_tech_norm = scale(eci_tech),
    eci_research_norm = scale(eci_research),
    log_pop = log10(population),
    log_nat_res = log10(natural_resources)
  ) %>%
  data.table()


# ECI clusters
eci_clusters <- fread("../outputs/eci_clusters_2020_2023.csv") %>%
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


### Table 1 -- GPD vs ECI -- for replication on new World Bank Data
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_pc", "gini_norm", "emission_norm", "eci_clusters_norm", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


gdp_m01_clusters <- feols(log_gdp_pc ~ eci_clusters_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08_clusters <- feols(log_gdp_pc ~ eci_clusters_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m01_clusters <- feols(gini_norm ~ eci_clusters_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08_clusters <- feols(gini_norm ~ eci_clusters_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m01_clusters <- feols(emission_norm ~ eci_clusters_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08_clusters <- feols(emission_norm ~ eci_clusters_norm + log_gdp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_m01_clusters, gdp_m08_clusters, gini_m01_clusters, gini_m08_clusters, em_m01_clusters, em_m08_clusters,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






# ENTRY regressions
en_df <- fread("../outputs/data_entry_regressions_0011_clusters.csv")
en_df$rel_density <- scale(en_df$density)
en_df$pci <- scale(en_df$pci)
en_df$ubiquity <- scale(en_df$ubiquity)

ex_df <- fread("../outputs/data_exit_regressions_1100_clusters.csv")
ex_df$rel_density <- scale(ex_df$density)
ex_df$pci <- scale(ex_df$pci)
ex_df$ubiquity <- scale(ex_df$ubiquity)


ent_m1 <- feols(entry01 ~ density, cluster = "iso2_code", data = en_df)
ent_m6 <- feols(entry01 ~ density + ubiquity, cluster = "iso2_code", data = en_df)
ent_m7 <- feols(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = en_df)

ex_m1 <- feols(exit01 ~ density, cluster = "iso2_code", data = ex_df)
ex_m6 <- feols(exit01 ~ density + ubiquity, cluster = "iso2_code", data = ex_df)
ex_m7 <- feols(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = ex_df)


etable(
  ent_m1, ent_m6, ent_m7, ex_m1, ex_m6, ex_m7,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)








### mathematical dependencies models

# GDP per capita
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_pc", "log_gdp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gdp_m01 <- feols(log_gdp ~ eci_software_norm + log_nat_res + log_pop, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_nat_res + log_pop, vcov = "HC1", data = reg_df)


### Gini
reg_df <- subset(df, year==2020)
key_columns <- c("gini_norm", "log_gdp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

gini_m01 <- feols(gini_norm ~ eci_software_norm + log_gdp + log_gdp2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ eci_software_norm + log_gdp + log_gdp2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


# Emissions
reg_df <- subset(df, year==2020)
key_columns <- c("emission_norm", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

em_m01 <- feols(emission_norm ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(emission_norm ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_m01, gdp_m08, gini_m01, gini_m08, em_m01, em_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






### VIF values for main models

# GDP per capita models 2020
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


gdp_bm01 <- lm(log_gdp_pc ~ eci_software_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm02 <- lm(log_gdp_pc ~ eci_trade_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm03 <- lm(log_gdp_pc ~ eci_tech_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm04 <- lm(log_gdp_pc ~ eci_research_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm05 <- lm(log_gdp_pc ~ eci_software_norm + eci_trade_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm06 <- lm(log_gdp_pc ~ eci_software_norm + eci_tech_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm07 <- lm(log_gdp_pc ~ eci_software_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)
gdp_bm08 <- lm(log_gdp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)

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
key_columns <- c("gini_norm", "log_gdp", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]

# change to lm()
gini_bm01 <- lm(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, data = reg_df)
gini_bm02 <- lm(gini_norm ~ eci_trade_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, data = reg_df)
gini_bm03 <- lm(gini_norm ~ eci_tech_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, data = reg_df)
gini_bm04 <- lm(gini_norm ~ eci_research_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, data = reg_df)
gini_bm05 <- lm(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + log_pop + log_nat_res, data = reg_df)
gini_bm06 <- lm(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + eci_tech_norm + log_pop + log_nat_res, data = reg_df)
gini_bm07 <- lm(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + eci_research_norm + log_pop + log_nat_res, data = reg_df)
gini_bm08 <- lm(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)

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
key_columns <- c("emission_norm", "log_gdp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


em_bm01 <- lm(emission_norm ~ eci_software_norm + log_gdp_pc + log_pop + log_nat_res, data = reg_df)
em_bm02 <- lm(emission_norm ~ eci_trade_norm + log_gdp_pc + log_pop + log_nat_res, data = reg_df)
em_bm03 <- lm(emission_norm ~ eci_tech_norm + log_gdp_pc + log_pop + log_nat_res, data = reg_df)
em_bm04 <- lm(emission_norm ~ eci_research_norm + log_gdp_pc + log_pop + log_nat_res, data = reg_df)
em_bm05 <- lm(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + log_pop + log_nat_res, data = reg_df)
em_bm06 <- lm(emission_norm ~ eci_software_norm + log_gdp_pc + eci_tech_norm + log_pop + log_nat_res, data = reg_df)
em_bm07 <- lm(emission_norm ~ eci_software_norm + log_gdp_pc + eci_research_norm + log_pop + log_nat_res, data = reg_df)
em_bm08 <- lm(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, data = reg_df)

car::vif(em_bm01)
car::vif(em_bm02)
car::vif(em_bm03)
car::vif(em_bm04)
car::vif(em_bm05)
car::vif(em_bm06)
car::vif(em_bm07)
car::vif(em_bm08)





### same sample -- GDP/Gini/emissions all available for 2020
reg_df <- subset(df, year==2020)
key_columns <- c("log_gdp_pc", "gini_norm", "emission_norm",  "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


gdp_m01 <- feols(log_gdp_pc ~ eci_software_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gdp_pc ~ eci_software_norm + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m01 <- feols(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ eci_software_norm + log_gdp_pc + log_gdp_pc2 + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m01 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + log_pop+ log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)



etable(
  gdp_m01, gdp_m08, gini_m01, gini_m08, em_m01, em_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)



### IV extensions
# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/eci_comparisons_2020.csv")
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
df$log_gpd_pc <- log10(df$gdpcap_o)
df$log_gpd_pc2 <- df$log_gpd_pc ** 2
df$gini_norm <- scale(df$gini_mean)
#df$emission_norm <- scale(df$embodied_emissions)
df$emission_norm <- scale(df$emissions)
df$software_eci_norm <- scale(df$software_eci_2020)
df$trade_eci_norm <- scale(df$trade_eci_2020)
df$tech_eci_norm <- scale(df$tech_eci_2020)
df$research_eci_norm <- scale(df$research_eci_2020)
df$log_pop <- log10(df$pop_o)
df$log_nat_res <- log10(df$nat_res)
df$sim_software_eci_norm <- scale(df$avg_eci_similar_spec)


### GPD vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res", "sim_software_eci_norm")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


gdp_ivm01 <- feols(log_gpd_pc ~ 1 | software_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm02 <- feols(log_gpd_pc ~ trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm03 <- feols(log_gpd_pc ~ tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm04 <- feols(log_gpd_pc ~ research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm05 <- feols(log_gpd_pc ~ 1 | software_eci_norm + trade_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm06 <- feols(log_gpd_pc ~ 1 | software_eci_norm + tech_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm07 <- feols(log_gpd_pc ~ 1 | software_eci_norm + research_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm08 <- feols(log_gpd_pc ~ 1 | software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)



















### emissions vs ECI
# drop rows w/ NAs in key columns
reg_df <- subset(df, year==2020)
key_columns <- c("emission_norm", "log_gdp_pc", "eci_software_norm", "eci_trade_norm", "eci_tech_norm", "eci_research_norm", "log_pop", "log_nat_res")
reg_df <- reg_df[complete.cases(reg_df[, ..key_columns]), ]


em_m01 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + log_nat_res, vcov = "HC1", data = reg_df)
em_m02 <- feols(emission_norm ~ eci_trade_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m03 <- feols(emission_norm ~ eci_tech_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m04 <- feols(emission_norm ~ eci_research_norm + log_gdp_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m05 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m06 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_tech_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m07 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(emission_norm ~ eci_software_norm + log_gdp_pc + eci_trade_norm + eci_tech_norm + eci_research_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  em_m01, em_m02, em_m03, em_m04, em_m05, em_m06, em_m07, em_m08,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)




### emissions vs ECI -- example left here for stargazer output
# drop rows w/ NAs in key columns
key_columns <- c("emission_norm", "log_gdp", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res")
reg_df <- df[complete.cases(df[, ..key_columns]), ]

# models
summary(m1 <- lm(emission_norm ~ software_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m2 <- lm(emission_norm ~ trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m3 <- lm(emission_norm ~ tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m4 <- lm(emission_norm ~ research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m5a <- lm(emission_norm ~ software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m5b <- lm(emission_norm ~ software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m5c <- lm(emission_norm ~ software_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))
summary(m6 <- lm(emission_norm ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df))

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
  covariate.labels = c("ECI software", "ECI trade", "ECI technology", "ECI research", "GPD per capita (log)", "Population (log)", "Natural resources (log)"),
  out = "../outputs/table3_emission_eci_regressions.html"
)


















### IV approach for SI

# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/eci_comparisons_2020.csv")
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
df$log_gpd_pc <- log10(df$gdpcap_o)
df$log_gpd_pc2 <- df$log_gpd_pc ** 2
df$gini_norm <- scale(df$gini_mean)
#df$emission_norm <- scale(df$embodied_emissions)
df$emission_norm <- scale(df$emissions)
df$software_eci_norm <- scale(df$software_eci_2020)
df$trade_eci_norm <- scale(df$trade_eci_2020)
df$tech_eci_norm <- scale(df$tech_eci_2020)
df$research_eci_norm <- scale(df$research_eci_2020)
df$log_pop <- log10(df$pop_o)
df$log_nat_res <- log10(df$nat_res)
df$sim_software_eci_norm <- scale(df$avg_eci_similar_spec)


### GPD vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res", "sim_software_eci_norm")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


gdp_ivm01 <- feols(log_gpd_pc ~ 1 | software_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm02 <- feols(log_gpd_pc ~ trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm03 <- feols(log_gpd_pc ~ tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm04 <- feols(log_gpd_pc ~ research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm05 <- feols(log_gpd_pc ~ 1 | software_eci_norm + trade_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm06 <- feols(log_gpd_pc ~ 1 | software_eci_norm + tech_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm07 <- feols(log_gpd_pc ~ 1 | software_eci_norm + research_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_ivm08 <- feols(log_gpd_pc ~ 1 | software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_ivm01, gdp_ivm02, gdp_ivm03, gdp_ivm04, gdp_ivm05, gdp_ivm06, gdp_ivm07, gdp_ivm08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)


### Gini vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("gini_norm", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res", "sim_software_eci_norm")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


gini_ivm01 <- feols(gini_norm ~ 1 | software_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res ~ sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm02 <- feols(gini_norm ~ trade_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm03 <- feols(gini_norm ~ tech_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm04 <- feols(gini_norm ~ research_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm05 <- feols(gini_norm ~ 1 | software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm06 <- feols(gini_norm ~ 1 | software_eci_norm + log_gpd_pc + log_gpd_pc2 + tech_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm07 <- feols(gini_norm ~ 1 | software_eci_norm + log_gpd_pc + log_gpd_pc2 + research_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_ivm08 <- feols(gini_norm ~ 1 | software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res ~ sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)



etable(
  gini_ivm01, gini_ivm02, gini_ivm03, gini_ivm04, gini_ivm05, gini_ivm06, gini_ivm07, gini_ivm08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)



### emissions vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("emission_norm", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res", "sim_software_eci_norm")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


em_ivm01 <- feols(emission_norm ~ 1 | software_eci_norm + log_gpd_pc + log_pop + log_nat_res ~ sim_software_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm02 <- feols(emission_norm ~ trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm03 <- feols(emission_norm ~ tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm04 <- feols(emission_norm ~ research_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm05 <- feols(emission_norm ~ 1 | software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop + log_nat_res ~ sim_software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm06 <- feols(emission_norm ~ 1 | software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop + log_nat_res ~ sim_software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm07 <- feols(emission_norm ~ 1 | software_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res ~ sim_software_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_ivm08 <- feols(emission_norm ~ 1 | software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc +log_pop + log_nat_res ~ sim_software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  em_ivm01, em_ivm02, em_ivm03, em_ivm04, em_ivm05, em_ivm06, em_ivm07, em_ivm08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)


# Function to extract diagnostics including the First-Stage F and Wu-Hausman Stat

library(AER)
library(ivreg)

extract_diagnostics <- function(models) {
  diagnostics_list <- lapply(models, function(model) summary(model, diagnostics = TRUE)$diagnostics)
  diagnostics_table <- do.call(rbind, lapply(seq_along(diagnostics_list), function(i) {
    model_name <- names(models)[i]
    diagnostics <- diagnostics_list[[i]]
    data.frame(
      Model = model_name,
      Weak_Instruments_Statistic = diagnostics[1, "statistic"],
      Weak_Instruments_PValue = diagnostics[1, "p-value"],
      Wu_Hausman_Statistic = diagnostics[2, "statistic"],
      Wu_Hausman_PValue = diagnostics[2, "p-value"]
    )
  }))
  return(diagnostics_table)
}

# Define models for GDP, Gini, and Emissions
gdp_models <- list(
  gdp_ivm01 = ivreg(log_gpd_pc ~ software_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + log_pop + log_nat_res, data = reg_df),
  gdp_ivm05 = ivreg(log_gpd_pc ~ software_eci_norm + trade_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + trade_eci_norm + log_pop + log_nat_res, data = reg_df),
  gdp_ivm06 = ivreg(log_gpd_pc ~ software_eci_norm + tech_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + tech_eci_norm + log_pop + log_nat_res, data = reg_df),
  gdp_ivm07 = ivreg(log_gpd_pc ~ software_eci_norm + research_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + research_eci_norm + log_pop + log_nat_res, data = reg_df),
  gdp_ivm08 = ivreg(log_gpd_pc ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, data = reg_df)
)

gini_models <- list(
  gini_ivm01 = ivreg(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res | sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, data = reg_df),
  gini_ivm05 = ivreg(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + log_pop + log_nat_res, data = reg_df),
  gini_ivm06 = ivreg(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + tech_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + tech_eci_norm + log_pop + log_nat_res, data = reg_df),
  gini_ivm07 = ivreg(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + research_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + research_eci_norm + log_pop + log_nat_res, data = reg_df),
  gini_ivm08 = ivreg(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res | sim_software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, data = reg_df)
)

emissions_models <- list(
  em_ivm01 = ivreg(emission_norm ~ software_eci_norm + log_gpd_pc + log_pop + log_nat_res | sim_software_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df),
  em_ivm05 = ivreg(emission_norm ~ software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop + log_nat_res | sim_software_eci_norm + trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df),
  em_ivm06 = ivreg(emission_norm ~ software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop + log_nat_res | sim_software_eci_norm + tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df),
  em_ivm07 = ivreg(emission_norm ~ software_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res | sim_software_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df),
  em_ivm08 = ivreg(emission_norm ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res | sim_software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_gpd_pc + log_pop + log_nat_res, data = reg_df)
)

# Extract diagnostics for all models
gdp_diag_table <- extract_diagnostics(gdp_models)
gini_diag_table <- extract_diagnostics(gini_models)
em_diag_table <- extract_diagnostics(emissions_models)

# Display the tables
gdp_diag_table
gini_diag_table
em_diag_table





