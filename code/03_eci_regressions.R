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


### GPD vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


gdp_m01 <- feols(log_gpd_pc ~ software_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m02 <- feols(log_gpd_pc ~ trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m03 <- feols(log_gpd_pc ~ tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m04 <- feols(log_gpd_pc ~ research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m05 <- feols(log_gpd_pc ~ software_eci_norm + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m06 <- feols(log_gpd_pc ~ software_eci_norm + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m07 <- feols(log_gpd_pc ~ software_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gdp_m08 <- feols(log_gpd_pc ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_m01, gdp_m02, gdp_m03, gdp_m04, gdp_m05, gdp_m06, gdp_m07, gdp_m08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)




### Gini vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("gini_norm", "log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


gini_m01 <- feols(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m02 <- feols(gini_norm ~ trade_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m03 <- feols(gini_norm ~ tech_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m04 <- feols(gini_norm ~ research_eci_norm + log_gpd_pc + log_gpd_pc2 + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m05 <- feols(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m06 <- feols(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m07 <- feols(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
gini_m08 <- feols(gini_norm ~ software_eci_norm + log_gpd_pc + log_gpd_pc2 + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gini_m01, gini_m02, gini_m03, gini_m04, gini_m05, gini_m06, gini_m07, gini_m08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)




### emissions vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("emission_norm", "log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


em_m01 <- feols(emission_norm ~ software_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m02 <- feols(emission_norm ~ trade_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m03 <- feols(emission_norm ~ tech_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m04 <- feols(emission_norm ~ research_eci_norm + log_gpd_pc + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m05 <- feols(emission_norm ~ software_eci_norm + log_gpd_pc + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m06 <- feols(emission_norm ~ software_eci_norm + log_gpd_pc + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m07 <- feols(emission_norm ~ software_eci_norm + log_gpd_pc + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
em_m08 <- feols(emission_norm ~ software_eci_norm + log_gpd_pc + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  em_m01, em_m02, em_m03, em_m04, em_m05, em_m06, em_m07, em_m08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)




### emissions vs ECI -- example left here for stargazer output
# drop rows w/ NAs in key columns
key_columns <- c("emission_norm", "log_gpd_pc", "software_eci_norm", "trade_eci_norm", "tech_eci_norm", "research_eci_norm", "log_pop", "log_nat_res")
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





###### revision -- new World Bank Data

# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/eci_regression_table.csv")

# 2020 crossec test
df <- subset(df, year==2020)

# manipulation
df$log_gdp_per_capita <- log10(df$gdp_per_capita)
df$log_gdp_per_capita2 <- df$log_gdp_per_capita ** 2
#df$gini_norm <- scale(df$gini_mean)
#df$emission_norm <- scale(df$embodied_emissions)
#df$emission_norm <- scale(df$emissions)
#df$software_eci_norm <- scale(df$software_eci_2020)
df$software_eci_norm <- scale(df$eci)
#df$trade_eci_norm <- scale(df$trade_eci_2020)
#df$tech_eci_norm <- scale(df$tech_eci_2020)
#df$research_eci_norm <- scale(df$research_eci_2020)
df$log_pop <- log10(df$population)
df$log_nat_res <- log10(df$natural_resources)


### GPD vs ECI
# drop rows w/ NAs in key columns
key_columns <- c("log_gdp_per_capita", "software_eci_norm", "log_pop", "log_nat_res")
reg_df <- df[complete.cases(df[, ..key_columns]), ]


summary(gdp_m01 <- feols(log_gdp_per_capita ~ software_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df))
#gdp_m02 <- feols(log_gdp_per_capita ~ trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#gdp_m03 <- feols(log_gdp_per_capita ~ tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#gdp_m04 <- feols(log_gdp_per_capita ~ research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#gdp_m05 <- feols(log_gdp_per_capita ~ software_eci_norm + trade_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#gdp_m06 <- feols(log_gdp_per_capita ~ software_eci_norm + tech_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#gdp_m07 <- feols(log_gdp_per_capita ~ software_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)
#gdp_m08 <- feols(log_gdp_per_capita ~ software_eci_norm + trade_eci_norm + tech_eci_norm + research_eci_norm + log_pop + log_nat_res, vcov = "HC1", data = reg_df)


etable(
  gdp_m01, gdp_m02, gdp_m03, gdp_m04, gdp_m05, gdp_m06, gdp_m07, gdp_m08,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)


