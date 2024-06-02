### related diversification
### by sandorjuhasz


library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)


# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/data_entry_regressions_0011.csv")


# normalize
df$rel_density <- scale(df$rel_density)
df$pci <- scale(df$pci)
df$ubiquity <- scale(df$ubiquity)


# model versions 1 -- linear probability
summary(m1 <- lm(entry01 ~ rel_density, data = df))
summary(m1_fe <- lm(entry01 ~ rel_density + as.factor(iso2_code), data = df))
summary(m1_fe2 <- lm(entry01 ~ rel_density + as.factor(language), data = df))
summary(m2 <- lm(entry01 ~ ubiquity, data = df))
summary(m3 <- lm(entry01 ~ rel_density + ubiquity, data = df))
summary(m3_fe <- lm(entry01 ~ rel_density + ubiquity + as.factor(iso2_code), data = df))
#summary(m3_fe2 <- lm(entry01 ~ rel_density + as.factor(iso2_code) + as.factor(language), data = df))
#summary(m5_fe2 <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) + as.factor(language), data = df))


stargazer(
  m1,
  m1_fe,
  m1_fe2,
  m2,
  m3,
  m3_fe,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("Entry"),
  covariate.labels = c("Relatedness density", "Language ubiquity"),
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "No", "No", "Yes"),
    c("Language FE", "No", "No", "Yes", "No", "No", "No")
  ),
  out = "../outputs/table5_entry_regressions.html"
)



# model versions 1.2 -- fixest linear probability w/ diff SE clustering

fxm1 <- feols(entry01 ~ rel_density, data = df)
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df)
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df)
fxm2 <- feols(entry01 ~ pci, data = df)
fxm3 <- feols(entry01 ~ rel_density + pci, data = df)
fxm3_fe <- feols(entry01 ~ rel_density + pci | iso2_code, data = df)

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe)


fxm1 <- feols(entry01 ~ rel_density, data = df, vcov = "HC1")
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df, vcov = "HC1")
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df, vcov = "HC1")
fxm2 <- feols(entry01 ~ pci, data = df, vcov = "HC1")
fxm3 <- feols(entry01 ~ rel_density + pci, data = df, vcov = "HC1")
fxm3_fe <- feols(entry01 ~ rel_density + pci | iso2_code, data = df, vcov = "HC1")

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe)


fxm1 <- feols(entry01 ~ rel_density, data = df, cluster = "iso2_code")
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df, cluster = "iso2_code")
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df, cluster = "iso2_code")
fxm2 <- feols(entry01 ~ pci, data = df, cluster = "iso2_code")
fxm3 <- feols(entry01 ~ rel_density + pci, data = df, cluster = "iso2_code")
fxm3_fe <- feols(entry01 ~ rel_density + pci | iso2_code, data = df, cluster = "iso2_code")

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe)



# model version 2 -- logistic regression
summary(logm1 <- glm(entry01 ~ rel_density, data = df, family = binomial))
summary(logm1_fe <- glm(entry01 ~ rel_density + as.factor(iso2_code), data = df, family = binomial))
summary(logm1_fe2 <- glm(entry01 ~ rel_density + as.factor(language), data = df, family = binomial))
summary(logm2 <- glm(entry01 ~ pci, data = df, family = binomial))
summary(logm3 <- glm(entry01 ~ rel_density + pci, data = df, family = binomial))
summary(logm3_fe <- glm(entry01 ~ rel_density + pci + as.factor(iso2_code), data = df, family = binomial))


stargazer(
  logm1,
  logm1_fe,
  logm1_fe2,
  logm2,
  logm3,
  logm3_fe,
  dep.var.caption = "",
  dep.var.labels = c("Entry (2022-2023)"),
  covariate.labels = c("Relatedness density", "Language complexity"),
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "No", "No", "Yes"),
    c("Language FE", "No", "No", "Yes", "No", "No", "No")
  ),
  out = "../outputs/table3_entry_regressions_logistic_regressions.html"
)

