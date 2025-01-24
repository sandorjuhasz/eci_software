### related diversification
### by sandorjuhasz


library(data.table)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)
library(htmlTable)

# dataframe from 01_data_prep.ipynb


### ENTRY 
en_df <- fread("../outputs/data_entry_regressions_0011.csv")


# normalize
en_df$rel_density <- scale(en_df$density)
en_df$pci <- scale(en_df$pci)
en_df$ubiquity <- scale(en_df$ubiquity)


ent_m1 <- feols(entry01 ~ density, cluster = "iso2_code", data = en_df)
ent_m2 <- feols(entry01 ~ density | iso2_code, cluster = "iso2_code", data = en_df)
ent_m3 <- feols(entry01 ~ density | language, cluster = "iso2_code", data = en_df)
ent_m4 <- feols(entry01 ~ density | iso2_code + language, cluster = "iso2_code", data = en_df)
ent_m5 <- feols(entry01 ~ ubiquity, cluster = "iso2_code", data = en_df)
ent_m6 <- feols(entry01 ~ density + ubiquity, cluster = "iso2_code", data = en_df)
ent_m7 <- feols(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = en_df)

etable(
  ent_m1, ent_m2, ent_m3, ent_m4, ent_m5, ent_m6, ent_m7,
  #fitstat = ~ r2,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)


### ENTRY -- SI
logit_ent_m1 <- feglm(entry01 ~ density, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m2 <- feglm(entry01 ~ density | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m3 <- feglm(entry01 ~ density | language, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m4 <- feglm(entry01 ~ density | iso2_code + language, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m5 <- feglm(entry01 ~ ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m6 <- feglm(entry01 ~ density + ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)
logit_ent_m7 <- feglm(entry01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = en_df)

etable(
  logit_ent_m1, logit_ent_m2, logit_ent_m3, logit_ent_m4, logit_ent_m5, logit_ent_m6, logit_ent_m7,
  #fitstat = ~ r2,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)




### EXIT
ex_df <- fread("../outputs/data_exit_regressions_1100.csv")


# normalize
ex_df$rel_density <- scale(ex_df$density)
ex_df$pci <- scale(ex_df$pci)
ex_df$ubiquity <- scale(ex_df$ubiquity)


ex_m1 <- feols(exit01 ~ density, cluster = "iso2_code", data = ex_df)
ex_m2 <- feols(exit01 ~ density | iso2_code, cluster = "iso2_code", data = ex_df)
ex_m3 <- feols(exit01 ~ density | language, cluster = "iso2_code", data = ex_df)
ex_m4 <- feols(exit01 ~ density | iso2_code + language, cluster = "iso2_code", data = ex_df)
ex_m5 <- feols(exit01 ~ ubiquity, cluster = "iso2_code", data = ex_df)
ex_m6 <- feols(exit01 ~ density + ubiquity, cluster = "iso2_code", data = ex_df)
ex_m7 <- feols(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", data = ex_df)

etable(
  ex_m1, ex_m2, ex_m3, ex_m4, ex_m5, ex_m6, ex_m7,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)


### EXIT -- SI
logit_ex_m1 <- feglm(exit01 ~ density, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m2 <- feglm(exit01 ~ density | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m3 <- feglm(exit01 ~ density | language, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m4 <- feglm(exit01 ~ density | iso2_code + language, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m5 <- feglm(exit01 ~ ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m6 <- feglm(exit01 ~ density + ubiquity, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)
logit_ex_m7 <- feglm(exit01 ~ density + ubiquity | iso2_code, cluster = "iso2_code", family = binomial(link = "logit"), data = ex_df)

etable(
  logit_ex_m1, logit_ex_m2, logit_ex_m3, logit_ex_m4, logit_ex_m5, logit_ex_m6, logit_ex_m7,
  #fitstat = ~ r2,
  #digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
  #tex = TRUE
)






### stargazer version for quick reproduction
summary(m1 <- lm(entry01 ~ rel_density, data = df))
summary(m1_fe <- lm(entry01 ~ rel_density + as.factor(iso2_code), data = df))
summary(m1_fe2 <- lm(entry01 ~ rel_density + as.factor(language), data = df))
summary(m2 <- lm(entry01 ~ ubiquity, data = df))
summary(m3 <- lm(entry01 ~ rel_density + ubiquity, data = df))
summary(m3_fe <- lm(entry01 ~ rel_density + ubiquity + as.factor(iso2_code), data = df))
summary(m3_fe2 <- lm(entry01 ~ rel_density + ubiquity + as.factor(iso2_code) + as.factor(language), data = df))


stargazer(
  m1,
  m1_fe,
  m1_fe2,
  m2,
  m3,
  m3_fe,
  m3_fe2,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("Entry"),
  covariate.labels = c("Relatedness density", "Language ubiquity"),
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "No", "No", "Yes", "Yes"),
    c("Language FE", "No", "No", "Yes", "No", "No", "No", "Yes")
  ),
  out = "../outputs/table5_entry_regressions_0011.html"
)



# model versions 1.2 -- fixest linear probability w/ diff SE clustering

fxm1 <- feols(entry01 ~ rel_density, data = df)
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df)
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df)
fxm2 <- feols(entry01 ~ ubiquity, data = df)
fxm3 <- feols(entry01 ~ rel_density + ubiquity, data = df)
fxm3_fe <- feols(entry01 ~ rel_density + ubiquity | iso2_code, data = df)
fxm3_fe2 <- feols(entry01 ~ rel_density + ubiquity | iso2_code + language, data = df)

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe, fxm3_fe2)



fxm1 <- feols(entry01 ~ rel_density, data = df, vcov = "HC1")
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df, vcov = "HC1")
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df, vcov = "HC1")
fxm2 <- feols(entry01 ~ ubiquity, data = df, vcov = "HC1")
fxm3 <- feols(entry01 ~ rel_density + ubiquity, data = df, vcov = "HC1")
fxm3_fe <- feols(entry01 ~ rel_density + ubiquity | iso2_code, data = df, vcov = "HC1")
fxm3_fe2 <- feols(entry01 ~ rel_density + ubiquity | iso2_code + language, data = df, vcov = "HC1")

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe, fxm3_fe2)




fxm1 <- feols(entry01 ~ rel_density, data = df, cluster = "iso2_code")
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df, cluster = "iso2_code")
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df, cluster = "iso2_code")
fxm2 <- feols(entry01 ~ ubiquity, data = df, cluster = "iso2_code")
fxm3 <- feols(entry01 ~ rel_density + ubiquity, data = df, cluster = "iso2_code")
fxm3_fe <- feols(entry01 ~ rel_density + ubiquity | iso2_code, data = df, cluster = "iso2_code")
fxm3_fe2 <- feols(entry01 ~ rel_density + ubiquity | iso2_code + language, data = df, cluster = "iso2_code")

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe, fxm3_fe2)




fxm1 <- feols(entry01 ~ rel_density, data = df, cluster = "language")
fxm1_fe <- feols(entry01 ~ rel_density | iso2_code, data = df, cluster = "language")
fxm1_fe2 <- feols(entry01 ~ rel_density | language, data = df, cluster = "language")
fxm2 <- feols(entry01 ~ ubiquity, data = df, cluster = "language")
fxm3 <- feols(entry01 ~ rel_density + ubiquity, data = df, cluster = "language")
fxm3_fe <- feols(entry01 ~ rel_density + ubiquity | iso2_code, data = df, cluster = "language")
fxm3_fe2 <- feols(entry01 ~ rel_density + ubiquity | iso2_code + language, data = df, cluster = "language")

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe, fxm3_fe2)





# model versions 1 -- linear probability
summary(m1 <- lm(exit01 ~ rel_density, data = df))
summary(m1_fe <- lm(exit01 ~ rel_density + as.factor(iso2_code), data = df))
summary(m1_fe2 <- lm(exit01 ~ rel_density + as.factor(language), data = df))
summary(m2 <- lm(exit01 ~ ubiquity, data = df))
summary(m3 <- lm(exit01 ~ rel_density + ubiquity, data = df))
summary(m3_fe <- lm(exit01 ~ rel_density + ubiquity + as.factor(iso2_code), data = df))
summary(m3_fe2 <- lm(exit01 ~ rel_density + ubiquity + as.factor(iso2_code) + as.factor(language), data = df))


stargazer(
  m1,
  m1_fe,
  m1_fe2,
  m2,
  m3,
  m3_fe,
  m3_fe2,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("Exit"),
  covariate.labels = c("Relatedness density", "Language ubiquity"),
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "No", "No", "Yes", "Yes"),
    c("Language FE", "No", "No", "Yes", "No", "No", "No", "Yes")
  ),
  out = "../outputs/table5_exit_regressions_1100.html"
)


fxm1 <- feols(exit01 ~ rel_density, data = df)
fxm2 <- feols(exit01 ~ ubiquity, data = df)
fxm3 <- feols(exit01 ~ rel_density + ubiquity, data = df)

etable(fxm1, fxm1_fe, fxm1_fe2, fxm2, fxm3, fxm3_fe, fxm3_fe2)












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

