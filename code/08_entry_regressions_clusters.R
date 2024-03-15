### related diversification -- test
### by sandorjuhasz

library(data.table)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)

# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/data_entry_regression_2022_to_2023_communities.csv")

# normalize
df$rel_density <- scale(df$rel_density)
df$pci <- scale(df$pci)

df$group <- factor(df$group, levels = c("Systems_Low_Level_Programming", "Machine_Learning_Embedded_Systems", "Scripting_Automation", "Specialized_Research", "Web_Application_Development"))

# models
summary(m1 <- lm(entry01 ~ as.factor(iso2_code) + as.factor(group), data = df))
summary(m2 <- lm(entry01 ~ rel_density + as.factor(iso2_code) + as.factor(group), data = df))
summary(m3 <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) + as.factor(group), data = df))

levels(df$group)

stargazer(
  m1,
  m2,
  m3,
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "Yes", "Yes", "Yes"),
    c("Language FE", "No", "No", "Yes")
  ),
  out = "../outputs/new_entry_2022_2023_clusters.html"
)