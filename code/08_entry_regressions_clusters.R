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

# Group names
group_names <- c("Machine_Learning_Embedded_Systems", 
                 "Scripting_Automation", 
                 "Specialized_Research", 
                 "Systems_Low_Level_Programming", 
                 "Web_Application_Development")

# Create dummy variables for the 'group' variable, without excluding one as reference
group_dummies <- model.matrix(~ group - 1, data = df, colnames = group_names)
df <- cbind(df, group_dummies)

# models
summary(m1 <- lm(entry01 ~ rel_density, data = df))
summary(m1_fe <- lm(entry01 ~ rel_density + as.factor(iso2_code), data = df))
summary(m1_fe2 <- lm(entry01 ~ rel_density + as.factor(language), data = df))
summary(m2 <- lm(entry01 ~ rel_density + pci, data = df))
summary(m2_fe <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code), data = df))
summary(m3_fe2 <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) +  group_dummies, data = df))

stargazer(
  m1,
  m1_fe,
  m1_fe2,
  m2,
  m2_fe,
  m3_fe2,
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "No", "Yes", "Yes"),
    c("Language FE", "No", "No", "Yes", "No", "No", "No")
  ),
  out = "../outputs/new_entry_2022_2023_communities.html"
)