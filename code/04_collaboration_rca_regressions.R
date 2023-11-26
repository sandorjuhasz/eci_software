### related diversification via collaboration
### by jw

library(EconGeo)
library(data.table)
library(dplyr)
library(igraph)
library(mefa4)
library(stargazer)


fdf=fread('../outputs/relatedness_country_semester_panel_with_collab_neighbors.csv')

head(fdf)

summary(m1 <- lm(rca_entry ~ rel_density + neighbor_related, data = fdf))
summary(m1_fe <- lm(rca_entry ~ rel_density +neighbor_related + as.factor(iso2_code), data = fdf))
summary(m2 <- lm(rca_entry ~ rel_density + pci + neighbor_related, data = fdf))
summary(m2_fe <- lm(rca_entry ~ rel_density + pci +neighbor_related + as.factor(iso2_code), data = fdf))
summary(m2I <- lm(rca_entry ~ rel_density + pci + neighbor_related+rel_density:neighbor_related, data = fdf))
summary(m2I_fe <- lm(rca_entry ~ rel_density + pci +neighbor_related +rel_density:neighbor_related+
                      as.factor(iso2_code), data = fdf))

summary(m3_fe <- lm(rca_entry ~ rel_density + pci +neighbor_related +rel_density:neighbor_related+
                      as.factor(iso2_code) + as.factor(language), data = fdf))
summary(m4_fe <- lm(rca_entry ~ rel_density + pci +neighbor_related +rel_density:neighbor_related+
                      as.factor(iso2_code) + as.factor(language) +
                      as.factor(semester_id), data = fdf))

stargazer(
  m1,
  m1_fe,
  m2,
  m2_fe,
  m2I,
  m2I_fe,
  m3_fe,
  m4_fe,
  digits = 4,
  omit = c("iso2_code", "language", "semester_id"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "Yes","No","Yes", "Yes", "Yes"),
    c("Language FE", "No", "No", "No", "No","No","No", "Yes", "Yes"),
    c("Period FE", "No", "No", "No", "No","No","No", "No", "Yes")
  ),
  #type="text"
  out = "../outputs/interaction_model_v1.html"
)





