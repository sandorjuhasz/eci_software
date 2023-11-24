### related diversification -- test
### by sandorjuhasz

library(EconGeo)
library(data.table)
library(dplyr)
library(igraph)
library(mefa4)
library(stargazer)


# software space to relatedness matrix
ssel <- fread("../outputs/software_space_edgelist2020-2021.csv") %>% select(-drop)
ssel$proximity <- 1
relatedness <- as.matrix(get.adjacency(graph.data.frame(ssel)))
languages <- unique(c(ssel$language_1, ssel$language_2))


# Mcp matrix -- relatedness density
mcps <- fread("../outputs/entry_table.csv")
mcps <- subset(mcps, language %in% languages)
  
periods <- unique(mcps$semester_id)
rel_dens <- list()
for(p in 1:length(periods)){
  print(periods[p])
  mcp <- subset(mcps, semester_id == periods[p])
  mcp <- select(mcp, iso2_code, language, rca01)
  mcp_mat <- EconGeo::get_matrix(mcp)
  
  rel_density <- EconGeo::relatedness_density(mcp_mat, relatedness)
  rel_density <- data.table(Melt(rel_density))
  colnames(rel_density) <- c("iso2_code", "language", "rel_density")
  rel_density$semester_id <- periods[p]
  #rel_density$rel_density[is.na(rel_density$rel_density)==1] <- 0
  rel_dens[[p]] <- rel_density
}
rel_dens <- rbindlist(rel_dens)


# add relatedness density to entry table
edf <- merge(
  mcps,
  rel_dens,
  by = c("semester_id", "iso2_code", "language"),
  all.x = TRUE,
  all.y = FALSE
)
edf$rel_density[is.na(edf$rel_density)==1] <- 0


# complexity
cdf <- fread("../outputs/complexity_table2020-2021.csv")
cdf <- select(cdf, language, pci) %>% unique()
edf <- merge(
  edf,
  cdf,
  by = "language",
  all.x = TRUE,
  all.y = FALSE
)


summary(m1 <- lm(rca_entry ~ rel_density, data = edf))
summary(m1_fe <- lm(rca_entry ~ rel_density + as.factor(iso2_code), data = edf))
summary(m2 <- lm(rca_entry ~ rel_density + pci, data = edf))
summary(m2_fe <- lm(rca_entry ~ rel_density + pci + as.factor(iso2_code), data = edf))
summary(m3_fe <- lm(rca_entry ~ rel_density + pci + as.factor(iso2_code) + as.factor(language), data = edf))
summary(m4_fe <- lm(rca_entry ~ rel_density + pci + as.factor(iso2_code) + as.factor(language) + as.factor(semester_id), data = edf))

stargazer(
  m1,
  m1_fe,
  m2,
  m2_fe,
  m3_fe,
  m4_fe,
  omit = c("iso2_code", "language", "semester_id"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "Yes", "Yes", "Yes"),
    c("Language FE", "No", "No", "No", "No", "Yes", "Yes"),
    c("Period FE", "No", "No", "No", "No", "No", "Yes")
  ),
  #type="text"
  out = "../outputs/baseline_model_v2.html"
)








### previous version -- kept for now ###

# complexity table to Mcp matrix
cdf <- fread("../outputs/complexity_table2020.csv")
cdf$rca01 <- ifelse(cdf$rca >= 1, 1, 0)
mcp <- select(cdf, iso2_code, language, rca01)
mcp_mat <- EconGeo::get_matrix(mcp)


# relatedness density
rel_density <- EconGeo::relatedness_density(mcp_mat, relatedness)
rel_density <- data.table(Melt(rel_density))
colnames(rel_density) <- c("country", "language", "rel_density")
  

# construct 2020 baseline dataframe
df1 <- select(cdf, iso2_code, language, pci, rca01)
df1 <- merge(
  df1,
  rel_density,
  by.x = c("iso2_code", "language"),
  by.y = c("country", "language"),
  all.x = TRUE,
  all.y = FALSE
)
df1$rel_density[is.na(df1$rel_density)==1] <- 0


# simple 2023 dataframe
df2 <- fread("../outputs/complexity_table2022.csv") %>%
  select(iso2_code, language, mcp) %>%
  rename(rca01_2023 = mcp)

reg_df <- merge(
  df1,
  df2,
  by = c("iso2_code", "language"),
  all.x = TRUE,
  all.y = FALSE
)


# entry and regression test
reg_df$entry <- ifelse(reg_df$rca01==0 & reg_df$rca01_2023==1, 1, 0)

summary(m1 <- lm(entry ~ rel_density, data = reg_df))
summary(m2 <- lm(entry ~ rel_density + pci, data = reg_df))
summary(m2_fe <- lm(entry ~ rel_density + as.factor(iso2_code), data = reg_df))
summary(m2_fe <- lm(entry ~ rel_density + pci + as.factor(iso2_code), data = reg_df))

stargazer(
  m1,
  m2,
  type="text"
)

stargazer(
  m1,
  m2,
  m2_fe,
  omit = c("iso2_code"),
  add.lines=list(c("Country FE", "No", "NO", "Yes")),
  type="text"
)

