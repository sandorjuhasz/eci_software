### related diversification -- test
### by sandorjuhasz

library(EconGeo)
library(data.table)
library(dplyr)
library(igraph)
library(mefa4)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)


# dataframe from 01_data_prep.ipynb
#df <- fread("../outputs/regression_df01_2020_2022.csv")
#df <- fread("../outputs/data_entry_regression_version1.csv")
#df <- fread("../outputs/data_entry_regression_version2.csv")
# df <- fread("../outputs/data_entry_regression_version3_semester_based.csv")

#df <- fread("../outputs/data_entry_regression_version1_log.csv")
df <- fread("../outputs/data_entry_regression_version2_log.csv")
#df <- fread("../outputs/data_entry_regression_version3_semester_based_log.csv")

# normalize
df$rel_density <- scale(df$rel_density)
df$pci <- scale(df$pci)


# models
summary(m1 <- lm(entry01 ~ rel_density, data = df))
summary(m1_fe <- lm(entry01 ~ rel_density + as.factor(iso2_code), data = df))
summary(m1_fe2 <- lm(entry01 ~ rel_density + as.factor(language), data = df))
summary(m2 <- lm(entry01 ~ rel_density + pci, data = df))
summary(m2_fe <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code), data = df))
summary(m3_fe2 <- lm(entry01 ~ rel_density + as.factor(iso2_code) + as.factor(language), data = df))
summary(m3_fe2 <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) + as.factor(language), data = df))



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
    c("Language FE", "No", "No", "Yes", "No", "No", "Yes")
  ),
  #type="text"
  #out = "../outputs/new_entry_2022_2023_version3_semester_based.html"
  out = "../outputs/new_entry_2022_2023_version2_log.html"
)



m1<-feols(entry01 ~ rel_density | iso2_code,
          data=df,vcov = 'HC1')
m2<-feols(entry01 ~ rel_density + pci | iso2_code,
          data=df,vcov = 'HC1')
etable(m1,m2)


m1<-feols(entry01 ~ rel_density | iso2_code,
          data=df,
          cluster = "language")
m2<-feols(entry01 ~ rel_density + pci | iso2_code,
          data=df,
          cluster = "language")
etable(m1,m2)


# cluster='iso2_code'
#m3<-feols(entry01 ~ rel_density+neighbor_related | iso2_code,
#          data=fdf,vcov = 'HC1')
#m4<-feols(entry01 ~ rel_density+neighbor_related + pci | iso2_code,
#          data=fdf, vcov = 'HC1')
#m5<-feols(entry01 ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code,
#          data=fdf,vcov = 'HC1')






library(fixest)
fdf <- fread("../outputs/relatedness_country_semester_panel_with_collab_neighbors_2p.csv")
m1<-feols(entry01 ~ rel_density | iso2_code,
          data=fdf,vcov = 'HC1')
m2<-feols(entry01 ~ rel_density + pci | iso2_code,
          data=fdf,vcov = 'HC1')
#m3<-feols(entry01 ~ rel_density+neighbor_related | iso2_code,
#          data=fdf,vcov = 'HC1')
#m4<-feols(entry01 ~ rel_density+neighbor_related + pci | iso2_code,
#          data=fdf, vcov = 'HC1')
#m5<-feols(entry01 ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code,
#          data=fdf,vcov = 'HC1')
etable(m1,m2)






# software space to relatedness matrix
ssel <- fread("../outputs/software_space_edgelist2020-2021.csv") %>% select(-drop)
ssel$proximity <- 1
relatedness <- as.matrix(get.adjacency(graph.data.frame(ssel)))
languages <- unique(c(ssel$language_1, ssel$language_2))




mcps <- fread("../outputs/entry_table_smooth.csv")
mcps <- subset(mcps, language %in% languages)
mcp <- select(mcps, iso2_code, language, rca01)
mcp$rca01 <- as.numeric(mcp$rca01)
mcp_mat <- EconGeo::get_matrix(mcp)




# Mcp matrix -- relatedness density
mcps <- fread("../outputs/entry_table_2periods.csv")
mcps <- subset(mcps, language %in% languages)
  
periods <- unique(mcps$year)
rel_dens <- list()
for(p in 1:length(periods)){
  print(periods[p])
  mcp <- subset(mcps, year == periods[p])
  mcp <- select(mcp, iso2_code, language, rca01)
  mcp_mat <- EconGeo::get_matrix(mcp)
  
  rel_density <- EconGeo::relatedness_density(mcp_mat, relatedness)
  rel_density <- data.table(Melt(rel_density))
  colnames(rel_density) <- c("iso2_code", "language", "rel_density")
  rel_density$year <- periods[p]
  #rel_density$rel_density[is.na(rel_density$rel_density)==1] <- 0
  rel_dens[[p]] <- rel_density
}
rel_dens <- rbindlist(rel_dens)


# add relatedness density to entry table
edf <- merge(
  mcps,
  rel_dens,
  by = c("year", "iso2_code", "language"),
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


# export
write.table(edf,
            paste0("../outputs/entry_reg_table_2periods.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)



# variable manipulation
edf$rel_density <- scale(edf$rel_density)
edf$pci <- scale(edf$pci)





summary(m1 <- lm(entry01 ~ rel_density, data = edf))
summary(m1_fe <- lm(entry01 ~ rel_density + as.factor(iso2_code), data = edf))
summary(m1_fe2 <- lm(entry01 ~ rel_density + as.factor(iso2_code) + as.factor(language), data = edf))
summary(m2 <- lm(entry01 ~ rel_density + pci, data = edf))
summary(m2_fe <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code), data = edf))
#summary(m3_fe <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) + as.factor(language), data = edf))
#summary(m4_fe <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) + as.factor(language) + as.factor(semester_id), data = edf))

stargazer(
  m1,
  m1_fe,
  m1_fe2,
  m2,
  m2_fe,
  #m3_fe,
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "Yes", "No", "Yes"),
    c("Language FE", "No", "No", "Yes", "No", "No")
  ),
  #type="text"
  out = "../outputs/baseline_model_entry01.html"
)

cm1 <- coeftest(m1, vcov = vcovCL, cluster = ~language)
cm1_fe <- coeftest(m1_fe, vcov = vcovCL, cluster = ~language)
#cm1_fe2 <- coeftest(m1_fe2, vcov = vcovCL, cluster = ~language)
cm2 <- coeftest(m2, vcov = vcovCL, cluster = ~language)
cm2_fe <- coeftest(m2_fe, vcov = vcovCL, cluster = ~language)
cm3_fe <- coeftest(m3_fe, vcov = vcovCL, cluster = ~language)

stargazer(
  cm1,
  cm1_fe,
  m1_fe2,
  cm2,
  cm2_fe,
  cm3_fe,
  omit = c("iso2_code", "language"),
  add.lines=list(
    c("Country FE", "No", "Yes", "No", "Yes", "Yes"),
    c("Language FE", "No", "No", "No", "No", "Yes")
  ),
  #type="text"
  out = "../outputs/baseline_model_entry01_cse.html"
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




# descriptives -- exploration
test <- df %>%
  group_by(iso2_code) %>%
  summarise(sum_rca01 = sum(rca01)) %>%
  data.table()



