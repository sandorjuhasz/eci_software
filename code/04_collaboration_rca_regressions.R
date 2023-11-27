### related diversification via collaboration
### by jw

library(EconGeo)
library(data.table)
library(dplyr)
library(igraph)
library(mefa4)
library(stargazer)
library(fixest)


fdf=fread('/Users/johanneswachs/Dropbox/majorprojects/empirical_software/github_innovation_graph/git_complexity/outputs/relatedness_country_semester_panel_with_collab_neighbors.csv')

head(fdf)

m1<-feols(rca_entry ~ rel_density | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="DK")
m2<-feols(rca_entry ~ neighbor_related | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="DK")
m3<-feols(rca_entry ~ rel_density+neighbor_related | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="DK")

m4<-feols(rca_entry ~ rel_density+neighbor_related + pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="DK")

m5<-feols(rca_entry ~ rel_density+neighbor_related+rel_density:neighbor_related | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="DK")

m6<-feols(rca_entry ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="DK")

etable(m1,m2,m3,m4,m5,m6,file = '../johanneswachs/Dropbox/majorprojects/empirical_software/github_innovation_graph/git_complexity/outputs/interaction_model_v2.tex')




