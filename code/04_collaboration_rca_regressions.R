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


### selected panel models
m1<-feols(rca_entry ~ rel_density | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          cluster = ~iso2_code+semester_id)
m2<-feols(rca_entry ~ neighbor_related | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          cluster = ~iso2_code+semester_id)
m3<-feols(rca_entry ~ rel_density+neighbor_related | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          cluster = ~iso2_code+semester_id)

m4<-feols(rca_entry ~ rel_density+neighbor_related + pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          cluster = ~iso2_code+semester_id)

m5<-feols(rca_entry ~ rel_density+neighbor_related+rel_density:neighbor_related | iso2_code +language + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          cluster = ~iso2_code+semester_id)

m6<-feols(rca_entry ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          cluster = ~iso2_code+semester_id)

etable(m1,m2,m3,m4,m5,m6)

#etable(m1,m2,m3,m4,m5,m6,file = '../johanneswachs/Dropbox/majorprojects/empirical_software/github_innovation_graph/git_complexity/outputs/interaction_model_v2.tex')


m1<-feols(rca_entry ~ rel_density | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,cluster='iso2_code')
m2<-feols(rca_entry ~ neighbor_related | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,cluster='iso2_code')
m3<-feols(rca_entry ~ rel_density+neighbor_related | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,cluster='iso2_code')

m4<-feols(rca_entry ~ rel_density+neighbor_related + pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,cluster='iso2_code')

m5<-feols(rca_entry ~ rel_density+neighbor_related+rel_density:neighbor_related | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,cluster='iso2_code')
          #se="twoway")

m6<-feols(rca_entry ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,cluster='iso2_code')
          #vcov='iso2_code')
          #se="twoway")

etable(m1,m2,m3,m4,m6)



m1<-feols(rca01 ~ rel_density | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="twoway")
m2<-feols(rca01 ~ neighbor_related | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="twoway")
m3<-feols(rca01 ~ rel_density+neighbor_related | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="twoway")
m4<-feols(rca01 ~ rel_density+neighbor_related + pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="twoway")
m5<-feols(rca01 ~ rel_density+neighbor_related+rel_density:neighbor_related | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="twoway")
m6<-feols(rca01 ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code + semester_id,
          panel.id=~iso2_code+semester_id,
          data=fdf,
          se="twoway")

etable(m1,m2,m3,m4,m5,m6)


