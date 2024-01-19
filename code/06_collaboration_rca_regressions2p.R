### related diversification via collaboration
### by jw

library(EconGeo)
library(data.table)
library(dplyr)
library(igraph)
library(mefa4)
library(stargazer)
library(fixest)


fdf=fread('/Users/johanneswachs/Dropbox/majorprojects/empirical_software/github_innovation_graph/git_complexity/outputs/relatedness_country_semester_panel_with_collab_neighbors_2p.csv')

head(fdf)

### selected cross section
m1<-feols(entry01 ~ rel_density | iso2_code,
          data=fdf,vcov = 'HC1')
m2<-feols(entry01 ~ neighbor_related | iso2_code,
          data=fdf,vcov = 'HC1')
m3<-feols(entry01 ~ rel_density+neighbor_related | iso2_code,
          data=fdf,vcov = 'HC1')
m4<-feols(entry01 ~ rel_density+neighbor_related + pci | iso2_code,
          data=fdf, vcov = 'HC1')
m5<-feols(entry01 ~ rel_density+neighbor_related+rel_density:neighbor_related +pci | iso2_code,
          data=fdf,vcov = 'HC1')
etable(m1,m2,m3,m4,m5)




