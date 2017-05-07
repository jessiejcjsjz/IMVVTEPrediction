rm(list=ls())


setwd('/zfsauton/project/highmark/data/longoutput/readydata')
load('imv_selectbig_cum0405.rda')

#get all the patient id and sample 10% of it
p.id = unique(mf.wh$CI_ID_t0_t0)
sample.id = sample(p.id, size = length(p.id)*0.1)

#get new sampled data file
mf.wh<-mf.wh[which(mf.wh$CI_ID_t0_t0%in%sample.id),]