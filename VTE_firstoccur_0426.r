

#
load('vte_whole_set2.rda')
load('control_sample_12345.rda')

mf.wh<-mf.wh2
#negatv

#get those positive patient first and vte = ture
pos_vte<-mf.wh[which(mf.wh$CI_ID_t0 %in% idlist[[1]]),]
neg_vte<-mf.wh[which(!mf.wh$CI_ID_t0 %in% idlist[[1]]),]

#select those with t1
pos_vte<-pos_vte[which(pos_vte$VTE_t1 == 1),]

#select first 
pos_vte<-pos_vte[match(unique(pos_vte$CI_ID_t0),pos_vte$CI_ID_t0),]

#combine new dataset, postive class is round 3.8% , previous is 3.2%
mf.wh_vte_first<-rbind(pos_vte,neg_vte)

save(mf.wh_vte_first, file = "vte_firstoccur_0426.rda")

#prepare a dataset for random forest


