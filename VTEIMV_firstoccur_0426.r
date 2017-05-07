

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
p.id<-unique(mf.wh_vte_first$CI_ID_t0)
fold<-rep(1:10,round(length(p.id)/10))
fold<-head(fold,length(p.id))

id.map<-cbind(p.id,fold)
colnames(id.map)<-c('CI_ID_t0','fold')
#map fold to mf.wh according to id
mf.wh_vte_first<-merge(mf.wh_vte_first,id.map, by = 'CI_ID_t0', all.x= TRUE)

mf.wh_vte_first<-mf.wh_vte_first[,-1:-2]


write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv(mf.wh_vte_first,'vte_firstoccr.csv')


############################################################
#below is for imv
############################################################
load('imv_whole_set5.rda')
load('control_sample_12345.rda')

mf.wh<-mf.wh5

#select those IMVT_t1 == 1
pos_imv<-mf.wh[which(mf.wh$IMV_t1 == 1),]
pos_imv<-pos_imv[match(unique(pos_imv$CI_ID_t0_t0),pos_imv$CI_ID_t0_t0),]

#negative part
neg_imv<-mf.wh[which(!mf.wh$CI_ID_t0_t0 %in% idlist[[2]]),]
check<-mf.wh[which(mf.wh$CI_ID_t0_t0 %in% idlist[[2]]),]

#combine
# > sum(mf.wh_imv_first$IMV_t1)/nrow(mf.wh_imv_first)
# [1] 0.03869766
mf.wh_imv_first<-rbind(pos_imv,neg_imv)


save(mf.wh_imv_first, file = "imv_firstoccur_0426.rda")


# here the people has imv have more claim records
# > dim(neg_imv)
# [1] 69680   963
# > check<-mf.wh[which(mf.wh$CI_ID_t0_t0 %in% idlist[[2]]),]
# > dim(check)
# [1] 205769    963

#######################
#prepare for the rf of IMV
#######################
#prepare a dataset for random forest
p.id<-unique(mf.wh_imv_first$CI_ID_t0)
fold<-rep(1:10,round(length(p.id)/10))
fold<-head(fold,length(p.id))

id.map<-cbind(p.id,fold)
colnames(id.map)<-c('CI_ID_t0_t0','fold')
#map fold to mf.wh according to id
mf.wh_imv_first<-merge(mf.wh_imv_first,id.map, by = 'CI_ID_t0_t0', all.x= TRUE)

mf.wh_imv_first<-mf.wh_imv_first[,-1:-2]


write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv(mf.wh_imv_first,'imv_firstoccr.csv')


########################################
#double check vte, try another dataset
########################################


#
load('vte_whole_set1.rda')
load('control_sample_12345.rda')

mf.wh<-mf.wh1
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

save(mf.wh_vte_first, file = "vte_firstoccur_0426_doublecheck.rda")

# #prepare a dataset for random forest
# p.id<-unique(mf.wh_vte_first$CI_ID_t0)
# fold<-rep(1:10,round(length(p.id)/10))
# fold<-head(fold,length(p.id))

# id.map<-cbind(p.id,fold)
# colnames(id.map)<-c('CI_ID_t0','fold')
# #map fold to mf.wh according to id
# mf.wh_vte_first<-merge(mf.wh_vte_first,id.map, by = 'CI_ID_t0', all.x= TRUE)

# mf.wh_vte_first<-mf.wh_vte_first[,-1:-2]


# write.csv <- function(ob, filename) {
#   write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
# }

# write.csv(mf.wh_vte_first,'vte_firstoccr.csv')
