

# load('vte_select_cum0405.rda')

# #select those IMVT_t1 == 1
# pos_vte<-mf.wh[which(mf.wh$VTE_t1 == 1),]
# pos_vte<-pos_vte[match(unique(pos_vte$CI_ID_t0),pos_vte$CI_ID_t0),] #first time


load('vte_whole_set2.rda')

load('control_sample_12345.rda')

#get only those positive patient
pos_vte<-mf.wh2[which(mf.wh2$CI_ID_t0 %in% idlist[[1]]),]


#select those VTE_t1 == 1 claims
pos_vte1<-pos_vte[which(pos_vte$VTE_t1 == 1),]
#get unique one
pos_vte1<-pos_vte1[match(unique(pos_vte1$CI_ID_t0),pos_vte1$CI_ID_t0),]


#delete those happened at the firsttime
firstpid<-pos_vte1$CI_ID_t0
firstclaimid<-pos_vte1$Claim_No_t0


f<-pos_vte[which(!pos_vte$Claim_No_t0 %in% firstclaimid),]


#####################################################################IMV

load('imv_whole_set5.rda')
load('control_sample_12345.rda')


#get only those positive patient
pos_imv<-mf.wh5[which(mf.wh5$CI_ID_t0_t0 %in% idlist[[2]]),]


#select those IMV_t1 == 1 claims
pos_imv1<-pos_imv[which(pos_imv$IMV_t1 == 1),]
#get unique one
pos_imv1<-pos_imv1[match(unique(pos_imv1$CI_ID_t0),pos_imv1$CI_ID_t0_t0),]


#delete those happened at the firsttime
firstpid<-pos_imv1$CI_ID_t0_t0
firstclaimid<-pos_imv1$Claim_No_t0_t0


f<-pos_imv[which(!pos_imv$Claim_No_t0_t0 %in% firstclaimid),]






