load('vte_whole_set2.rda')
#load('control_sample_12345.rda')

#get unique patientid

patientid <- unique(mf.wh2$CI_ID_t0)  #16278
positiveid<- unique(mf.wh2$CI_ID_t0[which(mf.wh2$VTE_t1 == 1)])  #5236 positive

#for those postive patient claims, start from begin
pos.begin<-match(positiveid,mf.wh2$CI_ID_t0)


#All postive claim
pos_vte<-mf.wh2[which(mf.wh2$VTE_t1 == 1),]
first_pos_claim<-pos_vte$Claim_No_t0[match(unique(pos_vte$CI_ID_t0),pos_vte$CI_ID_t0)]

#find the first postive claim position
pos.end<-match(first_pos_claim, mf.wh2$Claim_No_t0)

#create the select list number for positive patients' claim
k<-sapply(1:length(pos.begin), function(i) pos.begin[i]:pos.end[i])
l<-do.call(c,k)

#posivie patient, negative claims before first time occur
pos_first_np<-mf.wh2[l,]


#all the negative patients' claims
neg_n<-mf.wh2[which(!mf.wh2$CI_ID_t0 %in% positiveid),]

final<-rbind(pos_first_np,neg_n)


write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv(mf.wh,'vte_firstoc_addnegbeforefirst.rda')


#####################################################################IMV

load('imv_whole_set5.rda')
#load('control_sample_12345.rda')

mf.wh5<-mf.wh2

#get unique patientid
patientid <- unique(mf.wh2$CI_ID_t0_t0)  #8466
positiveid<- unique(mf.wh2$CI_ID_t0_t0[which(mf.wh2$IMV_t1 == 1)])  #2805 positive

#for those postive patient claims, start from begin
pos.begin<-match(positiveid,mf.wh2$CI_ID_t0_t0)


#All postive claim
pos_imv<-mf.wh2[which(mf.wh2$IMV_t1 == 1),]
first_pos_claim<-pos_imv$Claim_No_t0_t0[match(unique(pos_imv$CI_ID_t0_t0),pos_imv$CI_ID_t0_t0)]

#find the first postive claim position
pos.end<-match(first_pos_claim, mf.wh2$Claim_No_t0_t0)

#create the select list number for positive patients' claim
k<-sapply(1:length(pos.begin), function(i) pos.begin[i]:pos.end[i])
l<-do.call(c,k)

#posivie patient, negative claims before first time occur
pos_first_np<-mf.wh2[l,]


#all the negative patients' claims
neg_n<-mf.wh2[which(!mf.wh2$CI_ID_t0_t0 %in% positiveid),]

final<-rbind(pos_first_np,neg_n)

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv(mf.wh,'imv_firstoc_addnegbeforefirst.rda')

----------











