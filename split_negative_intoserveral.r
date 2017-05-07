load('vte_whole_health_cumsum.rda')

current.id<-unique(mf.wh$CI_ID_t0)

load('control_sample_12345.rda')

#rest who are not 
rest.id<-current.id[which(current.id%in%idlist[[1]] == FALSE)]



rest1.id<-sample(rest.id, length(idlist[[1]])*2)
rest2.id<-sample(rest.id, length(idlist[[1]])*2)
rest3.id<-sample(rest.id, length(idlist[[1]])*2)
rest4.id<-sample(rest.id, length(idlist[[1]])*2)
rest5.id<-sample(rest.id, length(idlist[[1]])*2)

#all the id needed to be sampled
sample1.id<-sample(c(idlist[[1]],rest1.id))
sample2.id<-sample(c(idlist[[1]],rest2.id))
sample3.id<-sample(c(idlist[[1]],rest3.id))
sample4.id<-sample(c(idlist[[1]],rest4.id))
sample5.id<-sample(c(idlist[[1]],rest5.id))


mf.wh1<-mf.wh[which(mf.wh$CI_ID_t0%in%sample1.id),]
mf.wh2<-mf.wh[which(mf.wh$CI_ID_t0%in%sample2.id),]
mf.wh3<-mf.wh[which(mf.wh$CI_ID_t0%in%sample3.id),]
mf.wh4<-mf.wh[which(mf.wh$CI_ID_t0%in%sample4.id),]
mf.wh5<-mf.wh[which(mf.wh$CI_ID_t0%in%sample5.id),]

# #check
# sum(mf.wh1$VTE_t1)/nrow(mf.wh1)
# sum(mf.wh2$VTE_t1)/nrow(mf.wh2)
# sum(mf.wh3$VTE_t1)/nrow(mf.wh3)
# sum(mf.wh4$VTE_t1)/nrow(mf.wh4)
# sum(mf.wh5$VTE_t1)/nrow(mf.wh5)


save(mf.wh1,file ='vte_whole_set1.rda')
save(mf.wh2,file ='vte_whole_set2.rda')
save(mf.wh3,file ='vte_whole_set3.rda')
save(mf.wh4,file ='vte_whole_set4.rda')
save(mf.wh5,file ='vte_whole_set5.rda')


#########
#for ivm
#########


current.id<-unique(mf.wh$CI_ID_t0_t0)

load('control_sample_12345.rda')

#rest who are not 
rest.id<-current.id[which(current.id%in%idlist[[2]] == FALSE)]

pos.id<-current.id[which(current.id%in%idlist[[2]] == TRUE)]

rest1.id<-sample(rest.id, length(pos.id)*2)
rest2.id<-sample(rest.id, length(pos.id)*2)
rest3.id<-sample(rest.id, length(pos.id)*2)
rest4.id<-sample(rest.id, length(pos.id)*2)
rest5.id<-sample(rest.id, length(pos.id)*2)



#all the id needed to be sampled
sample1.id<-sample(c(pos.id,rest1.id))
sample2.id<-sample(c(pos.id,rest2.id))
sample3.id<-sample(c(pos.id,rest3.id))
sample4.id<-sample(c(pos.id,rest4.id))
sample5.id<-sample(c(pos.id,rest5.id))


mf.wh1<-mf.wh[which(mf.wh$CI_ID_t0_t0%in%sample1.id),]
mf.wh2<-mf.wh[which(mf.wh$CI_ID_t0_t0%in%sample2.id),]
mf.wh3<-mf.wh[which(mf.wh$CI_ID_t0_t0%in%sample3.id),]
mf.wh4<-mf.wh[which(mf.wh$CI_ID_t0_t0%in%sample4.id),]
mf.wh5<-mf.wh[which(mf.wh$CI_ID_t0_t0%in%sample5.id),]

# #check
# sum(mf.wh1$VTE_t1)/nrow(mf.wh1)
# sum(mf.wh2$VTE_t1)/nrow(mf.wh2)
# sum(mf.wh3$VTE_t1)/nrow(mf.wh3)
# sum(mf.wh4$VTE_t1)/nrow(mf.wh4)
# sum(mf.wh5$VTE_t1)/nrow(mf.wh5)


save(mf.wh1,file ='imv_whole_set1.rda')
save(mf.wh2,file ='imv_whole_set2.rda')
save(mf.wh3,file ='imv_whole_set3.rda')
save(mf.wh4,file ='imv_whole_set4.rda')
save(mf.wh5,file ='imv_whole_set5.rda')