#to assign folder to based on id
#10 folds 



p.id<-unique(mf.wh$CI_ID_t0)
fold<-rep(1:10,round(length(p.id)/10))
fold<-head(fold,length(p.id))

id.map<-cbind(p.id,fold)
colnames(id.map)<-c('CI_ID_t0','fold')
#map fold to mf.wh according to id
mf.wh<-merge(mf.wh,id.map, by = 'CI_ID_t0', all.x= TRUE)

mf.wh<-mf.wh[,-1:-2]


write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv(mf.wh,'vte_firstoc_addnegbeforefirst.csv')



#########
p.id<-unique(mf.wh$CI_ID_t0_t0)
fold<-rep(1:10,round(length(p.id)/10))
fold<-head(fold,length(p.id))

id.map<-cbind(p.id,fold)
colnames(id.map)<-c('CI_ID_t0_t0','fold')
#map fold to mf.wh according to id
mf.wh<-merge(mf.wh,id.map, by = 'CI_ID_t0_t0', all.x= TRUE)

mf.wh<-mf.wh[,-1:-2]


write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv(mf.wh,'imv_firstoc_addnegbeforefirst.csv')



