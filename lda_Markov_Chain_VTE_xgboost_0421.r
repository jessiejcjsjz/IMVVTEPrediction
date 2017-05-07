rm(list=ls())
library(xgboost)
# library(ROCR)
# library(pROC)

#setwd("/zfsauton/project/highmark/data/longoutput/readydata")
args <- as.numeric(commandArgs(trailingOnly=TRUE))

name<-c()
if (args == 1) {name = 'vte_whole_set1.rda'}
if (args == 2) {name = 'vte_whole_set2.rda'}
if (args == 3) {name = 'vte_whole_set3.rda'}
if (args == 4) {name = 'vte_whole_set4.rda'}
if (args == 5) {name = 'vte_whole_set5.rda'}

#setwd(getwd())

load(name)

print('data loaded')


list<-ls()
list<-list[grep('mf.wh',list)]

mf.wh<-get(list)


p.id<-unique(mf.wh$CI_ID_t0)

#shuffle the data to avoid soring order
my.data<-mf.wh[sample(nrow(mf.wh)),]

#each time sample size, except for the 10th time
size = round(length(p.id)/10,0)
pool = c(1:length(p.id))


result<-data.frame() #store actual and predict class value

print("start 10 fold cross validation")
#10 fold cross validation
for(k in 1:10) {
  if(k != 10){
    sample.id<-sample(pool,size = size, replace = FALSE)
    pool<-pool[-sample.id]
    choose.id<-p.id[sample.id]
  }
  if(k == 10){
    choose.id<-p.id[sample.id] #when k = 10, sample the rest of p.id 
  }

  #turn them into numreric for xgboost algorihtm
  test<-data.matrix(my.data[which(my.data$CI_ID_t0%in%choose.id), -1:-2])
  train<-data.matrix(my.data[-which(my.data$CI_ID_t0%in%choose.id), -1:-2])
  
  #kick out constant in both train and test,
  ct<-as.numeric(which(apply(train,2,var)== 0))
  cte<-as.numeric(which(apply(test,2,var) == 0))

  if(length(ct!=0) & length(cte != 0)){
    test<-test[,-c(ct,cte)]
    train<-train[,-c(ct,cte)]
  }

  xgboost.m<- xgboost(
    data = data.matrix(train[,-grep('VTE_t1',colnames(train))]),
    label = train[,grep('VTE_t1',colnames(train))],
    max.depth = 5,
    eta = 1,
    nround = 2,
    objective = "binary:logistic"
  )
  

  pred.xg<- predict(xgboost.m, data.matrix(test[,-grep('VTE_t1',colnames(test))]))

  #result.r<-data.frame(actual = test$VTE_t1, predict = pred.xg)
  result.r<-data.frame(actual = test[,grep('VTE_t1',colnames(test))], predict= pred.xg)

  result<-rbind(result,result.r)

  print(k)
}

filename <-paste0('10folds_xg_vte_health',args,'.rda')

print('saving result')

save(result,file = filename)

#plot the ROC Curve
# predror<-prediction(as.numeric(as.character(result$predict)),
#                     as.numeric(as.character(result$actual)))
                    
# pred.f<-performance(predror,"tpr","fpr")
# plot(pred.f)

# #calculate AUC
# auc.ob<-roc(as.numeric(as.character(result$actual)),as.numeric(as.character(result$predict)))
# auc(auc.ob)


