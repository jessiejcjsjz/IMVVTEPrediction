rm(list=ls())
library(data.table)
library(plyr)
# library(ROCR)
# library(pROC)

#setwd("/zfsauton/project/highmark/data/longoutput/readydata")
setwd(getwd())
load('vte_select_cum0405.rda')

mf.wh<-get(ls())

p.id<-unique(mf.wh$CI_ID_t0)



#shuffle the data to avoid soring order
my.data<-mf.wh[sample(nrow(mf.wh)),]

#each time sample size, except for the 10th time
size = round(length(p.id)/10,0)
pool = c(1:length(p.id))


result<-data.frame() #store actual and predict class value
fpr<-c() #
acc<-c() #
tpr<-c() #

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
  test<-my.data[which(my.data$CI_ID_t0%in%choose.id), -1:-2]
  train<-my.data[-which(my.data$CI_ID_t0%in%choose.id), -1:-2]

  v1<-grep('VTE_t1',colnames(train))
  v2<-grep('VTE_t0',colnames(train))

  #prepare new test and train set for multiclass value
  new_test<-test[,-c(v1,v2)]
  new_test$y<-paste(test$VTE_t0,test$VTE_t1)

  new_train<-train[,-c(v1,v2)]
  new_train$y<-paste(train$VTE_t0,train$VTE_t1)

  markov.m<-MASS::lda(y~.,data = new_train)
  pred<-predict(markov.m,new_test)$posterior

  pred_class<-rep(0,nrow(test))
  #for those VTE_t0 == 0, compare only within 00, 01
  pred_class[which(pred[which(test$VTE_t0 == 0),1]<pred[which(test$VTE_t0 ==0),2])] = 1
  #for those VTE_t0 == 1, compare only within 10,11
  pred_class[which(pred[which(test$VTE_t0 > 0),3]<pred[which(test$VTE_t0 > 0),4])] = 1

  ta<-table(pred_class, test$VTE_t1)
  acc.r<-(ta[1,1]+ta[2,2])/sum(ta)
  tpr.r<-(ta[2,2])/sum(ta[,2])
  fpr.r<-(ta[2,1])/sum(ta[,1])
  result.r<-data.frame(actual = test$VTE_t1, predict = pred_class)
  #result.r<-data.frame(actual = test$y, predict = pred)
  acc[k]<-acc.r
  tpr[k]<-tpr.r
  fpr[k]<-fpr.r
  result<-rbind(result,result.r)
}

save(acc,fpr,tpr,result,file = '10folds_lda_VTE_multiclass.rda')

# #plot the ROC Curve
# predror<-prediction(as.numeric(as.character(result$predict)),
#                     as.numeric(as.character(result$actual)))
                    
# pred.f<-performance(predror,"tpr","fpr")
# plot(pred.f)

# #calculate AUC
# auc.ob<-roc(as.numeric(as.character(result$actual)),as.numeric(as.character(result$predict)))
# auc(auc.ob)
