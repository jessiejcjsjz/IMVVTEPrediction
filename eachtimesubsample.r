rm(list=ls())
library(data.table)
library(plyr)
library(ROCR)
library(pROC)

#setwd("/zfsauton/project/highmark/data/longoutput/readydata")
setwd(getwd())
#load('vte_select_cum0405.rda')

mf.wh<-get(ls())

#kick out constant
mf.wh<-mf.wh[,as.numeric(which(apply(mf.wh,2,var)>0))]

p.id<-unique(mf.wh$CI_ID_t0)



#shuffle the data to avoid soring order
my.data<-mf.wh[sample(nrow(mf.wh)),]

#each time sample size, except for the 10th time
size = round(length(p.id)/10,0)
pool = c(1:length(p.id))


result<-data.frame() #store actual and predict class value
# fpr<-c() #
# acc<-c() #
# tpr<-c() #

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
  #-1,-2 drop for VTE
  # test<-my.data[which(my.data$CI_ID_t0%in%choose.id), -1:-2]
  # train<-my.data[-which(my.data$CI_ID_t0%in%choose.id), -1:-2]

  #-1,-8 drop for IMV
  test<-my.data[which(my.data$CI_ID_t0%in%choose.id), -1:-8]
  train<-my.data[-which(my.data$CI_ID_t0%in%choose.id), -1:-8]
  
  #create a relatively balanced subsample
  #y<-which(train$VTE_t1 == 1) 
  y<-which(train$IMV_t1 == 1)
  keep_row<-c(y,(y-1),(y-2),(y+1))

  if(length(which(keep_row<0)) != 0){
  	keep_row<-keep_row[-which(keep_row<0)]
  }

  train<-train[keep_row,]
  # data<-ubENN(X = train[,-grep('VTE_t1',colnames(train))], Y = train$VTE_t1)
  #new_train<-cbind(data$X,data$Y)

  #kick out constant in both train and test,
  ct<-as.numeric(which(apply(train,2,var)== 0))
  cte<-as.numeric(which(apply(test,2,var) == 0))

  if(length(ct!=0) & length(cte != 0)){
    test<-test[,-c(ct,cte)]
    train<-train[,-c(ct,cte)]
  }

  markov.m<-MASS::lda(IMV_t1~.,data = train)
  pred<-predict(markov.m,test)$posterior
  result.r<-as.data.frame(cbind(actual = test$IMV_t1,pred))
  result<-rbind(result,result.r)
}

#save(acc,fpr,tpr,result,file = '10folds_lda_VTE_pca_z.rda')

# #plot the ROC Curve
# predror<-prediction(as.numeric(as.character(result$predict)),
#                     as.numeric(as.character(result$actual)))
                    
# pred.f<-performance(predror,"tpr","fpr")
# plot(pred.f)

# #calculate AUC
# auc.ob<-roc(as.numeric(as.character(result$actual)),as.numeric(as.character(result$predict)))
# auc(auc.ob)
