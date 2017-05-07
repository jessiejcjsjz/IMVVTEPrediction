#cluster first
#remove the label
library(e1071)
library(rpart)



load('vte_select_cum0405.rda')

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

  test<-my.data[which(my.data$CI_ID_t0%in%choose.id), -1:-2]
  train<-my.data[-which(my.data$CI_ID_t0%in%choose.id), -1:-2]
  
  #kick out constant in both train and test,
  ct<-as.numeric(which(apply(train,2,var)== 0))
  cte<-as.numeric(which(apply(test,2,var) == 0))

  if(length(ct!=0) & length(cte != 0)){
    test<-test[,-c(ct,cte)]
    train<-train[,-c(ct,cte)]
  }

  # test<-as.numeric(test)
  # train<-as.numeric(train)
  
  xgboost.m<- xgboost(
    data = data.matrix(train[,-grep('VTE_t1',colnames(train))]),
    label = train[,grep('VTE_t1',colnames(train))],
    max.depth = 5,
    eta = 1,
    nround = 2,
    objective = "binary:logistic"
  )

  pred.xg<- predict(xgboost.m, data.matrix(test[,-grep('VTE_t1',colnames(test))]))

  x<-data.matrix(train[,-grep('VTE_t1',colnames(train))]),
  y<-train[,grep('VTE_t1',colnames(train))],
  svm.m<-svm(x,y)
  svm.p<-predict(svm.m, )

  result.r<-data.frame(actual = test$VTE_t1, predict = pred.xg)

  result<-rbind(result,result.r)
}