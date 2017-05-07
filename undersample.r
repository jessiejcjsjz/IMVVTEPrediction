#cluster first
#remove the label
library(e1071)
library(rpart)



load('vte_select_cum0405.rda')

p.id<-unique(mf.wh$CI_ID_t0)


#shuffle the data to avoid soring order
my.data<-mf.wh[sample(nrow(mf.wh)),]

y<-which(mf.wh$VTE_t1 == 1)

keep_row<-c(y,(y-1),(y-2),(y+1))
keep_row<-keep_row[-which(keep_row<0)]

mf.wh<-mf.wh[keep_row,]