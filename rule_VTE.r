#cluster first
#remove the label
library(dplyr)
library(data.table)
library(slam)


# # mf.wh<-mf.wh[,grep('VTE_t1',colnames(mf.wh))]
# # test<-as.data.frame(mf.wh%>%group_by(CI_ID_t0)%>% summarize_each(funs(mean)))



# load('modelfile_20170402.rda')
# claim <- t(sapply(strsplit(rowidlist,' '),as.numeric))

# #combine id vs data need to be averaged
# need.avg <- cbind(claim[1],as.data.table(as.matrix(modelfile)))
# need.avg <-as.data.frame(need.avg%>%group_by(CI_ID_t0)%>% summarize_each(funs(mean)))

#decide to use tree

load('vte_select_cumsum0405.rda')
 library(rpart)
> w<-mf.wh[,-1:-2]
> fit<-rpart(VTE_t1~., method = 'class', data = w)