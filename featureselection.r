rm(list=ls())
library(data.table)
library(plyr)
setwd("/zfsauton/project/highmark/data/longoutput")
load('mf_vte.rda')


#kick out all the variable with no variation
mf2 <- as.data.frame(mf)[,as.numeric(which(apply(mf,2,var)>0))] 

i <- 0
feature_selection <- apply(mf2,2,function(x){
	print(i<<-i+1)
	try(t.test(x~mf2$VTE_t1))
}) #Test the correlation between all the t0 variable with vte_t1

fs_out <- c(sapply(feature_selection[-length(feature_selection)],function(x) x$p.value),0)
mf2_featured <- mf2[,fs_out<=(0.05/length(feature_selection)),drop=F]
#test <- MASS::lda(VTE_t1~.,data=mf2_featured)
#table(predict(test)$class,mf2_featured$VTE_t1)

feature_selection2 <- 
	sapply(1:100,function(i){
		print(i)
		set.seed(i); train <- sample(1:nrow(mf2),nrow(mf2)*0.5)
		train_data <- mf2_featured[train,]
		apply(train_data[,-ncol(train_data)],2,function(x){t.test(x~train_data$VTE_t1)$p.value})
	})

#select those 80% significant
fs2_out <- c(rowMeans(feature_selection2 <= (0.05/nrow(feature_selection2)))>=0.8,T) 
save.image('mf_vte_model.rda')

# fs2_out <- c(rowMeans(fs100 <= (0.05/nrow(fs100)))>=0.8,T)

# later
# with 102 selected in fs2_out
# mf3<- mf2_featured[,which(fs2_out == TRUE)]ls
