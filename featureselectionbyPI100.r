rm(list=ls())
library(data.table)
library(plyr)
#setwd("/zfsauton/project/highmark/data/longoutput/featureselectresult")
load('feature_selection_whole_imv.rda')

args <- as.numeric(commandArgs(trailingOnly=TRUE))

test<-args

feature_selection2 <- 
	sapply(test,function(i){
		set.seed(i); train <- sample(1:nrow(mf2),nrow(mf2)*0.5)
		train_data <- mf2_featured[train,]
		apply(train_data[,-ncol(train_data)],2,function(x){t.test(x~train_data$IMV_t1)$p.value}
	})

set.seed(i); train<-sample(1:nrow(mf2), nrow(mf2)*0.5)
train_data <- mf2_featured[train,]


filename <- paste0('my_imv_select_sub',args,'.rda')

save.image(filename)


# # #run parel
# Rscript featureselectionbyPI.r 1 >fsp1.log &
# Rscript featureselectionbyPI.r 2 >fsp2.log &
# Rscript featureselectionbyPI.r 3 >fsp3.log &
# Rscript featureselectionbyPI.r 4 >fsp4.log &
# Rscript featureselectionbyPI.r 5 >fsp5.log &
# Rscript featureselectionbyPI.r 6 >fsp6.log &
# Rscript featureselectionbyPI.r 7 >fsp7.log &
# Rscript featureselectionbyPI.r 8 >fsp8.log &
# Rscript featureselectionbyPI.r 9 >fsp9.log &
# Rscript featureselectionbyPI.r 10 >fsp10.log &

# > a<-data.frame( ID = rep(1:2,3), group = rep(c("a","b"),3), n1 = rep(0:1,3),n2 = rep(1:2,3))
# > a
#   ID group n1 n2
# 1  1     a  0  1
# 2  2     b  1  2
# 3  1     a  0  1
# 4  2     b  1  2
# 5  1     a  0  1
# 6  2     b  1  2

# > for(i in 3 : ncol(a)){
# +   a[,i]<-ave(a[,i],a$ID, FUN = cumsum)
# + }
# > 
# > a
#   ID group n1 n2
# 1  1     a  0  1
# 2  2     b  1  2
# 3  1     a  0  2
# 4  2     b  2  4
# 5  1     a  0  3
# 6  2     b  3  6