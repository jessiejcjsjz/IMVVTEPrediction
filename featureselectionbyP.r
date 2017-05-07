rm(list=ls())
library(data.table)
library(plyr)
setwd("/zfsauton/project/highmark/data/longoutput")
load('feature_selection_whole.rda')

args <- as.numeric(commandArgs(trailingOnly=TRUE))


if (args == 1) { test <- c(1:10)}
if (args == 2) { test <- c(11:20)}
if (args == 3) { test <- c(21:30)}
if (args == 4) { test <- c(31:40)}
if (args == 5) { test <- c(41:50)}
if (args == 6) { test <- c(51:60)}
if (args == 7) { test <- c(61:70)}
if (args == 8) { test <- c(71:80)}
if (args == 9) { test <- c(81:90)}
if (args == 10) { test <- c(91:100)}


feature_selection2 <- 
	sapply(test,function(i){
		print(i)
		set.seed(i); train <- sample(1:nrow(mf2),nrow(mf2)*0.5)
		train_data <- mf2_featured[train,]
		apply(train_data[,-ncol(train_data)],2,function(x){t.test(x~train_data$VTE_t1)$p.value})
	})

filename <- paste0('my_vte_select_sub',args,'.rda')

save.image(filename)


# #run parel
# Rscript featureselectionbyP.r 1 >fsp1.log &
# Rscript featureselectionbyP.r 2 >fsp2.log &
# Rscript featureselectionbyP.r 3 >fsp3.log &
# Rscript featureselectionbyP.r 4 >fsp4.log &
# Rscript featureselectionbyP.r 5 >fsp5.log &
# Rscript featureselectionbyP.r 6 >fsp6.log &
# Rscript featureselectionbyP.r 7 >fsp7.log &
# Rscript featureselectionbyP.r 8 >fsp8.log &
# Rscript featureselectionbyP.r 9 >fsp9.log &
# Rscript featureselectionbyP.r 10 >fsp10.log &