
rm(list=ls())

#args <- as.numeric(commandArgs(trailingOnly=TRUE))
#if(length(args)==0){args<-1}

#Setup

library(data.table)
library(dplyr)

setwd('/zfsauton/project/highmark/data/longoutput')
#kname <- dir(pattern='output_out.Medical')[args]

#get idlist
idname <- dir(pattern='idlist_out.Medical')
idlist <- lapply(idname,function(x){
				load(x)
				return(idlist)
			})
vte <- unique(unlist(lapply(idlist,function(x){x[[1]]})))
imv <- unique(unlist(lapply(idlist,function(x){x[[2]]})))
control <- unique(unlist(lapply(idlist,function(x) x[[3]])))
control <- control[!control%in%c(vte,imv)]

count_disease <- length(unique(c(vte,imv)))
set.seed(12345); control_sample <- sample(control,count_disease*10)

idlist <- list(vte,imv,control,control_sample,seed=12345)
save(idlist,file='control_sample_12345.rda')




