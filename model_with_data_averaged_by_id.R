rm(list=ls())

#Setup

library(data.table)
library(dplyr)
library(slam)

#import data

setwd("/zfsauton/project/highmark/data/longoutput")
load('modelfile_20170402.rda')

claim <- t(sapply(strsplit(rowidlist,' '),as.numeric))
id <- unique(claim[,1])

load('control_sample_12345.rda')
y <- unique(unlist(idlist[c(1,2,4)]))
y <- cbind(as.numeric(y),y%in%idlist[[1]],y%in%idlist[[2]],y%in%idlist[[4]])

data <- as.data.table(as.matrix(modelfile))
colnames(data) <- colidlist
data$id <- claim[,1]

syntax <- paste(
	"data %>% group_by(id) %>% summarise(",
		paste(paste(colidlist,
					'=mean(',
					colidlist,')'),
				collapse=',')
		,")")
data2 <- eval(parse(text=syntax))