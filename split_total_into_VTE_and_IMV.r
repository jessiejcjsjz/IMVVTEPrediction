#Modeling with Markov Chain
#here prepared 2 dataset
#d0 is the claim information for same CI_ID(member) but the previous record info
#d1 is the claim information for same CI_ID(member) with current record info
#what I hope is to use d0 to predict if d1 will have IMV or VTE
#testfile_er.rda is the dataset prepared for IMV = 1
#testfile_yi.rda is the dataset prepared for VTE = 1
#idlist[[1]] is list of the CI_ID ,Claim_No whose VTE = 1
#idlist[[2]] is list of the CI_ID ,Claim_No whose IMV = 1

rm(list=ls())

#Setup

library(data.table)
library(dplyr)
library(slam)

#import data

setwd("/zfsauton/project/highmark/data/longoutput")
load('modelfile_20170402.rda')
#get the claim no and ci id out from the rowid previous stored
claim <- t(sapply(strsplit(rowidlist,' '),as.numeric))

load('control_sample_12345.rda')

#subdata for Y1
#if i = 1, then the information pulled out from idlist is those VTE = 1
#if i = 2, then the information pulled out from idlist is those IMV = 1
i <- 1
#get subdata for those samples with Yi
datai <- modelfile[which(claim[,1]%in%as.numeric(idlist[[i]])),]
datai <- as.data.table(as.matrix(datai))
colnames(datai) <- colidlist

#Claimi and ID information for Yi, coding T
claimi <- claim[claim[,1]%in%as.numeric(idlist[[i]]),]
claimi <- cbind(claimi,ordert1=1)
#need to order the cliam by id
for(k in 2:nrow(claimi)){
	if(claimi[k,1]==claimi[k-1,1]){
		claimi[k,3] <- claimi[k-1,3]+1
	}
}
claimi <- cbind(claimi,ordert0=claimi[,3]-1)

#2 identifiers, t1 stands for current claim and id
# t0 stands for the previous claim and id
t0 <- paste(claimi[,1],claimi[,4])
t1 <- paste(claimi[,1],claimi[,3])

#get xinfo subdata
xcode <- paste(x_info[,1],x_info[,2])
#find where to get information from x_info to merge
claimi <- cbind(claimi,getxinfo=paste(claimi[,1],claimi[,2])%in%xcode)

xcode <- x_info[match(paste(claimi[,1],claimi[,2]),xcode),]
data1 <- data.table(xcode,datai)
data0 <- data1[match(t0,t1)]
colnames(data0) <- paste0(colnames(data0),'_t0')


 #t0 could be matched and xinfo could be found
rowsel <- (!is.na(match(t0,t1)))&(!is.na(claimi[,5]))
data1 <- data1[which(rowsel)]
data0 <- data0[which(rowsel)]

#testfile_er.rda is the dataset prepared for IMV = 1
#testfile_yi.rda is the dataset prepared for VTE = 1

save(data0,data1,file='testfile_yi.rda')
#save(data0,data1,file='testfile_er.rda')


#modeling file

# rm(list=ls())
# load('testfile_yi.rda')

# vte1 <- data1$VTE=='1'; vte1[is.na(vte1)] <- 0
# vte0 <- data0$VTE=='1'; vte0[is.na(vte0)] <- 0


# #just use previous vte0 record to predict next vte record
# test <- MASS::lda(vte1~vte0)
# table(predict(test)$class,vte1)

# #try use not only previous vte0 but also use previous historical claim info
# data2<-cbind(data0,vte_t1 = vte1)
# test1 <- MASS::lda(vte_t1~., data = data2)

# #check if there is one level factors or not
# for(q in 1:ncol(data2)){
# 		print(is.factor(data2[,q]))
# 	}


# rm(list=ls())
# load('testfile_er.rda')

# imv1 <- data1$IMV=='1'; imv1[is.na(imv1)] <- 0
# imv0 <- data0$IMV=='1'; imv0[is.na(imv0)] <- 0


# #just use previous vte0 record to predict next vte record
# test <- MASS::lda(imv1~imv0)
# table(predict(test)$class,imv1)


# #
# data0$VTE_t0 <- ifelse(is.na(as.numeric(data0$VTE_t0)),0,1)
# data_cumsum <- ddply(data0,.(CI_ID_t0),h415=cumsum(DIAG_CD_415),h451=cumsum(DIAG_CD_451),h453=cumsum(DIAG_CD_453),hvte=cumsum(VTE))
# data0$VTE_t1 <- ifelse(is.na(as.numeric(data1$VTE)),0,1))


IMV_t1<-ifelse(is.na(as.numeric(data1$IMV)),0,1)
my_imv<-cbind(data0,IMV_t1)