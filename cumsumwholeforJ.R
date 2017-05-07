rm(list = ls())

library(data.table)
library(dplyr)
library(slam)

# #this is the cumsum for modelfile_20170402.rda
load('modelfile_20170402.rda')

print('dataloaded')

claim <- t(sapply(strsplit(rowidlist,' '),as.numeric))

xcode<-paste(x_info[,1],x_info[,2])
#get xinfo subdata
xcode <- x_info[match(rowidlist,xcode),]

print('x_info geted')

#turn sparse matrix as the dataframe
datai <- as.data.frame(as.data.table(as.matrix(modelfile)))
colnames(datai) <- colidlist

#combine CI_ID and Claim_ID and datai information prepare for cumsum
data_c <- data.table(claim,datai)
colnames(data_c)[1]<-'CI_ID'
colnames(data_c)[2]<-'Claim_NO'

print('dataframe turned')

#create a data_cc to store the cumsum data for variables
data_cc<-data.frame(CI_ID = data_c[,1], Claim_NO = data_c[,2])

# #rest of the variable ,cumsum them
data_c<-as.data.frame(data_c)

# #for each CI ID, calculate cumsum and added to data_cc
for(i in 3:ncol(data_c)){
	v<-ave(data_c[,i],data_c$CI_ID, FUN = cumsum)
	data_cc<-cbind(data_cc,v)
	print(i)
}

colnames(data_cc)<-paste("c",colnames(data_c), sep = "_")

print("start combining")

#add x_info to data_cc
data_cc<-cbind(xcode,data_cc[3:ncol(data_cc)])

print("start saving")

save(data_cc,file = 'modelfile_cumsum.rda')

print("done")

