
rm(list=ls())
gc()

###################
#library we need 
####################
library(slam)



write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

print("start")

load("myrbindsample.rda")

print("loaded")

n<-ncol(my.rbind)
# x<-my.rbind[,c(1,2,3,(n-3),(n-1),n)]
# my.rbind.NT<-my.rbind[,c(-1,-2,-3,-(n-3),-(n-1),-n)]


#unique claimid needed , ignore the EACL_NO
sample_uniq <- names(which(table(my.rbind$Claim_No)==1))
my.rbind<-my.rbind[my.rbind$Claim_No%in%sample_uniq,]

print("duplicated deleted")

rows<- do.call(paste,c(my.rbind[,1:(n-2)], sep = ' '))
code <-cbind(row= rows,col= my.rbind$Var_New)


#generate dimnames of the final matrix
rowid <- unique(code[,1])
colid <- unique(code[,2])
row2tag <- match(code[,1],rowid)
col2tag <- match(code[,2],colid)

print("start to create matrix")

#
rlt <- as.matrix(slam::simple_triplet_matrix(row2tag,col2tag,x$count))
dimnames(rlt) <- list(rowid,colid)
rlt<-as.data.frame(rlt)
names<-rownames(rlt)
name.c<-stringr::str_split_fixed(names, " ", (n-2))
colnames(name.c)<-c(colnames(my.rbind)[1:(n-2)])
rlt.new<-cbind(name.c,rlt)


#write.csv(rlt.new,"test09slam.csv")


# ###################
# #wide
# ###################
# re.final = reshape(my.rbind, v.names = "count", timevar = "Var_New",
#                                             idvar =c("CI_ID",
#                                             "Claim_No",
#                                             "EACL_NO"),
#                                             direction = 'wide')
###################
#sort by ID by Date
###################

print("start to sort")
re.final = dplyr::arrange(rlt.new,CI_ID,Date)



###################
print("start to save")

save(re.final, file = "withoutCum.rda")


