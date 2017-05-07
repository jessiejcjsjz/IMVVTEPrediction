
rm(list=ls())
gc()

###################
#library we need 
####################
library(reshape2)



load("myrbindsample.rda")

###################
#wide
###################

re.final = reshape(my.rbind, v.names = "count", timevar = "Var_New",
                                            idvar =c("CI_ID",
                                            "Claim_No",
                                            "EACL_NO"),
                                            direction = 'wide')
###################
#sort by ID by Date
###################
re.final = dplyr::arrange(re.final,CI_ID,Date)

#unique claimid needed , ignore the EACL_NO
sample_uniq <- names(which(table(re.final$Claim_No)==1))
re.final<-re.final[re.final$Claim_No%in%sample_uniq,]

###################
save(re.final, file = "withoutCum.rda")


