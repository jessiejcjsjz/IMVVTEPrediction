	rm(list=ls())
library(data.table)
library(plyr)
setwd("/zfsauton/project/highmark/data/longoutput")
load('mf_vte.rda')


#kick out all the variable with no variation
mf2 <- as.data.frame(mf)[,as.numeric(which(apply(mf,2,var)>0))]

print(Sys.time())
#kick out all the variable with no variation
i <- 0
feature_selection <- apply(mf2,2,function(x){
	print(i<<-i+1)
	try(t.test(x~mf2$IMV_t1))
}) #Test the correlation between all the t0 variable with vte_t1

print(Sys.time())

fs_out <- c(sapply(feature_selection[-length(feature_selection)],function(x) x$p.value),0)
mf2_featured <- mf2[,fs_out<=(0.05/length(feature_selection)),drop=F]

save.image('feature_selection_whole.rda')