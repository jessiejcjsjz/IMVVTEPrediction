

##################################below is the one for VTE ###########################
load('vte_select_cum0405.rda')
#mf.wh
# dim(mf.wh)
# [1] 265986    203
# start no 105 : 203, there's cumsum variable

for(i in 105:203){
	var<-paste("b",colnames(mf.wh)[i],sep = '_')
	new<-ifelse(mf.wh[i]>0,1,0)
	colnames(new)<-var
	mf.wh<-cbind(mf.wh,new)
}


save(mf.wh,file = 'vte_select_cum0405_binary0410.rda')

#check 1 b_c_DIAG_CD_780_t0, pass
#check 2 > sum(mf.wh[,204:ncol(mf.wh)]>1) == 0 , pass


##################################below is the one for VTE ###########################
load('imv_selectbig_cum0405.rda')
# > dim(mf.wh)
# [1] 1998548    1117

load('imv_selectbig_cum_sampled_0407.rda')

for(i in 565:ncol(mf.wh)){
	var<-paste("b",colnames(mf.wh)[i],sep = '_')
	new<-ifelse(mf.wh[i]>0,1,0)
	colnames(new)<-var
	mf.wh<-cbind(mf.wh,new)
}

save(mf.wh,file = 'imv_selectbig_cum_sample_binary0412.rda')


