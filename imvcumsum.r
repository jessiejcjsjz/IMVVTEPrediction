rm(list=ls())


#this is the cumsum for vte model data
load('feature_selection_whole_imv.rda')

#the object is feature2_featured , 2 million records, 600 columns

mf.wh<-mf2_featured
# mf.wh<-mf.wh[,-3] #fine date is 0 , not important

#turn all 
#mf.wh<-ddply(mf.wh,.(CI_ID_t0),mutate,c_vte_t0 = cumsum(VTE_t0))
#mf.wh$c_vte_t0<-ave(mf.wh$VTE_t0,mf.wh$CI_ID,FUN = cumsum)

#rest of the variable ,cumsum them, besied the y
for(i in 3:(ncol(mf.wh)-1)){
	names<-c()
	var<-colnames(mf.wh)[i]
	vark<-paste('c',var,sep = '_')
	v<-ave(mf.wh[,i],mf.wh$CI_ID_t0_t0,FUN = cumsum)
	mf.wh<-cbind(mf.wh,v)
	colnames(mf.wh)[ncol(mf.wh)]<-vark
}

save(mf.wh,file = 'imv_health_cum0419.rda')