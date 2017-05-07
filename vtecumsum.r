#this is the cumsum for vte model data
#load('vte_selected_nocum0405.rda')

#the object is mf.wh
mf.wh<-mf.wh[,-3] #fine date is 0 , not important

#turn all 
# mf.wh<-ddply(mf.wh,.(CI_ID_t0),mutate,c_vte_t0 = cumsum(VTE_t0))
mf.wh$c_vte_t0<-ave(mf.wh$VTE_t0,mf.wh$CI_ID,FUN = cumsum)

#rest of the variable ,cumsum them
for(i in 6:103){
	names<-c()
	var<-colnames(mf.wh)[i]
	vark<-paste('c',var,sep = '_')
	v<-ave(mf.wh[,i],mf.wh$CI_ID, FUN = cumsum)
	mf.wh<-cbind(mf.wh,v)
	colnames(mf.wh)[ncol(mf.wh)]<-vark
}

save(mf.wh,file = 'vte_select_cum0405.rda')



######new

k<-ncol(mf.wh)-1

for(i in 3:k){
	names<-c()
	var<-colnames(mf.wh)[i]
	vark<-paste('c',var,sep = '_')
	v<-ave(mf.wh[,i],mf.wh$CI_ID, FUN = cumsum)
	mf.wh<-cbind(mf.wh,v)
	colnames(mf.wh)[ncol(mf.wh)]<-vark
}

save(mf.wh,file = 'vte_select_cum0405.rda')




#####
#c
######