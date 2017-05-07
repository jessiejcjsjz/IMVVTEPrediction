library(foreach)
library(reshape2)
library(tidyr)

getCodeList <- function(claim.df){ #specific for A data
  claim.melt = melt(claim.df,id.vars=c(colnames(claim.df)[c(1:8,95)]))
  dxproc.index = c(grep('proc',claim.melt$variable),grep('dx',claim.melt$variable),
                   grep('pdate',claim.melt$variable),grep('drg',claim.melt$variable))
  dxproc.vars = unique(claim.melt$variable[dxproc.index])
  claim.melt = claim.melt[which(!(claim.melt$variable %in% dxproc.vars & claim.melt$value %in% c("-2",NA))),]
  claim.melt = unique(claim.melt)
  claim.melt$variable = as.character(claim.melt$variable)
  claim.melt$variable[which(substr(claim.melt$variable,1,2)=='dx')]='dx'
  claim.melt$variable[which(substr(claim.melt$variable,1,4)=='proc')]='proc'
  claim.melt$variable[which(substr(claim.melt$variable,1,5)=='pdate')]='pdate'  
  return(claim.melt)
  
}

claim = get(load('Claims_ICD.RData'))

# data transform: specific to A dataset
date.cols=grep('date',colnames(claim))
date.cols = c(3,4,5,date.cols)
for(i in date.cols){
  claim[,i] = as.Date(claim[,i],'%m/%d/%Y')
}

# data cleaning: specific to A dataset
claim = claim[which(!is.na(claim$admit)),]
claim.id = paste0(claim$studyid,claim$admit)
claim = cbind(claim.id,claim)
claim$this.age = floor(as.numeric(claim$admit-claim$dob)/365.25)
claim = claim[order(claim$studyid,claim$admit),]
#claim.info = claim[,c(1:8,95)] 


# convert raw data into sparse format
time1 <-Sys.time()
claim.melt = getCodeList(claim) 
time2<-Sys.time() # 36 seconds for claim data of 222,202 rows * 93 variables, melt part (first row of getCodeList()) takes 3.087 seconds
time2-time1

claim.melt$dummyVar = paste0(claim.melt$variable,'_',claim.melt$value)

dummyVar.list = unique(claim.melt$dummyVar)
claim.toWide = claim.melt[,c(1,2,12)]

claim.toWide$obs=1

table(substr(unique(claim.toWide$dummyVar),1,3)) # check the distribution of variables
claim.toWide = claim.toWide[which( !substr(claim.toWide$dummyVar,1,2)  %in% c('cc','fc','pd')),] # specific to A datasets
claim.toWide$claim.id = as.character(claim.toWide$claim.id) # to avoid factor data type

# reshape is too slow and consumes too much memory 
# time1 <-Sys.time()
# claim.wide = reshape(data=claim.toWide,idvar=c('claim.id','studyid'),timevar='dummyVar',v.names='obs',direction='wide')
# time2<-Sys.time()

# separate sparse long format into several subsets, and convert into individual matrix to avoid out-of-memory
dx.claim = claim.toWide[which(substr(claim.toWide$dummyVar,1,2)=='dx'),]
adm.claim = claim.toWide[which(substr(claim.toWide$dummyVar,1,3)=='adm'),]
drg.claim = claim.toWide[which(substr(claim.toWide$dummyVar,1,3)=='drg'),]
pro.claim = claim.toWide[which(substr(claim.toWide$dummyVar,1,3)=='pro'),]
res.claim = claim.toWide[which(!substr(claim.toWide$dummyVar,1,3) %in% c('dx_','adm','drg','pro')),]

# two alternatives to convert sparse long matrix into wide matrix
# computation efficiency varies for different sizes of datasets
# reference: http://www.kimberlycoffey.com/blog/2015/5/18/e3oehe6mn0y16kmw9p2azq23cz78qy

# option 1: spread from tidyr
# output is a data frame
time1 <-Sys.time()
dx.wide = spread(dx.claim[,-2],key='dummyVar',value='obs') # from library(tidyr)
time2<-Sys.time() # 1.437 mins to convert a dx.claim into 112930 * 12387 matrix on computing nodes 

# option 2: make sure to unique(df) before converting, otherwise double-counted
# faster, and output is a matrix
time1 <-Sys.time()
dx.wide = do.call(cbind, lapply(dx.claim[3], function(x) table(dx.claim[[1]], x)))
time2 <- Sys.time()# 4.5232 seconds to convert a matrix into 112930 * 12397 matrix on computing nodes

adm.wide = do.call(cbind, lapply(adm.claim[3], function(x) table(adm.claim[[1]], x)))
drg.wide = do.call(cbind, lapply(drg.claim[3], function(x) table(drg.claim[[1]], x)))
pro.wide = do.call(cbind, lapply(pro.claim[3], function(x) table(pro.claim[[1]], x)))
res.wide = do.call(cbind, lapply(res.claim[3], function(x) table(res.claim[[1]], x)))
# 19.05 seconds to finish four convertions

# merge matrices into one
agg.wide = cbind(adm.wide, drg.wide[match(rownames(adm.wide), rownames(drg.wide)),],
                 dx.wide[match(rownames(adm.wide), rownames(dx.wide)),]) # more to be added on
agg.wide[is.na(agg.wide)]=0 # fill the NA value with zero

# extra code to add class label for each row
agg.wide = cbind(agg.wide,label=ifelse(row.names(agg.wide) %in% pos.claim.ids),1,0))

save(agg.wide,file='**.RData')
#or
write.csv(agg.wide[,-1],'***.csv',row.names=F,quote=F)

