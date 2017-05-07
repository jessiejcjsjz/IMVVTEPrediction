library(GenABEL)

#function to change each column to normal distribution
#scale_ivn <- function(x){apply(x,2,rntransform)}



qpca <- function(A,rank=0){
  #first scale
  A <- scale(A)
  A.svd <- svd(A)


  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-10]
  r <- length(d)
  prop <- d^2; prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
  return(rlt)
}

# A <- t(expr)
# system.time(A.pca <- qpca(A))
# system.time(A.qpca <- qpca(A,rank=which(A.pca$prop>=0.9)[1]))
# x <- A.qpca$X[,1:which(A.qpca$prop>=0.9)[1],drop=F]
# system.time(xdist <- dist(x))
# save(xdist,file='xdist.rda')


qpca2 <- function(A){ 
 A.pca <- qpca(A) 
 A.qpca <- qpca(A,rank=which(A.pca$prop>=0.9)[1]) 
 #A.qpca$X[,1:which(A.qpca$prop>=0.9)[1],drop=F] 
 A.qpca$Z[,1:which(A.qpca$prop>=0.9)[1],drop=F]
 } 


######################
#below are the records
######################

#rownames
rownames<-mf.wh[,1:2]


#x
mf.wh<-mf.wh[,-1:-2]

#y
y<-mf.wh$VTE_t1

#x
mf.wh<-mf.wh[,-grep('VTE_t1',colnames(mf.wh))]

test<-qpca2(mf.wh)

######################
#below are the records
######################

load('imv_selectbig_cum_sampled_0407.rda')

#rownames
rownames<-mf.wh[,1:2]


#x
mf.wh<-mf.wh[,-1:-8]

#y
y<-mf.wh$IMV_t1

#x
mf.wh<-mf.wh[,-grep('IMV_t1',colnames(mf.wh))]
#kick out constant
mf.wh<-mf.wh[,as.numeric(which(apply(mf.wh,2,var)>0))] 



#begin PCA
test<-qpca2(mf.wh)

test.final<-cbind(rownames,test,y)
colnames(test.final)[ncol(test.final)]<-'IMV_t1'