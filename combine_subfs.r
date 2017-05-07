rm(list=ls())
#combine fs result

#first get the list of files
fileList <- list.files(getwd(), pattern="\\.rda$", full.names=TRUE)

#create a vector list that save all the rdata objects
mylist <-vector(length(fileList), mode = "list")

print(Sys.time())

#load all the RData files and created objests for it
for(i in 1:length(fileList)){
	load(fileList[i])
	mylist[[i]] <- feature_selection2
	#print which file has been combined
	print(fileList[i])
}

print(Sys.time())

#colbind all the feature selection result
fs.cbind<-do.call(cbind,mylist)

print("conbined")

save(fs.cbind, file = "featureselect100times.rda")