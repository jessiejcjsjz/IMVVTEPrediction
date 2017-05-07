#http://stackoverflow.com/questions/24626300/loading-data-from-rdata-files-into-a-single-data-table


#first get the list of files
fileList <- list.files(getwd(), pattern="\\.rda$", full.names=TRUE)

#create a vector list that save all the feature selection2 result
mylist <-vector(length(fileList), mode = "list")

#load all the RData files and created objests for it
for(i in 1:length(fileList)){
	load(fileList[i])
	mylist[[i]] <- k
	#print which file has been combined
	print(fileList[i])
}

my.rbind<-do.call(rbind,mylist)

print("conbined")

save(my.rbind, file = "myrbind.rda")