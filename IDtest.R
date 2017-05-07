#my.data<-import.csv('/zfsauton/project/highmark/data/part.Medical_201509.csv')

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))}

my.data<-import.csv('part.Medical_201509_p.csv')

my.data<-my.data[order(my.data$CI_ID),]

head(my.data,30)


mywords<-c("V1046","V4589",'85426')
letters <- grepl("^[A-Za-z]", mywords)


test<-re$value[grep('DIAG',re$variable)]
diag<-substring(test,1,3)

# code<-c("")
# 
# 
# k<-expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
#             sex = c("Male","Female"))
# 
code<-expand.grid(c("A","B"),0:99)
code <- paste(code[,1],code[,2],sep = '')


