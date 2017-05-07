
rm(list=ls())
gc()

args <- as.numeric(commandArgs(trailingOnly=TRUE))
arg1 <- args[1] #identify which file to read
if(length(args)==1){
    arg2 <- -1
} else {
    arg2 <- args[2]
}
filename <- dir(pattern='part.Medical_')[arg1]
print(filename)

##########################
#library required
##########################
library(data.table)
library(dplyr)
library(stringr)
library(reshape)

print("start")

# utility function for import from csv file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE,nrow = 100))
}
import.csv2 <- function(filename,n) {
  return(read.csv(filename, sep = ",", header = TRUE,nrow = n))
}


#############################
#read data
#############################

#sample = import.csv("part.Medical_201509.csv")
sample <- import.csv2(filename,arg2)

#############################
#data processing
#############################
print("processing")
#	a. Delete the duplicated Claim_no information. There's no difference between Claim_no , besides the EACL_NO (payment difference)
sample_uniq <- names(which(table(sample$Claim_No)==1))
sample<-sample[sample$Claim_No%in%sample_uniq,]
# b. Delte those without CI_ID
sample<-sample[!is.na(sample$CI_ID),]

#create labels for the final prediction
sample <- data.frame(lapply(sample, as.character), stringsAsFactors=FALSE)
IMV<-c('9604','9671','9672','51881')
VTE<-c('4151','41511','45111','4512','45181','4538')

sample$IMV <-c('')
sample$VTE <-c('')

#CLASS LABLE
for(j in 1:nrow(sample)){
  for(i in grep('DIAG_CD', colnames(sample))){
    if( sample[j,i] %in% IMV ){sample$IMV[j] = '1'}
    if( sample[j,i] %in% VTE & sample$PRI_DIAG_CD != '6731' ){sample$VTE[j] = '1'}
  }
}

print("label created")

#modify the date so that they are real date and can be sorted, change all those ends with _DT to date
for(i in grep('_DT',colnames(sample))){
  sample[,i]<-as.Date(as.character(sample[,i]),"%d%b%Y")
}

#Date which is going to be used as claim date
sample$Date<-with(sample, ifelse(sample$EACT_CD == 'I', sample$EAC_DCG_DT, sample$ICRD_DT))
sample$Date<-as.Date(sample$Date, origin = "1970-01-01")


#Create year old, to the floor of the age
sample$CUR_AGE<-with(sample,ifelse(sample$EACT_CD == 'I',
                                   round((sample$EAC_DCG_DT - sample$EACM_BIR_DT)/365,-1),
                                   round((sample$ICRD_DT - sample$EACM_BIR_DT)/365,-1)))
print("age created")

#select 2 years data
sample<-sample[sample$Date > as.Date("01JAN2014","%d%b%Y"),]

print("time 2 years chosen")

#start to reashape and code, all needs to be character
sample <- data.frame(lapply(sample, as.character), stringsAsFactors=FALSE)
re<-melt(sample, id = c("CI_ID",
                        "Claim_No",
                        "EACL_NO",
                        "Performing_Prov_NPI",
                        "EAC_ADMM_DT",
                        "EAC_DCG_DT",
                        "ICRD_DT",
                        "EACNSPS_CD",
                        "EACM_BIR_DT",
                        "MBR_EAG_CD",
                        "CMN_EACDRG_CD",
                        "EACDS_CD",
                        "EACPV_DRG_CD",
                        "EACPV_DRG_WT",
                        "EACPV_GPR_VSN_ID",
                        "CUR_CMN_EACDRG_CD",
                        "EAC_FNLN_DT",
                        "IMV",
                        "VTE",
                        "Date",
                        "CUR_AGE"
                        ))

#change variablenames first
re <- data.frame(lapply(re, as.character), stringsAsFactors=FALSE)
re$variable[grep('EACP_OTH_CLM_CD',re$variable)]<-'EACP_CLM_CD'
re$variable[grep('EACP_PRN_CLM_CD',re$variable)]<-'EACP_CLM_CD'
re$variable[grep('DIAG_CD',re$variable)]<-'DIAG_CD'

print("create new arity")
re$Arity = ''
#RECODE BETOS_CD
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1A"]="BED"
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1B"]="OXY"
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1C"]="WC"
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1D"]="OTHERDME"
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1E"]="PO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1F"]="DAT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="D1G"]="STI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I1A"]="STI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I1B"]="STI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I1C"]="STI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I1D"]="STI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I1E"]="STI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I1F"]="AI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I2A"]="AI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I2B"]="AI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I2C"]="AI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I2D"]="EU"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I3A"]="EU"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I3B"]="EU"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I3C"]="EU"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I3D"]="EU"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I3E"]="EU"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I3F"]="IP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I4A"]="IP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="I4B"]="OV"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M1A"]="OV"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M1B"]="HVT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M2A"]="HVT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M2B"]="HVT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M2C"]="ER"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M3"]="HV"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M4A"]="NHV"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M4B"]="SP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M5A"]="SP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M5B"]="SP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M5C"]="SP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M5D"]="CO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="M6"]="AM"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1A"]="CHI"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1B"]="ENT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1C"]="CHE"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1D"]="ODRUG"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1E"]="HS"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1F"]="IV"
re$Arity[re$variable == "BETOS_CD"  & re$value =="O1G"]="ANE"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P0"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1A"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1B"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1C"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1D"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1E"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1F"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P1G"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P2A"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P2B"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P2C"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P2D"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P2E"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P2F"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P3A"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P3B"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P3C"]="MAP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P3D"]="EP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P4A"]="EP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P4B"]="EP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P4C"]="EP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P4D"]="EP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P4E"]="AP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P5A"]="AP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P5B"]="AP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P5C"]="AP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P5D"]="AP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P5E"]="MIP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P6A"]="MIP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P6B"]="MIP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P6C"]="MIP"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P6D"]="ON"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P7A"]="ON"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P7B"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8A"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8B"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8C"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8D"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8E"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8F"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8G"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8H"]="ENDO"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P8I"]="DIA"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P9A"]="DIA"
re$Arity[re$variable == "BETOS_CD"  & re$value =="P9B"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1A"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1B"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1C"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1D"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1E"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1F"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1G"]="LAB"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T1H"]="OTHERT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T2A"]="OTHERT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T2B"]="OTHERT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T2C"]="OTHERT"
re$Arity[re$variable == "BETOS_CD"  & re$value =="T2D"]="OTHER"
re$Arity[re$variable == "BETOS_CD"  & re$value =="Y1"]="OTHER"
re$Arity[re$variable == "BETOS_CD"  & re$value =="Y2"]="LACC"
re$Arity[re$variable == "BETOS_CD"  & re$value =="Z1"]="UC"
re$Arity[re$variable == "BETOS_CD"  & re$value =="Z2"]="UC"

print("Arity created")

#RECODE DIAGNOSE
re$Arity[grep("DIAG",re$variable)] = substring(re$value[grep("DIAG",re$variable)],1,3)
re$Arity[grep("EACP_CLM_CD",re$variable)] = substring(re$value[grep("EACP_CLM_CD",re$variable)],1,3)
re$Arity[grep("EA_NATL_HCPCS",re$variable)] = substring(re$value[grep("EA_NATL_HCPCS",re$variable)],1,1)

print("diganose done")

#Handle rest
re$Arity[re$variable == 'EACT_CD'] = re$value[re$variable == 'EACT_CD']
re$Arity[re$variable == 'CMN_EACDRG_VSN_NO'] = re$value[re$variable == 'CMN_EACDRG_VSN_NO']
re$Arity[re$variable == 'EACAS_CD'] = re$value[re$variable == 'EACAS_CD']
re$Arity[re$variable == 'CUR_CMN_EACDRG_VSN_NO'] = re$value[re$variable == 'CUR_CMN_EACDRG_VSN_NO']

print("rest arity done")

#get the newvar
re$Var_New = paste(re$variable,re$Arity,sep="_")
re$V = 1


print("create re.sub")

#final
re.sub = re[,-22:-24]
re.sub = re.sub[,-ncol(re.sub)]


k<-re.sub %>% group_by(CI_ID,
                    Claim_No,
                    EACL_NO,
                    Performing_Prov_NPI,
                    EAC_ADMM_DT,
                    EAC_DCG_DT,
                    ICRD_DT,
                    EACNSPS_CD,
                    EACM_BIR_DT,
                    MBR_EAG_CD,
                    CMN_EACDRG_CD,
                    EACDS_CD,
                    EACPV_DRG_CD,
                    EACPV_DRG_WT,
                    EACPV_GPR_VSN_ID,
                    CUR_CMN_EACDRG_CD,
                    EAC_FNLN_DT,
                    IMV,
                    VTE,
                    Date,
                    CUR_AGE,
                    Var_New) %>% summarise(count=n())
#change k into dataframe and k is what i need from each file
k<-as.data.frame(k)
print("try to store re.sub")

#save(k,file = "output1long.rda")
save(k, file=paste0('output_',gsub('part','out',filename),'.rda'))

print("done")
rm(list=ls())
gc()








