## Code determines key words in a variety of URL scraped websites.

args<-commandArgs(TRUE)

#print(args)

setwd("C:/Users/Clayton/tutorial")
library (stringdist)



Read_dataset<-function(Name) {
   dataset<-read.csv(Name)

   dataset$current_url<-unlist(strsplit(Name,"[.]"))[1]
   return (dataset)
}

SaraSoftware<-Read_dataset("SaraSoftware.csv")
SoftwareAU<-Read_dataset("SoftwareAU.csv")
SoftwareKing<-Read_dataset("SoftwareKing.csv")
BuyCheapSoftware<-Read_dataset("BuyCheapSoftware.csv")

Aggregate_Data<-rbind(SaraSoftware,SoftwareAU,SoftwareKing,BuyCheapSoftware)


Load_Software<-function(brand,product,year) {
  Aggregate_Data$Brand<-lapply(Aggregate_Data[,3],determine_Brand)
  Aggregate_Data$Year<-lapply(Aggregate_Data[,3],determine_year)
  Aggregate_Data$product<-lapply(Aggregate_Data[,3],determine_product)
  t<-Aggregate_Data$Brand==brand
  u<-Aggregate_Data[t,]
  
  v<-u$product==product
  w<-u[v,]

  x<-w$Year==year
  y<-w[x,]
  }

determine_year<-function(string) {
  
  if (grepl("2007",string)) {return ("2007")}
  if (grepl("2010",string)) {return ("2010")}
  if (grepl("2011",string)) {return ("2011")}
  if (grepl("2013",string)) {return ("2013")}
  if (grepl("2016",string)) {return ("2016")}
  
}

determine_Brand<-function(string) {
  
  if (grepl("Mac",string)) {return ("Mac")}
  if (grepl("Microsoft",string)) {return ("Microsoft")}
  if (grepl("Office365",string)) {return ("Office365")}
  if (grepl("Norton",string)) {return ("Norton")}
  if (grepl("BitDefender",string)) {return ("BitDefender")}
  if (grepl("AVG",string)) {return ("AVG")}
  if (grepl("ESET",string)) {return ("ESET")}
  if (grepl("QuickBooks",string)) {return ("QuickBooks")}
  if (grepl("Kapersky",string)) {return ("Kapersky")}
  if (grepl("MalwareBytes",string)) {return ("MalwareBytes")}
  if (grepl("McAfee",string)) {return ("McAfee")}
  if (grepl("Panda",string)) {return ("Panda")}
  if (grepl("Parallels",string)) {return ("Parallels")}  
  if (grepl("Quark",string)) {return ("Quark")}
  if (grepl("Titanium",string)) {return ("Titanium")}
  if (grepl("FileMaker",string)) {return ("FileMaker")}
  if (grepl("CorelDraw",string)) {return ("CorelDraw")}
  
}

determine_product<-function(string) {
  if (grepl("Office",string)) {return ("Office")}
  if (grepl("Visio",string)) {return ("Visio")}
  if (grepl("AntiVirus",string)) {return ("AntiVirus")}
  if (grepl("Project",string)) {return ("Project")}
  if (grepl("Security",string)) {return ("Security")}
  if (grepl("Graphics",string)) {return ("Graphics")}
  if (grepl("Retail",string)) {return ("Retail")}
  if (grepl("Anti-Malware",string)) {return ("Anti-Malware")}
  if (grepl("Protection",string)) {return ("Protection")}
  if (grepl("Windows",string)) {return ("Windows")}
  if (grepl("Excel",string)) {return ("Excel")}
  if (grepl("Outlook",string)) {return ("Outlook")}
  if (grepl("Word",string)) {return ("Word")}
  if (grepl("Publisher",string)) {return ("Publisher")}  
  if (grepl("Business",string)) {return ("Business")}
  
}

#print(args[1])

new_data<-Load_Software(args[1],args[2],args[3])

#new_data<-Load_Software("Mac","Office","2011")
new_data$Brand<-NULL
new_data$Year<-NULL
new_data$product<-NULL
write.csv(new_data,"D:/upwork/Ronnie_Software/matched_data.csv")


#Websites<-c("SaraSoftware.csv","SoftwareAU.csv","SoftwareKing.csv")

#lapply(Websites,Read_dataset)





#BCS_Office_logical<-grepl("Office",BuyCheapSoftware[,2])
#Office_BCS<-BuyCheapSoftware[BCS_Office_logical,]
#Office_BCS$current_url<-NULL

#Aggregate_Data<-SoftwareKing

#
#Aggregate_Data$SaraSoftware_Price<-NA
#Aggregate_Data$SaraSoftware_Name<-NA

#for (j in 1:length(Aggregate_Data[,1])) 
#  {
#  
#  y<-c()
#  for (i in 1:length(SaraSoftware[,1])) 
#    {
#    if (determine_year(Aggregate_Data[j,2]) == determine_year(SaraSoftware[i,2])) 
#      {
#    y[i]<-stringdist(Aggregate_Data[j,2],SaraSoftware[i,2])
#    }
#   }
#  
#  
#  Aggregate_Data[j,3]<-as.character(SaraSoftware[which.min(y),1])
#  Aggregate_Data[j,4]<-as.character(SaraSoftware[which.min(y),2])
#  
#}


#counter=3

#match_data<-function(dataset) {


#for (j in 1:length(Aggregate_Data[,1])) {
  
#y<-c()
#for (i in 1:length(dataset[,1])) {
#  y[i]<-stringdist(Aggregate_Data[j,2],dataset[i,2])}
#
#  
#Aggregate_Data[j,counter]<-as.character(dataset[which.min(y),1])
#  counter = counter + 1
#Aggregate_Data[j,counter]<-as.character(dataset[which.min(y),2])
#  counter = counter + 1
#
#}
#  return (Aggregate_Data)
#}

#Aggregate_Data$SoftwareAU_Price<-NA
#Aggregate_Data$SoftwareAU_Name<-NA

#for (j in 1:length(Aggregate_Data[,1])) {
  
#  y<-c()
#  for (i in 1:length(SoftwareAU[,1])) {
#    y[i]<-stringdist(Aggregate_Data[j,2],SoftwareAU[i,2])}
  
  
#  Aggregate_Data[j,5]<-as.character(SoftwareAU[which.min(y),1])
#  Aggregate_Data[j,6]<-as.character(SoftwareAU[which.min(y),2])
  
#}

#Aggregate_Data$Office_BCS_Price<-NA
#Aggregate_Data$Office_BCS_Name<-NA

#for (j in 1:length(Aggregate_Data[,1])) {
  
#  y<-c()
#  for (i in 1:length(Office_BCS[,1])) {
#    y[i]<-stringdist(Aggregate_Data[j,2],Office_BCS[i,2])}
  
  
#  Aggregate_Data[j,7]<-as.character(Office_BCS[which.min(y),1])
#  Aggregate_Data[j,8]<-as.character(Office_BCS[which.min(y),2])
  
#}



#Aggregate_Data$Brand<-lapply(Aggregate_Data[,3],determine_Brand)

# BrandInput=function() {
#  readline("Enter Brand:")
#}

# ProductInput=function() {
#  readline("Enter Product:")
#}

#YearInput=function() {
#  readline("Enter Year:")
#}

#Brand_Input<-BrandInput()
#Product_Input<-ProductInput()
#Year_Input<-YearInput()

