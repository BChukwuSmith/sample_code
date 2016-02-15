# This program is a program designed to clean data.  The program divides the data
# into sets of ID & test_code, and then uses a loop to generate a single data line
# containing the important information from that ID / test_code subset.


library(plyr)

##for "411-498.csv"
#raw_data1<-read.csv("411-498.csv", stringsAsFactors = FALSE)

##for "333-410.csv"
raw_data<-read.csv("333-410.csv", stringsAsFactors = FALSE)
names<-colnames(raw_data)
names[1]<-"IDbyDNA_id"
names[7]<-"test_code"
names[11]<-"result"
colnames(raw_data)<-names

test_data<-raw_data
test_data$result<-as.character(test_data$result)

test_data$cleaned<-NA

for (i in 1:length(test_data[,1])) {
  if (!(grepl("Positive", test_data[i,"result"]) | grepl("positive", test_data[i,"result"])) && !(grepl("Negative", test_data[i,"result"]) | grepl("negative", test_data[i,"result"])))
  {
    test_data$cleaned[i]<-"Ambiguous"
    next}
  
  if (grepl("Process", test_data[i,"result"]) | grepl("pending",test_data[i,"result"]) | grepl("Progress", test_data[i,"result"])  | grepl("to follow",test_data[i,"result"]) | grepl("progress", test_data[i,"result"]))
  {
    test_data$cleaned[i]<-"Ambiguous"
    next}
  
  if (grepl("Positive",test_data[i,"result"])) 
     {
    test_data$cleaned[i]<-"Positive"
   }
  
  if (grepl("Negative",test_data[i,"result"]) | grepl("negative",test_data[i,"result"]) | grepl("Not Detected",test_data[i,"result"]) | grepl("None Detected", test_data[i,"result"])) 
    {
    test_data$cleaned[i]<-"Negative"
  }
  # else {test_data$cleaned[i]<-"Ambiguous"}
}

number<-length(raw_data[1,]) + 1

loj<-is.na(test_data[,number])
u<-test_data[loj,]
test_data[loj,number]<-"Ambiguous"

write.csv(test_data,"test_data.csv")

new_data<-data.frame()

unique_ID<-unique(raw_data$IDbyDNA_id)
unique_TC<-unique(raw_data$test_code)

test_data$Positive<-0
test_data$Negative<-0
test_data$Ambiguous<-0

x<-data.frame()


ping<-1

semicolon_p<-function(n) {
  if (n == 0) {return(NA)}
  x="Positive"
  m = n - 1
  for (i in 1:m){
    x<-paste(x,"; Positive")
             }
  return (x)
}

d<-NULL
align_rows<-function(id)  {
  for (i in unique_TC) {
    subset<-test_data[ which(test_data$IDbyDNA_id==id & test_data$test_code == i), ]
    if (length(subset[,1])==0) 
    {next}
    
    #ping=ping+1
  #  print(i)
    

    obs<-subset[1,]
    
    obs$Negative<-""
    obs$Positive<-""
    obs$Ambiguous<-""
    
    for (k in 1:length(subset[,1]))
      {
      if (subset$cleaned[k] == "Negative") 
      {obs$Negative<-paste(as.character(obs$Negative),as.character(subset$result[k]), sep=";")}
      if (subset$cleaned[k] == "Positive") 
      {obs$Positive<-paste(as.character(obs$Positive),as.character(subset$result[k]), sep="; ")}
      if (subset$cleaned[k] == "Ambiguous") 
      {obs$Ambiguous<-paste(as.character(obs$Ambiguous),as.character(subset$result[k]), sep="; ")}
      
      }
    d<-rbind(d,obs) 
    
    }
  return(d) 
}

y<-NULL
c<-0
for (i in 1:length(unique_ID)){
  c<-c+1
  z<-align_rows(unique_ID[i])
  y<-rbind(y,z)
}



class(y$Positive)<-"character"
class(y$Negative)<-"character"
class(y$Ambiguous)<-"character"

write.csv(y,"prototype.csv")

## programs used for testing and formatting

semicolon_n<-function(n) {
  if (n == 0) {return(NA)}
  x="Negative"
  m = n - 1
  for (i in 1:m){
    x<-paste(x,"; Negative")
  }
  return (x)
}

semicolon_a<-function(n) {
  if ( n==0 ) {return(NA)}
  x="Ambiguous"
  m = n - 1
  for (i in 1:m){
    x<-paste(x,"; Ambiguous")
  }
  return (x)
}

counter<-function(x) {
  Pos<-0
  Neg<-0
  Amb<-0
  for (i in 1:length(x[,1])) {
    if (x[i,11] == "Positive") {Pos = Pos + 1}
    if (x[i,11] == "Negative") {Neg = Neg + 1}
    if (x[i,11] == "Ambiguous") {Amb = Amb + 1}
  }
  return (c(Pos,Neg,Amb))
}