library(stringr)
setwd('D:\\Q2\\output1\\case')
read<-readLines('D:/Q2/output1/case/e.txt')
e_<-grep('Location IDs included',read)
e<-str_extract_all(grep('Location IDs included',read,value=T),'[0-9\\.\\,]+')

cluster<-list()
for(i in 1:length(e_)){
del<-paste(read[e_[i]:(grep('Overlap with clusters',read)[i]-1)],collapse = "")
del<-gsub('\\.|\\:','',gsub(" ","",substr(del,nchar('Location IDs included')+5,nchar(del))))
cluster[[i]]<-as.numeric(unlist(str_split(del,',')))}
cluster

RR<-as.numeric(str_trim(gsub('Relative risk.........:','',grep('Relative risk.........:',read,value=T))))
RR

library(devtools)
install_github('qkdrk777777/DUcj',force=T)
library(DUcj)
translate
