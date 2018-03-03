찌끄래기


#2
#data<-read.csv('data_raw4.csv')
head(data)
#data<-data[,-1]
#data$학생수<-as.character(data$학생수)
#data$학생수<-as.numeric(gsub("\\(.+?\\)","",data$학생수))

#del<-strsplit(gsub("(.+?)","",data$학생수),"[(]")
#for(i in 1:length(del))
#  if(is.na(del[[i]][1])==T)del[[i]][2]<-NA
#del<-matrix(as.numeric(unlist(del)),byrow=T,ncol=2)
#data$학생수<-del[,1]-del[,2]
#data$학생수<-del[,1]-del[,2]
#data$교원당_학생수<-data$학생수/data$교원수
