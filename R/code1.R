setwd('C:/Users/qkdrk/Downloads/data')
(list<-list.files())
ls<-list()
name<-'정보공시학교코드'
all_list=list()
for(i in 1:length(list)){
  all_list[[i]]<-read.csv(list[i])
if(i %in% c(1:15))j=4 else j=8
colnames(all_list[[i]])[j]<-name
}

#for(i in 1:length(all_list))
#assign(paste0('data',i),all_list[[i]])
#rm(list=ls(pat="^all"))
#for(i in 1:(length(list)-1))
#  assign(paste0('data',i+1),merge(get(paste0('data',i)),get(paste0('data',i+1)),by=name,all=T))


#del<-all_list[[1]]
del<-(all_list[[1]][all_list[[1]][,6]==2,])

#학교급 코드 있는지 확인
for(i in 1:length(list)){
  #print(paste(i,sum(colnames(all_list[[i]])%in%'학교급코드')))
print(paste(i,sum(colnames(all_list[[i]])%in%'제외여부')))}
#초등학교 추출
for(i in 1:length(list))
  all_list[[i]]<-all_list[[i]][all_list[[i]]$학교급코드%in%c(2,19),]
#제외여부가 Y인 값 제거
for(i in c(1:7,9:23,25:29))
all_list[[i]]<-all_list[[i]][all_list[[i]]$제외여부%in%'N',]

#시도교육청 지역교육청 학교급코드 제외여부 제외사유
all_list[[1]]<-all_list[[1]][,-c(1,2,6,8,9)]
all_list[[2]]<-all_list[[2]][,-c(1,2,6,8,9)]
all_list[[3]]<-all_list[[3]][,-c(1,2,6,8,9)]
all_list[[4]]<-all_list[[4]][,-c(1,2,6,8,9)]
all_list[[5]]<-all_list[[5]][,-c(1,2,6,8,9)]
all_list[[6]]<-all_list[[6]][,-c(1,2,6,8,9)]

i=7;list[i];head(all_list[[i]])
