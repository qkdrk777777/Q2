setwd('C:/Users/qkdrk/Downloads/data')
(list<-list.files())
ls<-list()
name<-'정보공시학교코드'
all_list=list()
for(i in 1:length(list)){
  all_list[[i]]<-read.csv(list[i])
  all_list[[i]]<-all_list[[i]][,-1]
if(i %in% c(1:15))j=3 else j=7
colnames(all_list[[i]])[j]<-name
}
names(all_list)<-list

colnames(all_list[[15]])[c(10:15,18,19)]<-paste('학급별',colnames(all_list[[15]])[c(9:14,17:18)])
colnames(all_list[[26]])[c(16:21,24)]<-paste0('학급수_',colnames(all_list[[26]])[c(16:21,24)])
colnames(all_list[[27]])[c(16:24)]<-paste0('학생수_',colnames(all_list[[27]])[c(16:24)])

#초등학교 추출
#제외여부가 Y인 값 제거
for(i in 1:length(list))
  all_list[[i]]<-all_list[[i]][all_list[[i]]$학교급코드%in%c(2,19),]
for(i in c(1:7,9:23,25:29))
all_list[[i]]<-all_list[[i]][all_list[[i]]$제외여부%in%'N',]

#2 수업일수이므로 제거, 11 필요한지원시설목록이라 제거
#12~14 급식비 목록 일딴 킵
#16은 row 0
#17체력관련
all_list[[1]]<-all_list[[1]][,-c(1,5,7,8)]
for(i in c(3:7,9:10,12:15))
all_list[[i]]<-all_list[[i]][,-c(1,4:8)]
all_list[[8]]<-all_list[[8]][,-c(1,4:7)]
all_list[[18]]<-all_list[[18]][,-c(1:4,6,8:11,13:14)]
for(i in c(19:28))
all_list[[i]]<-all_list[[i]][,-c(1:4,6,8:15)]

a1<-merge(all_list[[1]],all_list[[3]],by=c('정보공시학교코드','지역'),all=T)
for(i in c(4:10,12:15,18:28))
a1<-merge(a1,all_list[[i]],by=c('정보공시학교코드','지역'),all=T)
setwd('D:\\Q2')
write.csv(a1,'data_raw.csv')

head(all_list[[1]])
list[6]
