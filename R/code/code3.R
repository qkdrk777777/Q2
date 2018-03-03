k=0
data_ls<-list()
for(l in c(2016,""))
{setwd(paste0('C:/Users/qkdrk/Downloads/',l,'data'))
  getwd()
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
  colnames(all_list[[15]])[c(9:14,17,18)]<-paste('학급별',colnames(all_list[[15]])[c(9:14,17:18)])
  colnames(all_list[[26]])[c(16:21,24)]<-paste0('학급수_',colnames(all_list[[26]])[c(16:21,24)])
  colnames(all_list[[27]])[c(16:24)]<-paste0('학생수_',colnames(all_list[[27]])[c(16:24)])

  #초등학교 추출
  #제외여부가 Y인 값 제거
  for(i in 1:length(list))
    all_list[[i]]<-all_list[[i]][all_list[[i]]$학교급코드%in%c(2),]
  for(i in c(1:7,9:23,25:29))
    all_list[[i]]<-all_list[[i]][all_list[[i]]$제외여부=='N',]

  all_list[[1]]<-all_list[[1]][,-c(1,5,7,8)]
  for(i in c(3:7,9:10,12:15))
    all_list[[i]]<-all_list[[i]][,-c(1,4:8)]
  all_list[[8]]<-all_list[[8]][,-c(1,4:7)]
  all_list[[18]]<-all_list[[18]][,-c(1:4,6,8:11,13:14)]
  for(i in c(19:28))
    all_list[[i]]<-all_list[[i]][,-c(1:4,6,8:15)]

  #교원수
  #all_list[[6]]
  #면적
  #all_list[[10]]
  #1인당 장서수
  #all_list[[22]]
  #1인당 급식비
  #all_list[[14]]
  #학급당학생수
  #all_list[[15]]
  #학생수
  #all_list[[27]]
  #돌봄교실
  #all_list[[23]]
  del0<-all_list[[8]][,c(1:2,9)]
  assign(paste0('del',1),all_list[[6]][,c(1,2,58)])
  colnames(del1)<-c(colnames(del1)[1:2],'교원수')
  assign(paste0('del',2),all_list[[10]][,c(1,2,5)])
  colnames(del2)<-c(colnames(del2)[1:2],'면적')
  assign(paste0('del',3),all_list[[22]][,c(1,2,7)])
  colnames(del3)<-c(colnames(del3)[1:2],'1인당_장서수')
  assign(paste0('del',4),all_list[[14]])
  colnames(del4)<-c(colnames(del4)[1:2],'1인당 급식비(1식)')
  assign(paste0('del',5),all_list[[15]][,c(1,2,12)])
  colnames(del5)<-c(colnames(del5)[1:2],'학급당_학생수')
  assign(paste0('del',6),all_list[[27]][,c(1,2,11)])
  colnames(del6)<-c(colnames(del6)[1:2],'학생수')
  assign(paste0('del',7),all_list[[23]][,c(1,2,11:13,15)])
  assign(paste0('del',8),all_list[[1]][,c(1,2,3)])
  del<-list()
  for(i in 0:8)
    del[[i+1]]<-get(paste0('del',i))
  for(i in 1:8)
    del[[(i+1)]]<-merge(del[[i]],del[[(i+1)]],by=c('정보공시학교코드','지역'),all=T)
  a<-del[[9]];a<-a[,-1]
  rm(list=ls(pat='^del'))
  setwd('D:\\Q2')
  #write.csv(a,'data_raw3.csv')

  del2<-NULL
  a$주소내역<-as.character(a$주소내역)
  del<-strsplit(split=' ',a$주소내역)
  library(rvest)
  library(RCurl)
  library(XML)
  for(i in 1:length(a$주소내역)){
    if((as.character(a[i,1])=='')&is.na(del[[i]][1])){
      url<-'https://ko.wikipedia.org/wiki/'
      b<-unlist(strsplit(as.character(a[i,ncol(a)]),'학교'))
      url<-paste0(url,paste0(b[1],'학교'),"_",gsub("분교장","분교",b[2]))
      url<-getURL(url)
      line<-readHTMLTable(url,stringsAsFactors=F)
      b<-unlist(strsplit(split=' ',line[[2]][line[[2]][,1]=='위치',2]))
      b<-c(b[regexpr('시$',b)!=-1],b[regexpr('군$',b)!=-1],b[regexpr('구$',b)!=-1])
      while(length(b)<3)b<-c("",b)
      del3<-b
    } else if(is.na(del[[i]][1])){b<-a[i,1]
    b<-unlist(strsplit(split=' ',as.character(b)))
    b<-c(b[regexpr('시$',b)!=-1],b[regexpr('군$',b)!=-1],b[regexpr('구$',b)!=-1])
    while(length(b)<3)b<-c("",b)
    del3<-b} else{
      del3<-as.character(a[i,1])
      del3<-del[[i]][1:3]}
    del2<-rbind(del2,del3)}

  #확인용

  length(unique(del2[,2]))
  length(unique(del2[,2])[regexpr('구$',unique(del2[,2]))!=-1])
  length(unique(del2[,2])[regexpr('군$',unique(del2[,2]))!=-1])
  length(unique(del2[,2])[regexpr('시$',unique(del2[,2]))!=-1])

  (unique(del2[,2])[regexpr('읍$',unique(del2[,2]))!=-1])
  (unique(del2[,2])[regexpr('면$',unique(del2[,2]))!=-1])
  (unique(del2[,2])[regexpr('동$',unique(del2[,2]))!=-1])
  #확인용 끝
  #읍면동 제거1
  del2[(regexpr('읍$',del2[,2])!=-1)&(del2[,1]=='세종특별자치시'),2]<-""
  del2[(regexpr('면$',del2[,2])!=-1)&(del2[,1]=='세종특별자치시'),2]<-""
  del2[(regexpr('동$',del2[,2])!=-1)&(del2[,1]=='세종특별자치시'),2]<-""

  (unique(del2[,2])[regexpr('읍$',unique(del2[,2]))!=-1])
  (unique(del2[,2])[regexpr('면$',unique(del2[,2]))!=-1])
  (unique(del2[,2])[regexpr('동$',unique(del2[,2]))!=-1])
  #읍면동 제거2
  del3<-(unique(del2[,2])[regexpr('동$',unique(del2[,2]))!=-1])
  del4<-substr(del3,1,regexpr('구',del3))
  for(i in 1:length(del3))
    del2[del2[,2]==del3[i],2]<-del4[i]

  (unique(del2[,2])[regexpr('동$',unique(del2[,2]))!=-1])
  #시도 포멧 같게하기
  (del3<-unique(del2[,1]))
  del4<-c('대구광역시','충청남도','광주광역시','전라남도','서울특별시','전라북도','경상남도','충청북도'
          ,'부산광역시','인천광역시','제주특별자치도','강원도','경상북도','서울특별시'
          ,'부산광역시','인천광역시','광주광역시','대전광역시','대전광역시','울산광역시'
          ,'울산광역시','세종특별자치시','경기도','경기도','강원도','충청북도','충청남도','전라북도','전라남도','경상북도','경상남도',"")
  for(i in 1:length(del3))
    del2[del2[,1]==del3[i],1]<-del4[i]
  #결측값 제거
  del2[is.na(del2[,3]),3]<-''
  #읍면동 제거3
  del2[(regexpr('구$',del2[,3])==-1)&(regexpr('군$',del2[,3])==-1)&(regexpr('시$',del2[,3])==-1),3]<-''
  #도 제거
  del2[regexpr('고성군',del2[,2])!=-1,]
  del2[(regexpr('시$',del2[,1])==-1)&(regexpr('고성군',del2[,2])==-1),1]<-''
  unique(del2[,1])

  #시 구 같이 있는거 띄어쓰기 넣기
  unique(del2[(regexpr('시',del2[,2])!=-1)&(regexpr('구$',del2[,2])!=-1),2])
  #del2[(regexpr('시',del2[,2])!=-1)&(regexpr('구$',del2[,2])!=-1),2]<-gsub("시","시 ",del2[(regexpr('시',del2[,2])!=-1)&(regexpr('구$',del2[,2])!=-1),2])
  unique(del2[(regexpr('시',del2[,2])!=-1)&(regexpr('구$',del2[,2])!=-1),2])

  시군구<-paste0(del2[,1],del2[,2],del2[,3])
  data<-data.frame(a,시군구)
  data[,'시군구']<-as.character(data[,'시군구'])
  data[regexpr('시$',data$시군구)==-1,'시군구']<-gsub('시','시 ',data[regexpr('시$',data$시군구)==-1,'시군구'])
  data[regexpr('군$',data$시군구)!=-1,'시군구']<-gsub('남도','남도 ',data[regexpr('군$',data$시군구)!=-1,'시군구'])
  data[regexpr('군$',data$시군구)!=-1,'시군구']<-gsub('원도','원도 ',data[regexpr('군$',data$시군구)!=-1,'시군구'])
  head(data)
  data<-data[,-c(1,2)]

  setwd('D:\\Q2')
  #write.csv(data,'data_raw4.csv')

  data$학생수<-as.character(data$학생수)
  data$학생수<-as.numeric(gsub("\\(.+?\\)","",data$학생수))
  k=k+1
  data_ls[[k]]<-data
}
for(i in 1:2)
  colnames(data_ls[[i]])[8]<-'돌봄교실.운영교실수'

