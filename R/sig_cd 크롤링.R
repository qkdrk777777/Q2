setwd('D:\\Q2')
#data<-read.csv('data_raw4.csv')
head(data)
#data<-data[,-1]
data$학생수<-as.character(data$학생수)
  data$학생수<-as.numeric(gsub("\\(.+?\\)","",data$학생수))

  #del<-strsplit(gsub("(.+?)","",data$학생수),"[(]")
  #for(i in 1:length(del))
  #  if(is.na(del[[i]][1])==T)del[[i]][2]<-NA
  #del<-matrix(as.numeric(unlist(del)),byrow=T,ncol=2)
  #data$학생수<-del[,1]-del[,2]
  #data$학생수<-del[,1]-del[,2]
data$교원당_학생수<-data$학생수/data$교원수

library(DUcj)
package(RCurl)
package(XML)

q<-shp_all('D:/Q2/SIG_201703 (1)')
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(q[[1]])<-CRS(from.crs)
q[[1]]<-spTransform(q[[1]],CRS(to.crs))
del<-coordinates(q[[1]])
colnames(del)<-c('경도','위도')
a$주소내역<-as.character(a$주소내역)


a1<-'http://www.juso.go.kr/info/RoadNameDataList.do?type=search&roadCd='
a2<-'&keyword=&city1=&county1=&town1=&searchType=0&extend=true'

del<-as.numeric(as.character(q[[1]]@data[,1]))
del4<-NULL
for(i in 1:length(del)){
  url<-paste0(a1,del[i],a2)
  url<-getURL(url)
  url1<-readHTMLTable(url,stringsAsFactors=F)

  if(url1$`코드정보 검색결과 리스트입니다.`[1,1]=='검색 결과가 없습니다.'){
    a2<-'&keyword=&city1=&county1=&town1=&searchType=1&extend=true'
    url<-paste0(a1,del[i],a2)
    url<-getURL(url)
    url1<-readHTMLTable(url,stringsAsFactors=F)
    a2<-'&keyword=&city1=&county1=&town1=&searchType=0&extend=true'
  }
  del2<-url1$`코드정보 검색결과 리스트입니다.`[,5:6]
  del3<-paste(unique(del2)[1],unique(del2)[2])
  del4<-rbind(del4,del3)
}
del5<-strsplit(del4,' ')
del7<-NULL
for(i in 1:length(del5))
{del6<-del5[[i]][1:3]
del7<-rbind(del7,del6)}

del7[is.na(del7[,2]),2]<-''
del7[is.na(del7[,3]),3]<-''

del7[(regexpr('도$',del7[,1])!=-1)&(regexpr('고성군$',del7[,2])==-1),1]<-''
del7
colnames(del7)<-NULL
del9<-paste0(del7[,1],del7[,2],del7[,3])
del9[(regexpr('시',del9)!=-1)&(regexpr('구$',del9)!=-1)]<-gsub("시","시 ",del9[(regexpr('시',del9)!=-1)&(regexpr('구$',del9)!=-1)])
del9[(regexpr('시',del9)!=-1)&(regexpr('군$',del9)!=-1)]<-gsub("시","시 ",del9[(regexpr('시',del9)!=-1)&(regexpr('군$',del9)!=-1)])
del9[(regexpr('도',del9)!=-1)&(regexpr('군$',del9)!=-1)&(regexpr('도군',del9)==-1)]<-gsub("도","도 ",del9[(regexpr('도',del9)!=-1)&(regexpr('군$',del9)!=-1)&(regexpr('도군',del9)==-1)])
#확인용
#del9[del9%in%c('부천시 소사구','부천시 원미구','부천시 오정구')]<-'부천시'
setequal(del9,unique(as.character(data$시군구)))

q[[1]]@data<-data.frame(q[[1]]@data,del9)
q[[1]]@data<-q[[1]]@data[,-3]
#q[[1]]@data<-merge(q[[1]]@data,data,by.x='del9',by.y='시군구',all=T)
head(q[[1]]@data)
colnames(data)
library(plyr)
#일반
del<-ddply(data,~시군구,summarise,교원당_학생수=mean(교원당_학생수,na.rm=T),면적=mean(`면적`,na.rm=T)
           ,`1인당_장서수`=mean(`X1인당_장서수`,na.rm=T),`1인당급식비(1식)`=mean(`X1인당.급식비.1식.`,na.rm=T)
           ,`학급당학생수`=mean(학급당_학생수,na.rm=T)
           ,`오후돌봄참여학생수`=mean(`초등돌봄교실.오후돌봄.참여학생수`,na.rm=T)
           ,`돌봄교실수`=mean(`초등돌봄교실.저녁돌봄.운영교실수`,na.rm=T)
           ,`저녁돌봄참여학생수`=mean(`초등돌봄교실.저녁돌봄.참여학생수`,na.rm=T)
           ,`방과후학교연계돌봄학생수`=mean(`초등돌봄교실.방과후학교.연계형.돌봄.참여학생수`,na.rm=T)
           )

#plot그리러 ㄱㄱ
q[[1]]@data<-merge(q[[1]]@data,del,by.x='del9',by.y='시군구',all=T)

#지니지수
library(ineq)
del2<-ddply(data,~시군구,summarise,교원당_학생수=ineq(교원당_학생수,na.rm=T),면적=ineq(`면적`,na.rm=T)
            ,`1인당_장서수`=ineq(`X1인당_장서수`,na.rm=T),`1인당급식비(1식)`=ineq(`X1인당.급식비.1식.`,na.rm=T)
            ,`학급당학생수`=ineq(학급당_학생수,na.rm=T)
            ,`학생수`=ineq(학생수,na.rm=T)
            ,`오후돌봄참여학생수`=ineq(`초등돌봄교실.오후돌봄.참여학생수`,na.rm=T)
            ,`돌봄교실수`=ineq(`초등돌봄교실.저녁돌봄.운영교실수`,na.rm=T)
            ,`저녁돌봄참여학생수`=ineq(`초등돌봄교실.저녁돌봄.참여학생수`,na.rm=T)
            ,`방과후학교연계돌봄학생수`=ineq(`초등돌봄교실.방과후학교.연계형.돌봄.참여학생수`,na.rm=T)
            )

q[[1]]@data<-q[[1]]@data[,1:3]
q[[1]]@data<-merge(q[[1]]@data,del2,by.x='del9',by.y='시군구',all=T)

#엣킨슨지수
library(ineq)
del2<-ddply(data,~시군구,summarise,교원당_학생수=ineq(교원당_학생수,na.rm=T,type='Atkinson'),면적=ineq(`면적`,na.rm=T,type='Atkinson')
            ,`1인당_장서수`=ineq(`X1인당_장서수`,na.rm=T,type='Atkinson'),`1인당급식비(1식)`=ineq(`X1인당.급식비.1식.`,na.rm=T,type='Atkinson')
            ,`학급당학생수`=ineq(학급당_학생수,na.rm=T,type='Atkinson')
            ,`학생수`=ineq(학생수,na.rm=T,type='Atkinson')
            ,`오후돌봄참여학생수`=ineq(`초등돌봄교실.오후돌봄.참여학생수`,na.rm=T,type='Atkinson')
            ,`돌봄교실수`=ineq(`초등돌봄교실.저녁돌봄.운영교실수`,na.rm=T,type='Atkinson')
            ,`저녁돌봄참여학생수`=ineq(`초등돌봄교실.저녁돌봄.참여학생수`,na.rm=T,type='Atkinson')
            ,`방과후학교연계돌봄학생수`=ineq(`초등돌봄교실.방과후학교.연계형.돌봄.참여학생수`,na.rm=T,type='Atkinson')
            )

q[[1]]@data<-q[[1]]@data[,1:3]
q[[1]]@data<-merge(q[[1]]@data,del2,by.x='del9',by.y='시군구',all=T)


#plot 그리기
library('RColorBrewer')

p<-function(j,rev=F){
  if(rev)colors =rev(brewer.pal(9,"YlOrRd")) else colors =(brewer.pal(9,"YlOrRd"))
  col=cut(q[[1]]@data[,j],seq(floor(min(q[[1]]@data[,j])),
                              ceiling(max(q[[1]]@data[,j])),length=10),labels=F,right=F)
  par(bg='transparent')
  plot(q[[1]],col=colors[col],main=colnames(q[[1]]@data)[j],cex.main=2.5)
  tt<-(seq(floor(min(q[[1]]@data[,j])),ceiling(max(q[[1]]@data[,j])),length=10))
  tt<-round(tt,3)
  tttt<-NULL
  for(i in 2:length(tt)-1)
  {ttt<-paste0(tt[i],'~',tt[i+1])
  tttt<-c(tttt,ttt)}
  p<-c(37.25,129.62)
  legend(p[2],p[1],legend=tttt,col=colors,pch=16,cex=1,bty='n')
}
colnames(q[[1]]@data)
p(4,rev=T)


#writeSpatialShape(q[[1]],'out1.shp')
