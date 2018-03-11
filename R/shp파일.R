
library(DUcj)
package(maptools)
package(RCurl)
package(XML)
package(rgdal)
shp_ls<-c('SIG_201602','SIG_201703')

for(k in 1:2){
  q<-shp_all(paste0('D:/Q2/',shp_ls[k]))
  from.crs =  "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
  to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  proj4string(q[[1]])<-CRS(from.crs)
  q[[1]]<-spTransform(q[[1]],CRS(to.crs))

  writePolyShape(q[[1]],'D:/Q2/q.shp')
  q<-shp_all('D:/Q2/')
  a1<-'http://www.juso.go.kr/info/RoadNameDataList.do?type=search&roadCd='
  a2<-'&keyword=&city1=&county1=&town1=&searchType=0&extend=true'

  del<-as.numeric(as.character(q[[1]]@data[,2]))
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

  colnames(del7)<-NULL

  del9<-paste0(del7[,1],del7[,2],del7[,3])
  del9[(regexpr('시',del9)!=-1)&(regexpr('구$',del9)!=-1)]<-gsub("시","시 ",del9[(regexpr('시',del9)!=-1)&(regexpr('구$',del9)!=-1)])
  del9[(regexpr('시',del9)!=-1)&(regexpr('군$',del9)!=-1)]<-gsub("시","시 ",del9[(regexpr('시',del9)!=-1)&(regexpr('군$',del9)!=-1)])
  del9[(regexpr('도',del9)!=-1)&(regexpr('군$',del9)!=-1)&(regexpr('도군',del9)==-1)]<-gsub("도","도 ",del9[(regexpr('도',del9)!=-1)&(regexpr('군$',del9)!=-1)&(regexpr('도군',del9)==-1)])
  #확인용
  if(shp_ls[k]=='SIG_201602')del9[del9%in%c('부천시 소사구','부천시 원미구','부천시 오정구')]<-'부천시'

  q[[1]]@data<-data.frame(q[[1]]@data,del9)
  q[[1]]@data<-q[[1]]@data[,-3]
  #q[[1]]@data<-merge(q[[1]]@data,data,by.x='del9',by.y='시군구',all=T)
  #setwd(paste0('D:/Q2/',shp_ls[k],'_new'))
  setwd(paste0('D:/Q2/new'))
  write.csv(q[[1]]@data[,c(2,4)],paste0('out',k,'.csv'))
  writeSpatialShape(q[[1]],paste0('out',k,'.shp'))
}

