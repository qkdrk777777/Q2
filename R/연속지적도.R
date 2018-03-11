end1<-list();end2<-list();end3<-list()
tt<-1
del<-q[[2]]@data
library(DUcj)
package(RCurl)
package(XML)
package(rgdal)
setwd('D:\\Q2')
q<-shp_all('new')
q[[1]]@data<-q[[1]]@data[,-2];q[[2]]@data<-q[[2]]@data[,-2]

setwd('D:\\Q2\\new')
(list<-list.files(pattern='.csv'))


for(i in 1:length(list)){
q[[i]]@data<-q[[i]]@data[,1:3]
    assign(paste0('shp',i),read.csv(list[i],stringsAsFactors = F)[,-1])
  #q[[i]]@data<-q[[i]]@data[,c(2,3)]
  q[[i]]@data<-merge(q[[i]]@data,get(paste0('shp',i)),by='SIG_CD')
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
}

rm(list=ls(pat='shp'))

data_ls<-list()
setwd('D:\\Q2')
(list<-list.files(pattern='.csv'))
for(i in 1:length(list)){
  assign(paste0((i+2015),'data'),read.csv(list[i],stringsAsFactors = F)[,-1])
  data_ls[[i]]<-get(paste0((i+2015),'data'))
  assign(paste0('name',i),colnames(data_ls[[i]]))
}

library(ineq)
rm(list=ls(pat='data$'))
library(plyr)
#일반
intersect(name1,name2)
setdiff(name1,name2)
setdiff(name2,name1)
dd_data<-list()
for(i in 1:2){
  del<-data_ls[[2]]$학교명!='왕배초등학교'
  del[is.na(del)]<-T
  if(sum(!del)>0){
    del2<-ddply(data_ls[[1]][del,],~시군구,summarise,면적=mean(면적,na.rm=T))
    data_ls[[2]][!del,]$면적<-del2[del2[,1]=='화성시',2]
  }
  if(tt==1000){
  dd_data[[i]]<-ddply(data_ls[[i]],~시군구,summarise,교원당학생수=as.integer(round(sum(학생수,na.rm=T)/sum(교원수,na.rm=T)*tt))
                      ,면적=as.integer(round(mean(`면적`,na.rm=T)*tt))
                      ,`1인당_장서수`=as.integer(round(mean(`X1인당_장서수`,na.rm=T)*tt))
                      ,`1인당급식비(1식)`=as.integer(round(mean(`X1인당.급식비.1식.`,na.rm=T)*tt))
                      ,`학급당학생수`=as.integer(round(mean(학급당_학생수,na.rm=T)*tt))
                      ,`오후돌봄참여학생수`=as.integer(round(mean(`초등돌봄교실.오후돌봄.참여학생수`,na.rm=T)*tt))
                      ,`돌봄교실수`=as.integer(round(mean(`돌봄교실.운영교실수`,na.rm=T)*tt))
                      ,`저녁돌봄참여학생수`=as.integer(round(mean(`초등돌봄교실.저녁돌봄.참여학생수`,na.rm=T)*tt))
  )
}else dd_data[[i]]<-ddply(data_ls[[i]],~시군구,summarise,교원당학생수=sum(학생수,na.rm=T)/sum(교원수,na.rm=T)
                          ,면적=mean(`면적`,na.rm=T)
                          ,`1인당_장서수`=mean(`X1인당_장서수`,na.rm=T)
                          ,`1인당급식비(1식)`=mean(`X1인당.급식비.1식.`,na.rm=T)
                          ,`학급당학생수`=mean(학급당_학생수,na.rm=T)
                          ,`오후돌봄참여학생수`=mean(`초등돌봄교실.오후돌봄.참여학생수`,na.rm=T)
                          ,`돌봄교실수`=mean(`돌봄교실.운영교실수`,na.rm=T)
                          ,`저녁돌봄참여학생수`=mean(`초등돌봄교실.저녁돌봄.참여학생수`,na.rm=T)
)}

for(i in 1:2)print(setequal(unique(q[[i]]@data$del9),unique(data_ls[[i]]$시군구)))
if(tt==1000) for(i in 1:2){
  setwd('D:/Q2/output1')

    q[[i]]@data<-merge(q[[i]]@data,dd_data[[i]],by.x='del9',by.y='시군구',all=T)
  q[[i]]@data$SIG_CD<-as.numeric(as.character(q[[i]]@data$SIG_CD))
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
  q[[i]]@data<-data.frame(q[[i]]@data,pop=1000,lon=coordinates(q[[i]])[,1],lat=coordinates(q[[i]])[,2],sumpop=as.integer(nrow(q[[i]]@data)*tt))
  q[[i]]@data<-q[[i]]@data[,c(1:10,13:16)]
  end1[[i]]<-q[[i]]@data
  writeSpatialShape(q[[i]],paste0(i+2015,'output.shp'))
  q[[i]]@data<-q[[i]]@data[,1:4]
}

library(plyr)
dd_data1<-list()
for(i in 1:2){
  a<-data_ls[[i]]$교원수!=0
  a[is.na(a)==T]<-T
  x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)

  dd<-data_ls[[i]][a,]
  if(tt==1000){
    dd_data1[[i]]<-ddply(dd,~시군구,summarise,교원당학생수=as.integer(round(ineq(학생수/교원수)*tt))
                       ,면적=as.integer(round(ineq(`면적`)*tt))
                       ,`1인당_장서수`=as.integer(round(ineq(`X1인당_장서수`)*tt))
                       ,`1인당급식비(1식)`=as.integer(round(ineq(`X1인당.급식비.1식.`)*tt))
                       ,`학급당학생수`=as.integer(round(ineq(학급당_학생수)*tt))
                       ,`오후돌봄참여학생수`=as.integer(round(ineq(`초등돌봄교실.오후돌봄.참여학생수`)*tt))
                       ,`돌봄교실수`=as.integer(round(ineq(`돌봄교실.운영교실수`)*tt))
                       ,`저녁돌봄참여학생수`=as.integer(round(ineq(`초등돌봄교실.저녁돌봄.참여학생수`)*tt))

  )}else   dd_data1[[i]]<-ddply(dd,~시군구,summarise,교원당학생수=ineq(학생수/교원수)
                                ,면적=ineq(`면적`)
                                ,`1인당_장서수`=ineq(`X1인당_장서수`)
                                ,`1인당급식비(1식)`=ineq(`X1인당.급식비.1식.`)
                                ,`학급당학생수`=ineq(학급당_학생수)
                                ,`오후돌봄참여학생수`=ineq(`초등돌봄교실.오후돌봄.참여학생수`)
                                ,`돌봄교실수`=ineq(`돌봄교실.운영교실수`)
                                ,`저녁돌봄참여학생수`=ineq(`초등돌봄교실.저녁돌봄.참여학생수`)
)}
if(tt==1000)for(i in 1:2){
  setwd('D:/Q2/gini')
  q[[i]]@data<-merge(q[[i]]@data,dd_data1[[i]],by.x='del9',by.y='시군구',all=T)
  q[[i]]@data<-q[[i]]@data[,-c(ncol(q[[i]]@data),ncol(q[[i]]@data)-1)]
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
  q[[i]]@data$SIG_CD<-as.integer(as.character(q[[i]]@data$SIG_CD))
  q[[i]]@data<-data.frame(q[[i]]@data,pop=1000L,lon=coordinates(q[[i]])[,1],lat=coordinates(q[[i]])[,2],sumpop=as.integer(nrow(q[[i]]@data)*tt))
  #q[[i]]@data<-q[[i]]@data[,c(1:10,13:16)]
  end2[[i]]<-q[[i]]@data
  writeSpatialShape(q[[i]],paste0(i+2015,'output.shp'))
  q[[i]]@data<-q[[i]]@data[,1:4]
}

library(plyr)
dd_data2<-list()
for(i in 1:2){
  a<-data_ls[[i]]$교원수!=0
  a[is.na(a)==T]<-T
  dd<-data_ls[[i]][a,]
  if(tt==1000){
  dd_data2[[i]]<-ddply(dd,~시군구,summarise,교원당학생수=as.integer(round(ineq(학생수/교원수,type='Atkinson')*tt))
                       ,면적=as.integer(round(ineq(`면적`,type='Atkinson')*tt))
                       ,`1인당_장서수`=as.integer(round(ineq(`X1인당_장서수`,type='Atkinson')*tt))
                       ,`1인당급식비(1식)`=as.integer(round(ineq(`X1인당.급식비.1식.`,type='Atkinson')*tt))
                       ,`학급당학생수`=as.integer(round(ineq(학급당_학생수,type='Atkinson')*tt))
                       ,`오후돌봄참여학생수`=as.integer(round(ineq(`초등돌봄교실.오후돌봄.참여학생수`,type='Atkinson')*tt))
                       ,`돌봄교실수`=as.integer(round(ineq(`돌봄교실.운영교실수`,type='Atkinson')*tt))
                       ,`저녁돌봄참여학생수`=as.integer(round(ineq(`초등돌봄교실.저녁돌봄.참여학생수`,type='Atkinson')*tt))
  )}else dd_data2[[i]]<-ddply(dd,~시군구,summarise,교원당학생수=ineq(학생수/교원수,type='Atkinson')
                              ,면적=ineq(`면적`,type='Atkinson')
                              ,`1인당_장서수`=ineq(`X1인당_장서수`,type='Atkinson')
                              ,`1인당급식비(1식)`=ineq(`X1인당.급식비.1식.`,type='Atkinson')
                              ,`학급당학생수`=ineq(학급당_학생수,type='Atkinson')
                              ,`오후돌봄참여학생수`=ineq(`초등돌봄교실.오후돌봄.참여학생수`,type='Atkinson')
                              ,`돌봄교실수`=ineq(`돌봄교실.운영교실수`,type='Atkinson')
                              ,`저녁돌봄참여학생수`=ineq(`초등돌봄교실.저녁돌봄.참여학생수`,type='Atkinson')
  )
}
if(tt==1000)for(i in 1:2){
  setwd('D:/Q2/Atkinson')
  q[[i]]@data<-merge(q[[i]]@data,dd_data2[[i]],by.x='del9',by.y='시군구',all=T)
  q[[i]]@data<-q[[i]]@data[,-c(ncol(q[[i]]@data),ncol(q[[i]]@data)-1)]
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
  q[[i]]@data$SIG_CD<-as.integer(as.character(q[[i]]@data$SIG_CD))
  q[[i]]@data<-data.frame(q[[i]]@data,pop=1000L,lon=coordinates(q[[i]])[,1],lat=coordinates(q[[i]])[,2],sumpop=as.integer(nrow(q[[i]]@data)*tt))
  #q[[i]]@data<-q[[i]]@data[,c(1:10,13:16)]
  end3[[i]]<-q[[i]]@data
  writeSpatialShape(q[[i]],paste0(i+2015,'output.shp'))
  q[[i]]@data<-q[[i]]@data[,1:4]
}

dd_data1[[i]]
dd_data2[[i]]
del<-q[[i]]@data
rm(list=ls(pat='dd$'))
rm(list=ls(pat='data_ls'))

getwd()
#png('Lorenz curve.png',bg='transparent')
#par(mfrow=c(2,2))
#plot(Lc(dd_data1[[i]][,2]),col='darkred',lwd=2,main='Lorenz curve by Gini 교원당학생수')
#plot(Lc(dd_data1[[i]][,6]),col='darkred',lwd=2,main='Lorenz curve by Gini 학급당학생수')

#plot(Lc(dd_data2[[i]][,2]),col='darkred',lwd=2,main='Lorenz curve by Atkinson 교원당학생수')
#plot(Lc(dd_data2[[i]][,6]),col='darkred',lwd=2,main='Lorenz curve by Atkinson 학급당학생수')
#dev.off()
