library(DUcj)
package(RCurl)
package(XML)
package(rgdal)

setwd('D:\\Q2')
q<-shp_all('new')
setwd('D:\\Q2\\new')
(list<-list.files(pattern='.csv'))

for(i in 1:length(list)){
  assign(paste0('shp',i),read.csv(list[i],stringsAsFactors = F)[,-1])
  q[[i]]@data<-q[[i]]@data[,c(2,3)]
  q[[i]]@data<-merge(q[[i]]@data,get(paste0('shp',i)))
}
rm(list=ls(pat='shp'))

data_ls<-list()
setwd('D:\\Q2')
(list<-list.files(pattern='.csv'))
for(i in 1:length(list))
{assign(paste0((i+2015),'data'),read.csv(list[i],stringsAsFactors = F)[,-1])
data_ls[[i]]<-get(paste0((i+2015),'data'))
assign(paste0('name',i),colnames(data_ls[[i]]))
}
rm(list=ls(pat='data$'))
library(plyr)
#일반
intersect(name1,name2)
setdiff(name1,name2)
setdiff(name2,name1)

dd_data<-list()
for(i in 1:2)
dd_data[[i]]<-ddply(data_ls[[i]],~시군구,summarise,교원당학생수=sum(학생수,na.rm=T)/sum(교원수,na.rm=T)
,면적=mean(`면적`,na.rm=T)
,`1인당_장서수`=mean(`X1인당_장서수`,na.rm=T)
,`1인당급식비(1식)`=mean(`X1인당.급식비.1식.`,na.rm=T)
,`학급당학생수`=mean(학급당_학생수,na.rm=T)
,`오후돌봄참여학생수`=mean(`초등돌봄교실.오후돌봄.참여학생수`,na.rm=T)
,`돌봄교실수`=mean(`돌봄교실.운영교실수`,na.rm=T)
,`저녁돌봄참여학생수`=mean(`초등돌봄교실.저녁돌봄.참여학생수`,na.rm=T)
)

for(i in 1:2)print(setequal(unique(q[[i]]@data$del9),unique(data_ls[[i]]$시군구)))

for(i in 1:2){
setwd('D:/Q2/output1')
q[[i]]@data<-merge(q[[i]]@data,dd_data[[i]],by.x='del9',by.y='시군구',all=T)
writeSpatialShape(q[[i]],paste0(i+2015,'output.shp'))
q[[i]]@data<-q[[i]]@data[,1:3]
}

library(plyr)
dd_data1<-list()
for(i in 1:2){
a<-data_ls[[i]]$교원수!=0
a[is.na(a)==T]<-T

dd<-data_ls[[i]][a,]
dd_data1[[i]]<-ddply(dd,~시군구,summarise,교원당학생수=ineq(학생수/교원수)
                      ,면적=ineq(`면적`)
                      ,`1인당_장서수`=ineq(`X1인당_장서수`)
                      ,`1인당급식비(1식)`=ineq(`X1인당.급식비.1식.`)
                      ,`학급당학생수`=ineq(학급당_학생수)
                      ,`오후돌봄참여학생수`=ineq(`초등돌봄교실.오후돌봄.참여학생수`)
                      ,`돌봄교실수`=ineq(`돌봄교실.운영교실수`)
                      ,`저녁돌봄참여학생수`=ineq(`초등돌봄교실.저녁돌봄.참여학생수`)
  )
}

for(i in 1:2){
  setwd('D:/Q2/gini')
  q[[i]]@data<-merge(q[[i]]@data,dd_data1[[i]],by.x='del9',by.y='시군구',all=T)
  writeSpatialShape(q[[i]],paste0(i+2015,'output.shp'))
  q[[i]]@data<-q[[i]]@data[,1:3]
}

library(plyr)
dd_data2<-list()
for(i in 1:2){
  a<-data_ls[[i]]$교원수!=0
  a[is.na(a)==T]<-T
  dd<-data_ls[[i]][a,]
dd_data2[[i]]<-ddply(dd,~시군구,summarise,교원당학생수=ineq(학생수/교원수,'Atkinson')
                     ,면적=ineq(`면적`,'Atkinson')
                     ,`1인당_장서수`=ineq(`X1인당_장서수`,'Atkinson')
                     ,`1인당급식비(1식)`=ineq(`X1인당.급식비.1식.`,'Atkinson')
                     ,`학급당학생수`=ineq(학급당_학생수,'Atkinson')
                     ,`오후돌봄참여학생수`=ineq(`초등돌봄교실.오후돌봄.참여학생수`,'Atkinson')
                     ,`돌봄교실수`=ineq(`돌봄교실.운영교실수`,'Atkinson')
                     ,`저녁돌봄참여학생수`=ineq(`초등돌봄교실.저녁돌봄.참여학생수`,'Atkinson')
)
}

for(i in 1:2){
  setwd('D:/Q2/Atkinson')
  q[[i]]@data<-merge(q[[i]]@data,dd_data2[[i]],by.x='del9',by.y='시군구',all=T)
  writeSpatialShape(q[[i]],paste0(i+2015,'output.shp'))
  q[[i]]@data<-q[[i]]@data[,1:3]
}

rm(list=ls(pat='dd$'))
rm(list=ls(pat='data_ls'))
