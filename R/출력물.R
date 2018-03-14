
library('RColorBrewer')

p<-function(shp,data,j,rev=F,name=''){
  if(rev)colors =rev(brewer.pal(9,"YlOrRd")) else colors =(brewer.pal(9,"YlOrRd"))
  col=cut(data[,j],seq(floor(min(data[,j])),
                              ceiling(max(data[,j])),length=10),labels=F,right=F)
  par(bg='transparent')
  plot(shp,col=colors[col],main=paste(colnames(data)[j],name),cex.main=2.5)
  tt<-(seq(floor(min(data[,j])),ceiling(max(data[,j])),length=10))
  tt<-round(tt,3)
  tttt<-NULL
  for(i in 2:length(tt)-1)
  {ttt<-paste0(tt[i],'~',tt[i+1])
  tttt<-c(tttt,ttt)}
  legend(129.62,37.25,legend=tttt,col=colors,pch=16,cex=1,bty='n')
}
i=1;j=4

q[[i]]@data<-merge(q[[i]]@data,dd_data[[i]],by.x='del9',by.y='시군구',all=T)
p(q[[i]],q[[i]]@data,j,rev=T)
q[[i]]@data<-q[[i]]@data[,1:3]


q[[i]]@data<-merge(q[[i]]@data,dd_data1[[i]],by.x='del9',by.y='시군구',all=T)
p(q[[i]],q[[i]]@data,j,name='지니지수')
q[[i]]@data<-q[[i]]@data[,1:3]

q[[i]]@data<-merge(q[[i]]@data,dd_data2[[i]],by.x='del9',by.y='시군구',all=T)
p(q[[i]],q[[i]]@data,j,name='엣킨슨지수')
q[[i]]@data<-q[[i]]@data[,1:3]

for(i in 1:2){
  q[[i]]@data<-q[[i]]@data[,1:4]
  q[[i]]@data<-merge(q[[i]]@data,dd_data[[i]],by.x='del9',by.y='시군구',all=T)
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
  for(j in 5:ncol(q[[i]]@data)){
    if(j%in%c(4,7,8,9))rev=F else rev=T
    setwd('D:/Q2/output1')
png(filename=paste0(i+2015,colnames(q[[i]]@data[j]),'.png'),bg='transparent',width=1180,height=436)
p(q[[i]],q[[i]]@data,j,rev=rev)
dev.off()}}
install.pack
library(devtoo)
for(i in 1:2){
  q[[i]]@data<-q[[i]]@data[,1:4]
  q[[i]]@data<-merge(q[[i]]@data,dd_data1[[i]],by.x='del9',by.y='시군구',all=T)
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
  for(j in 5:(ncol(q[[i]]@data)-2)){
    setwd('D:/Q2/gini')
    png(filename=paste0(i+2015,colnames(q[[i]]@data[j]),'.png'),bg='transparent',width=1180,height=436)
    p(q[[i]],q[[i]]@data,j,name='지니지수',rev=F)
    dev.off()}}


for(i in 1:2){
  q[[i]]@data<-q[[i]]@data[,1:4]
  q[[i]]@data<-merge(q[[i]]@data,dd_data2[[i]],by.x='del9',by.y='시군구',all=T)
  q[[i]]@data<-q[[i]]@data[order(as.numeric(as.character(q[[i]]@data$SP_ID))),]
 if (i==2)t<-2 else t<-1
  for(j in 5:(ncol(q[[i]]@data)-t)){
    setwd('D:/Q2/atkinson')
    png(filename=paste0(i+2015,colnames(q[[i]]@data[j]),'.png'),bg='transparent',width=1180,height=436)
    p(q[[i]],q[[i]]@data,j,name='엣킨슨지수',rev=F)
    dev.off()}}
