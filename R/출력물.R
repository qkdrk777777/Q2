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
par(mfrow=c(1,1))
q[[i]]@data<-merge(q[[i]]@data,dd_data[[i]],by.x='del9',by.y='시군구',all=T)
p(q[[i]],q[[i]]@data,4,rev=T)
q[[i]]@data<-q[[i]]@data[,1:3]


q[[i]]@data<-merge(q[[i]]@data,dd_data1[[i]],by.x='del9',by.y='시군구',all=T)
p(q[[i]],q[[i]]@data,j,name='지니지수')
q[[i]]@data<-q[[i]]@data[,1:3]

q[[i]]@data<-merge(q[[i]]@data,dd_data2[[i]],by.x='del9',by.y='시군구',all=T)
p(q[[i]],q[[i]]@data,j,name='엣킨슨지수')
q[[i]]@data<-q[[i]]@data[,1:3]
