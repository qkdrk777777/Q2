
library(ggplot2)
library(DUcj)
package(class)
package(gmodels)
package(scales)

count=0;w=NULL;pp=NULL;ppp=NULL;f=1;clic_ls<-NULL
for(plot in 1){
for(i in c('output1','gini','atkinson')){
  if(i=='gini')w<-'_G' else if(i=='atkinson')w<-'_A' else w<-''
  for(k in c('a','e')){
    for(j in c(2,5,10,15,20,25,30,40,50)){




count=count+1
a<-foreign::read.dbf(paste0('D:/Q2/',i,'/case/',j,'_2017_',k,w,'.col.dbf'))
read<-readLines(paste0('D:/Q2/',i,'/case/',j,'_2017_',k,w,'.txt'))
len<-grep('Location IDs included',read)
cluster<-NULL;cl=list()
for( t in 1:length(len)){
  del<-paste(read[grep('Location IDs included',read)[t]:(grep('Overlap with clusters',read)[t]-1)],collapse = "")
  del<-gsub('\\.|\\:','',gsub(" ","",substr(del,nchar('Location IDs included')+5,nchar(del))))
  cluster[t]<-del}

keep<-gsub('Population|\\.|\\:| ','',grep('Population...',read,value=T))
pop<-as.numeric(keep[-grep('[A-z]',keep)])
a<-data.frame(a,pop,len=pop/1000)


a$LOC_ID<-cluster
q[[qq]]@data$cluster_id<-0
for(t in 1:length(a$LOC_ID)){
for(tt in 1:nrow(q[[qq]]@data))
{if(length(grep(as.character(q[[qq]]@data$SIG_CD[tt]),a$LOC_ID[t]))!=0)q[[qq]]@data$cluster_id[tt]<-t}}
q[[qq]]@data<-merge(q[[qq]]@data,a,by.x='cluster_id',by.y='CLUSTER',all=T)

q[[qq]]@data<-q[[qq]]@data[order(as.numeric(as.character(q[[qq]]@data$SP_ID))),]
q[[qq]]@data$col<-0;q[[qq]]@data$col2<-0

q[[qq]]@data[is.na(q[[qq]]@data$RADIUS),]$RADIUS<-0;q[[qq]]@data[is.na(q[[qq]]@data$GINI_CLUST),]$GINI_CLUST<-0
for(t in 1:nrow(q[[qq]]@data)){
  if(q[[qq]]@data$RADIUS[t]!=0)if(q[[qq]]@data$GINI_CLUST[t]!='F')q[[qq]]@data$col[t]<-q[[qq]]@data$REL_RISK[t]
if(q[[qq]]@data$col[t]>1)q[[qq]]@data$col2[t]<-2 else if(0<q[[qq]]@data$col[t]&q[[qq]]@data$col[t]<=1)q[[qq]]@data$col2[t]<-4 else q[[qq]]@data$col2[t]<-0}

clic<-data.frame(mean_CLIC=mean(-2*a$LLR+log(a$pop)*a$len),sum_CLIC=sum(-2*a$LLR+log(a$pop)*a$len))
clic_ls<-rbind(clic_ls,clic)
if(plot==1){setwd('D:/Q2/2018-03-11plot')

png(filename=paste0(k,'_',i,'_',j,'.png'),bg='transparent',width=1180,height=436)
  q[[qq]]@data$col3<-q[[qq]]@data$col;
  q[[qq]]@data[q[[qq]]@data$col<=1,]$col3<-0;
  q[[qq]]@data$col4<-q[[qq]]@data$col;
  q[[qq]]@data[q[[qq]]@data$col>1,]$col4<-0;

  plot(q[[qq]],col=c(alpha(2,q[[qq]]@data$col3/max(q[[qq]]@data$col3))),main=paste0(if(i=='output1')""else i,j,'%',if(k=='a')'교원당학생수'else '학급당학생수'))
  plot(q[[qq]],col=c(alpha(4,q[[qq]]@data$col4/max(q[[qq]]@data$col4))),main=paste0(if(i=='output1')""else i,j,'%',if(k=='a')'교원당학생수'else '학급당학생수'),add=T)
  dev.off()
}
plot(1:10,alpha(4,))
q[[qq]]@data<-q[[qq]]@data[,2:5]
    }

    pp<-rownames(clic_ls[f:(f+8),][clic_ls[f:(f+8),1]==min(clic_ls[f:(f+8),1,drop=F]),])
    ppp<-c(ppp,pp)
    f=f+9
  }}
}


e=2
size=c(2,5,10,15,20,25,30,40,50)
for(t in seq(1,54,9)){

         aa<-ifelse((t%/%9)%%2==0,'교원당학생수','학급당학생수')
         aaa<-ifelse(t%/%18<1,'',ifelse(t%/%18<2,'gini','atkinson'))
         del<-clic_ls[t:(t+8),]
         rownames(del)<-1:9
         p<-size[as.numeric(rownames(del[del[,e]==min(del[,e]),]))]
         png(filename=paste0(aa,'_',aaa,p,if(e==1)'mean' else if(e==2)'sum',' percent','.png'),bg='transparent')#,width=1180,height=436)
              plot(size,del[,e],type='b',ylab='CLIC',col=4,main=paste0(aa,aaa))
              abline(h=seq(min(del[,e]),max(del[,e]),len=5),lty=2,col='grey85')
              #abline(h=min(del[,e]),col=2)
              points(p,rep(min(del[,e]),length(p)),col=2,pch=16,cex=2)
              axis(side=1,size)
              dev.off()
}


getwd()



