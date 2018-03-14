library(stringr)
for(qq in 1:2){
for(ff in c('output1','gini','atkinson')){
for(f in c('a','e')){
setwd(paste0('D:\\Q2\\',ff,'\\case'))
read<-readLines(paste0('D:/Q2/',ff,'/case/',f,'.txt'))
e_<-grep('Location IDs included',read)
e<-str_extract_all(grep('Location IDs included',read,value=T),'[0-9\\.\\,]+')

cluster<-list();LLRs<-NULL;cl_len<-NULL
for(i in 1:length(e_)){
del<-paste(read[e_[i]:(grep('Overlap with clusters',read)[i]-1)],collapse = "")
del<-gsub('\\.|\\:','',gsub(" ","",substr(del,nchar('Location IDs included')+5,nchar(del))))
cluster[[i]]<-as.numeric(unlist(str_split(del,',')))
LLRs[i]<-str_extract_all(grep('Log likelihood ratio',read,value=T),'[0-9\\.]+')[[i]][2]
cl_len[i]<-length(cluster[[i]])
}
keep<-gsub('Population|\\.|\\:| ','',grep('Population...',read,value=T))
pop<-as.numeric(keep[-grep('[A-z]',keep)])
RR<-as.numeric(str_trim(gsub('Relative risk.........:','',grep('Relative risk.........:',read,value=T))))
overlap<-str_extract_all(grep('Overlap with clusters',read,value=T),'[0-9]+')

ls<-list()
for(i in 1:length(cluster))ls[[i]]<-list(cluster[[i]],if(RR[i]>1)RR[i]=2 else RR[i]=4,length(overlap[[i]])!=0)
q[[qq]]@data<-cbind(q[[qq]]@data,col=0)
for(j in 1:length(cluster)){
  for(k in 1:nrow(q[[qq]]@data)){
  if(q[[qq]]@data$SIG_CD[k]%in%ls[[j]][[1]]){
if(ls[[j]][[3]]==T){
q[[qq]]@data[k,ncol(q[[qq]]@data)]<-ls[[j]][[2]]}else q[[qq]]@data$col[k]<-0
}}}

#png(paste0(qq+2015,ff,'_',f,'.png'),bg='transparent',width=1180,height=436)
#plot(q[[qq]],col=q[[qq]]@data$col)
#dev.off()
q[[qq]]@data<-q[[qq]]@data[,1:3]
}}}
# 우도비 값->col 넣고
n<-unlist(lapply(cluster,length))
pop
CLIC=NULL
for(i in 1:length(cluster))
{CLIC[i]<--2*as.numeric(LLRs[i])+log(pop[i])*n[i]
if( i == length(cluster))print(sum(CLIC))}

sum(CLIC)
