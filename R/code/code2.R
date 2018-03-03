
a1<-read.csv('data_raw.csv',header=T)
a1<-a1[,-c(1)]

getwd()
write.table(colnames(a1),'list.txt')

a1[,c('설립구분','일반교실')]
a1[,-c(1,26:40,186:189,191:194,197)]

curve(exp(x),xlim=c(-5,5),ylim=c(-5,5))
curve(1/x,xlim=c(-5,5),ylim=c(-5,5))
unique(a1[,'설립유형.x'])
