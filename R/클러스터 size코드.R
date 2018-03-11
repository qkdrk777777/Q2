#library(foreign)
#library(rsatscan)
#a<-foreign::read.dbf('D://Q2/output1/case/a.col.dbf')
#e<-foreign::read.dbf('D://Q2/output1/case/e.col.dbf')
library(rsatscan)
td<-'D:/Q2/satscan'; name<-c('nom_','gini','atkinson')
name2<-letters

for(j in 1:2){
  for(l in 1:3){
for(i in c(5,9)){
  for(k in c(2,5,10,15,20,25,30,40,50)){
write.cas(get(paste0('end',l))[[j]][,c(2,i)],td,paste0(name[l],j+2015,name2[i]))
write.geo(get(paste0('end',l))[[j]][,c(2,ncol(get(paste0('end',l))[[j]]),
  ncol(get(paste0('end',l))[[j]])-1)],td,paste0(name[l],j+2015,name2[i]))
write.pop(data.frame(get(paste0('end',l))[[j]][,2],year='2000',
                     get(paste0('end',l))[[j]][,13]),td,paste0(name[l],j+2015,name2[i]))
invisible(ss.options(reset=T))
ss.options()
ss.options(list(PrecisionCaseTimes=0,CaseFile=paste0(name[l],j+2015,name2[i],'.cas'),StartDate='2000/1/1',EndDate='2000/12/31',
                 PopulationFile=paste0(name[l],j+2015,name2[i],'.pop'),
                CoordinatesFile=paste0(name[l],j+2015,name2[i],'.geo'),
                CoordinatesType=1,ModelType=0,AnalysisType=1,
                 ScanAreas=3,MaxSpatialSizeInPopulationAtRisk=k,ResultsFile=paste0('D:\\Q2\\satscan\\output\\',paste0(k,'_',name[l],j+2015,'_',name2[i]),'.txt'),
                 OutputGoogleEarthKML='y',OutputShapefiles='y',
                 MostLikelyClusterEachCentroidASCII='y',MostLikelyClusterEachCentroidDBase='y',
                 MostLikelyClusterCaseInfoEachCentroidASCII='y',MostLikelyClusterCaseInfoEachCentroidDBase='y',
                 CensusAreasReportedClustersASCII='y',CensusAreasReportedClustersDBase='y',
                 IncludeRelativeRisksCensusAreasASCII='y',IncludeRelativeRisksCensusAreasDBase='y',
                 SaveSimLLRsASCII='y',SaveSimLLRsDBase='y'))
write.ss.prm(td, paste0(k,'_',name[l],j+2015,'_',name2[i]))
}}}}



#NYCfever = satscan(td, "typeA1", sslocation="C:/Program Files/SaTScan")



