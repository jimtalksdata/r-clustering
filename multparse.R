require(data.table)
require(plyr)
require(dplyr)
require(BBmisc)
require(factoextra)
multparse=function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = llply(filenames, function(x){unique(data.table(read.csv(file=x,header=T),key="ID"),by="ID")})
  filetemp<-strsplit(filenames,"/")
  names(datalist)<-sub("-nyzo.csv","",unlist(filetemp)[length(filetemp[[1]])*(1:length(filenames))])
  datalist = lapply(seq(datalist),function(i){
    df<-datalist[[i]]
    df[Performance.score=='-',Performance.score:='0']
    df=transform(df,`Version` = as.numeric(`Version`),`Performance.score` = as.numeric(as.character(`Performance.score`)))
    colnames(df)<-paste0(colnames(df),"-",names(datalist)[[i]])
    colnames(df)[2]<-"ID"
    a<-strsplit(as.character(df[[c(5)]]),"/")
    a2<-as.numeric(unlist(a))
    a3<-as.data.table(matrix(a2,length(a),2,byrow=TRUE))
    colnames(a3)<-c(paste0("transmitted","-",names(datalist)[[i]]),paste0("created","-",names(datalist)[[i]]))
    if(i!=1) {
      df<-bind_cols(df[,c(2,4,6)],a3)
    }
    else {
      df<-bind_cols(df[,c(1,2,3,4,6)],a3)
    }
    print(lengths(df))
    return(df)
  })
  Reduce(function(x,y) {
    print(lengths(x))
    return(merge(x,y,by="ID",all=TRUE,sort=TRUE))
  }, datalist)
}
replaceNA = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=0]
}
processData= function(DT) {
  DT<-mydata[`Performance.score-2019-08-19`!=0]
  replaceNA(DT)
  rownames(DT)<-make.names(DT[,`Nickname-2019-08-19`],unique=TRUE)
  return(normalize(DT))
  
}