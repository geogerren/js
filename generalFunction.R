monthDiff <- Vectorize(function(startMth, endMth){
  return((as.numeric(substr(endMth,1,4))-as.numeric(substr(startMth,1,4)))*12+(as.numeric(substr(endMth,5,6))-as.numeric(substr(startMth,5,6))))
})

colonParser <- Vectorize(function(obj, targetText) {
  sub<-str_extract(obj, paste0(targetText, ".*<"))
  positionStart<-str_locate(sub, ":")[1]+1
  positionEnd<-str_locate(sub, "\\(<")[1]-1
  return(substr(sub,positionStart,positionEnd))
})

featureGen <- function(DT, modid, indexid, targetText, seqParam=1) {
  if(seqParam==1){
    DT[mod_id==modid&index_id==indexid, key:=targetText]
    DT[mod_id==modid&index_id==indexid, value:=colonParser(input_val, targetText)]
    DT[mod_id==modid&index_id==indexid, seq:=seqParam]
    return(DT)
  }else{
    mid<-DT[mod_id==modid&index_id==indexid&seq==1,]
    mid[, seq:=seqParam]
    mid[, key:=targetText]
    mid[, value:=colonParser(input_val, targetText)]
    DT<-rbind(DT, mid)
    return(DT)
  }
}

############## Value Infer 三重奏
viewAllValues <- function(DT, column){
  result<-table(DT[, column, with=F], useNA = "ifany")
  result<-data.table(result)
  setnames(result, "V1", names(DT[, column, with=F]))
  View(result)
}

isAlphaNum <- Vectorize(function(str){
  result<-ifelse(length(charToRaw(substr(str, 1, 1)))==1, 1, 0)
  return(result)
})

naBlankInfer <- function(DT, column, inferFrom=c(NA, 'None', ''), inferTo=0){
  DT[get(column) %in% inferFrom, column:=as.character(inferTo), with=F]
}
################################
banding <- function(DT, columnValue, columnBand, bands=seq(0, 1, 0.1)){
  DT[, eval(columnBand):=as.integer(cut(get(columnValue), quantile(get(columnValue), probs=bands, na.rm=T), include.lowest=TRUE))]
}

# 拉平data.table 
# test<-dcast.data.table(DT, mainID(用来left join的那个) ~ 变量名, fun.agg=max, value.var = "变量值")

featureAnalysis <- function(DT, exclude, cateConv=F) {
  result <- data.frame(VarName=character(),
                   sType=character(),
                   N=integer(),
                   Nmiss=integer(),
                   MissPctg=integer(),
                   Mean=double(),
                   Stdev=double(),
                   Max=double(),
                   Min=double(),
                   P01=double(),
                   P05=double(),
                   P10=double(),
                   P25=double(),
                   Median=double(),
                   P75=double(),
                   P90=double(),
                   P95=double(),
                   P99=double(),
                   stringsAsFactors=FALSE)


  for(varName in names(DT)){
    if(!(varName %in% exclude)){
      DT[, varName:=as.numeric(get(varName)), with=F]
      print(varName)
      if(cateConv & nrow(as.data.table(table(DT[, varName, with=F],useNA="ifany")))<=10){
        #如果变量的所有可选值不大于10，可以选择转成categorical，randomForest不用转
        DT[, varName:=as.factor(get(varName)), with=F]
      }
      oneVar<-unlist(DT[, varName, with=F])
      newRow<-data.frame(varName, 
                         typeof(oneVar),
                         length(oneVar), 
                         length(oneVar[is.na(oneVar)]),
                         length(oneVar[is.na(oneVar)])/length(oneVar),
                         mean(oneVar, na.rm = T), 
                         sd(oneVar, na.rm = T), 
                         min(oneVar, na.rm = T), 
                         median(oneVar, na.rm = T), 
                         max(oneVar, na.rm = T), 
                         quantile(oneVar, c(.01), na.rm=T),
                         quantile(oneVar, c(.05), na.rm=T),
                         quantile(oneVar, c(.10), na.rm=T),
                         quantile(oneVar, c(.25), na.rm=T),
                         quantile(oneVar, c(.75), na.rm=T),
                         quantile(oneVar, c(.90), na.rm=T),
                         quantile(oneVar, c(.95), na.rm=T),
                         quantile(oneVar, c(.99), na.rm=T)
                         )
      result<-rbind(result, newRow)
    }
  }
  return(result)
}

