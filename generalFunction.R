monthDiff <- Vectorize(function(startMth, endMth){
  return((as.numeric(substr(endMth,1,4))-as.numeric(substr(startMth,1,4)))*12+(as.numeric(substr(endMth,5,6))-as.numeric(substr(startMth,5,6))))
})

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

############## Parse 二人转
# Parse strings, default pattern: targetText:Value(<>) 
# 先用str_extract取targetText之后的一段string，然后用strStart, strEnd截targetText的值
strParseV <- Vectorize(function(obj, targetText, regexpStopping=".*<", strStart=":", strEnd="\\(") {
  sub<-str_extract(obj, paste0(targetText, regexpStopping))
  positionStart<-str_locate(sub, strStart)[1]+nchar(strStart)
  positionEnd<-str_locate(sub, strEnd)[1]-1
  return(substr(sub,positionStart,positionEnd))
})

strParse <- function(obj, targetText, regexpStopping=".*<", strStart=":", strEnd="\\(") {
  sub<-str_extract(obj, paste0(targetText, regexpStopping))
  positionStart<-str_locate(sub, strStart)[1]+nchar(strStart)
  positionEnd<-str_locate(sub, strEnd)[1]-1
  return(substr(sub,positionStart,positionEnd))
}

# Generate features from t_mod_score
featureGen <- function(DT, modid, indexid, targetText, seqParam=1) {
  if(seqParam==1){
    DT[mod_id==modid&index_id==indexid, key:=targetText]
    DT[mod_id==modid&index_id==indexid, value:=strParseV(input_val, targetText)]
    DT[mod_id==modid&index_id==indexid, seq:=seqParam]
    return(DT)
  }else{
    mid<-DT[mod_id==modid&index_id==indexid&seq==1,]
    mid[, seq:=seqParam]
    mid[, key:=targetText]
    mid[, value:=strParseV(input_val, targetText)]
    DT<-rbind(DT, mid)
    return(DT)
  }
}




# 造个scoreband放在新的column里
# banding <- function(DT, columnValue, columnBand, bands=seq(0, 1, 0.1)){
#   DT[, eval(columnBand):=as.integer(cut(get(columnValue), quantile(get(columnValue), probs=bands, na.rm=T), include.lowest=TRUE))]
# }

grouping <- function(dataVector, groups=10){
  total <- length(dataVector)
  rankVector <- rank(as.numeric(dataVector), ties.method = "min")
  groupVector <- ceiling((rankVector * groups)/(total+1))
  midFrame <- data.table(cbind(dataVector, groupVector))
  groupMax <- midFrame[, 
                       .("groupMax"=max(dataVector)),
                       by="groupVector"]
  midFrame <- merge(midFrame, groupMax, by="groupVector")
  midFrame <- midFrame[!duplicated(dataVector),]
  return(midFrame[, c("dataVector", "groupMax"), with=F])
}

# 拉平data.table 
# test<-dcast.data.table(DT, mainID(用来left join的那个) ~ 变量名, fun.agg=max, value.var = "变量值")


##############################################行/列分析
# 把features的min/max/percentile/Nmiss etc.做成一个data.frame
featureAnalysis <- function(DT, exclude, cateConv=F) {
  result <- data.frame(VarName=character(),
                   sType=character(),
                   N=integer(),
                   Nmiss=integer(),
                   MissPctg=integer(),
                   Mean=double(),
                   Stdev=double(),
                   Min=double(),
                   Median=double(),
                   Max=double(),
                   P01=double(),
                   P05=double(),
                   P10=double(),
                   P25=double(),
                   P75=double(),
                   P90=double(),
                   P95=double(),
                   P99=double(),
                   stringsAsFactors=FALSE)


  for(varName in names(DT)){
    if(!(varName %in% exclude)){
      if(!is.factor(unlist(DT[, varName, with=F])) & 
         length(table(as.numeric(unlist(DT[, varName, with=F]))))>0){
        DT[, varName:=as.numeric(get(varName)), with=F]
      }
      print(varName)
      if(cateConv & nrow(as.data.table(table(DT[, varName, with=F],useNA="ifany")))<=10){
        #如果变量的所有可选值不大于10，可以选择转成categorical，randomForest不用转
        DT[, varName:=as.factor(get(varName)), with=F]
      }
      oneVar<-unlist(DT[, varName, with=F])
      if(typeof(oneVar) %in% c("double","integer")){
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
      }else{
        newRow<-data.frame(varName, 
                           typeof(oneVar),
                           length(oneVar), 
                           length(oneVar[is.na(oneVar)]),
                           length(oneVar[is.na(oneVar)])/length(oneVar),
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA 
        )
        names(newRow)<-names(result)
      }
      result<-rbind(result, newRow)
    }
  }
  names(result)<-c("VarName","sType","N","Nmiss","MissPctg","Mean","Stdev",
                   "Min","Median","Max","P01","P05","P10","P25","P75","P90","P95","P99")
  return(result)
}

sampleMissingAnalysis <- function(DT, IDRow) {
  result<-data.frame()
  for(i in 1:nrow(DT)){
    result<-rbind(result, c(unlist(DT[i, IDRow, with=F]), sum(is.na(DT[i, ]))))
  }
  names(result)<-c("rowNum","missingCnt")
  return(result)
}

printTable <- function(DT){
  for(name in names(DT)){
    print(name)
    print(table(DT[, name, with=F]))
  }
}
############################################## WoE理论系列
# categoricalDefault set to TRUE if want to automatically bin by the factor levels
# Sample binning data table: data.table(1, c(1,2,3))
woeCalc <- function(DT, X, Y, binning=NULL, events=1, nonevents=0, printResult=T){
  if(!(X %in% names(DT))){
    print("Not a valid variable!")
    return(DT)
  }
  woeVarName<-paste0("w_", X)
  DT[, woeVarName:=NULL, with=F]
  resultCalc<-DT[, c(X,Y), with=F]
  independent<-unlist(DT[, X, with=F])
  if(is.null(binning)){
    if(is.factor(independent)){
      resultCalc[, maxValue:=get(X)]
    }else{
      groupsDF <- grouping(independent, groups = 10)
      resultCalc <- merge(resultCalc, groupsDF, by.x=X, by.y="dataVector", all.x=T)
      setnames(resultCalc, "groupMax", "maxValue")
    }
  }else if(length(binning)==1){
    # 用于numeric的分组数自定义分组
    groupsDF <- grouping(independent, groups = binning)
    resultCalc <- merge(resultCalc, groupsDF, by.x=X, by.y="dataVector", all.x=T)
    setnames(resultCalc, "groupMax", "maxValue")
  }else if(is.data.frame(binning)){
    # 用于factor的枚举自定义分组
    names(binning)<-c("maxValue", "value")
    binning[, value:=as.character(value)]
    resultCalc[, X:=as.character(get(X)), with=F]
    resultCalc<-merge(resultCalc, binning, by.x=X, by.y="value", all.x=T)
  }else{
    # 用于numeric的断点自定义分组
    resultCalc[, maxValue:=cut(independent, binning)]
  }
  resultCalc[, isEvents:=ifelse(get(Y)==events, 1, 0)]
  resultCalc[, isNonEvents:=ifelse(get(Y)==nonevents, 1, 0)]
  result<-resultCalc[, .("TotalCnt"=.N, 
                         "EventsCnt"=sum(isEvents), 
                         "NonEventsCnt"=sum(isNonEvents)), 
                     by="maxValue"]
  result[, EventsPctg:=EventsCnt/sum(EventsCnt)]
  result[, NonEventsPctg:=NonEventsCnt/sum(NonEventsCnt)]
  result[, WoE:=log(EventsPctg/NonEventsPctg)]
  result[, IV:=sum((EventsPctg-NonEventsPctg)*WoE)]
  result[, varName:=X]
  result<-result[order(maxValue)]

  # 把woe放进原data frame
  DT<-cbind(DT, resultCalc[, "maxValue", with=F])
  DT<-merge(DT, result[, c("maxValue", "WoE"), with=F], by.x="maxValue", by.y="maxValue")
  setnames(DT, "WoE", woeVarName)
  DT[, maxValue:=NULL]
  if(printResult)
    print(result)
  
  # 返回一个list，包括master DT和result
  return(list(resultDT=DT, woeVar=result))
}


autoWoE <- function(DT, Y, binning=NULL, events=1, nonevents=0, exclude=c()){
  result<-data.frame()
  tooFew<-c()
  for(name in names(DT)){
    if(name %in% exclude)
      next
    print(name)
    if(name != Y){
      newResult<-woeCalc(DT, name, Y, binning = binning, events = events, nonevents = nonevents, printResult = F)
      if(is.list(newResult)){
        DT<-newResult$resultDT
        result<-rbind(result, newResult$woeVar)
      }else{
        tooFew<-append(tooFew, newResult)
      }
    }
  }
  returnList<-list(binResult=DT, woeTable=result, tooFewList=tooFew)
  return(returnList)
}


woeAssign <- function(DT, X, binningDF){
  
}

############## Impute
# table()的GUI版本
viewAllValues <- function(DT, column){
  result<-table(DT[, column, with=F], useNA = "ifany")
  result<-data.table(result)
  setnames(result, "V1", names(DT[, column, with=F]))
  View(result)
}

# 判断是否alpha-numeric，不是的话就是中文字符
isAlphaNum <- Vectorize(function(str){
  result<-ifelse(length(charToRaw(substr(str, 1, 1)))==1, 1, 0)
  return(result)
})

# 把NA什么的替换成0或者你想要的
naBlankInfer <- function(DT, column, inferFrom=c(NA, 'None', ''), inferTo=0){
  DT[get(column) %in% inferFrom, column:=as.character(inferTo), with=F]
}

ggImpute <- function(DT, fullImpute=TRUE, removeMassiveMissing=TRUE){
  tooMuchMissingList <- c()
  imputeValueTable <- data.frame(col=as.character("name"), imputeValue=as.character("Value"), stringsAsFactors = F)
  for(col in names(DT)){
    imputeColVec<-unlist(DT[, col, with=F])
    if(sum(is.na(imputeColVec))/sum(!is.na(imputeColVec))>=2/3){
      tooMuchMissingList<-append(tooMuchMissingList, col)
      if(removeMassiveMissing)
        next
    }
    if(fullImpute){
      if(is.numeric(imputeColVec)){
        if(length(unique(imputeColVec))>10){
          imputeValue<-median(imputeColVec, na.rm=T)
        }else if( sum(imputeColVec==Mode(imputeColVec), na.rm=T) < 0.6*sum(!is.na(imputeColVec)) ){
          imputeValue<-mean(imputeColVec, na.rm=T)
        }else{
          imputeValue<-Mode(imputeColVec)
        }
        naBlankInfer(DT, col, inferTo = imputeValue)
        imputeValueTable<-rbind(imputeValueTable, data.frame(col, imputeValue))
        next
      }else if(is.factor(imputeColVec)){
        if( sum(imputeColVec==Mode(imputeColVec), na.rm=T) < 0.6*sum(!is.na(imputeColVec)) ){
          imputeValue<-"999"
        }else{
          imputeValue<-Mode(imputeColVec)
        }
        naBlankInfer(DT, col, inferTo = imputeValue)
        imputeValueTable<-rbind(imputeValueTable, data.frame(col, imputeValue))
        next
      }else{
        imputeValue<-Mode(imputeColVec)
        naBlankInfer(DT, col, inferTo = imputeValue)
        imputeValueTable<-rbind(imputeValueTable, data.frame(col, imputeValue))
        next
      }
    }else{
      naBlankInfer(DT, col, inferTo = -99999)
    }
  }
  return(list(removeList=tooMuchMissingList, imputeValues=imputeValueTable))
}
  
typeConverter <- function(DT, convList, typeTo){
  for(varName in convList){
    if(typeTo=="character"){
      DT[, varName:=as.character(get(varName)), with=F]
    }else if(typeTo=="factor"){
      DT[, varName:=as.factor(get(varName)), with=F]
    }else if(typeTo=="numeric"){
      DT[, varName:=as.numeric(get(varName)), with=F]
    }
  }
}  





