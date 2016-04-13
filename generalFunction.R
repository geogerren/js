monthDiff <- Vectorize(function(startMth, endMth){
  return((as.numeric(substr(endMth,1,4))-as.numeric(substr(startMth,1,4)))*12+(as.numeric(substr(endMth,5,6))-as.numeric(substr(startMth,5,6))))
})

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

######################################################################################################## Parse 
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



###################################################################################################### grouping 
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


####################################################################################################行/列分析
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
      if(typeof(oneVar) %in% c("double","integer") & !is.factor(oneVar)){
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
########################################################################################################## WoE理论系列
# categoricalDefault set to TRUE if want to automatically bin by the factor levels
# Sample binning data table: data.table(1, c(1,2,3))
woeCalc <- function(DT, X, Y, binning=NULL, events=1, nonevents=0, printResult=T){
  isFactor <- FALSE
  isNumeric <- FALSE
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
      resultCalc[, type:="categorical"]
    }else{
      groupsDF <- grouping(independent, groups = 10)
      resultCalc <- merge(resultCalc, groupsDF, by.x=X, by.y="dataVector", all.x=T)
      
      setnames(resultCalc, "groupMax", "maxValue")
      resultCalc[, type:="continuous"]
    }
  }else if(length(binning)==1){
    # 用于numeric的分组数自定义分组
    groupsDF <- grouping(independent, groups = binning)
    resultCalc <- merge(resultCalc, groupsDF, by.x=X, by.y="dataVector", all.x=T)
    
    setnames(resultCalc, "groupMax", "maxValue")
    resultCalc[, type:="continuous"]
  }else if(is.data.frame(binning)){
    # 用于factor的枚举自定义分组
    isFactor <- TRUE
    names(binning)<-c("maxValue", "value")
    binning[, value:=as.character(value)]
    resultCalc[, X:=as.character(get(X)), with=F]
    resultCalc<-merge(resultCalc, binning, by.x=X, by.y="value", all.x=T)
    resultCalc[, type:="categorical"]
  }else{
    # 用于numeric的断点自定义分组
    isNumeric <- TRUE
    binDF <- data.table(value=independent, range=cut(independent, binning))
    binDFMax <- binDF[, .("groupMax"=max(value)), by="range"]
    binDF <- merge(binDF, binDFMax, by="range")
    binDF <- binDF[!duplicated(value),]
    resultCalc <- merge(resultCalc, binDF[, c("value", "groupMax"), with=F], by.x=X, by.y="value")
    
    setnames(resultCalc, "groupMax", "maxValue")
    resultCalc[, type:="continuous"]
  }
  resultCalc[, maxValue:=as.character(maxValue)]
  resultCalc[, isEvents:=ifelse(get(Y)==events, 1, 0)]
  resultCalc[, isNonEvents:=ifelse(get(Y)==nonevents, 1, 0)]
  result<-resultCalc[, .("TotalCnt"=.N, 
                         "EventsCnt"=sum(isEvents), 
                         "NonEventsCnt"=sum(isNonEvents)), 
                     by=c("maxValue","type")]
  result[, EventsPctg:=EventsCnt/sum(EventsCnt)]
  result[, NonEventsPctg:=NonEventsCnt/sum(NonEventsCnt)]
  result[, WoE:=log(EventsPctg/NonEventsPctg)]
  result[, IV:=sum((EventsPctg-NonEventsPctg)*WoE)]
  result[, varName:=X]
  result<-result[order(as.numeric(maxValue))]
  
  # 把woe放进原data frame
  DT<-cbind(DT, resultCalc[, "maxValue", with=F])
  DT<-merge(DT, result[, c("maxValue", "WoE"), with=F], by.x="maxValue", by.y="maxValue")
  setnames(DT, "WoE", woeVarName)
  DT[, maxValue:=NULL]

  if(printResult)
    print(result)
  
################################# 为了便于auto woe assigning
  #如果是factor变量,把woeVar list里面的maxValue(binning cutpoint)转回value(raw value), 使得后续assigning woe时候方便auto assign
  if(isFactor){
    result <- merge(result, binning, by.x="maxValue", by.y="maxValue")
    result[, maxValue:=value]
    result[, value:=NULL]
  }
  
  #如果是numeric变量,把woeVar list里面最大的maxValue(binning cutpoint)变成一个足够大的数. 为了与character类型统一,不能用Inf和1e34
  if(isNumeric){
    result[, maxValue:=as.numeric(maxValue)]
    result[maxValue==max(maxValue), maxValue:='9999999999']
  }
##############################################################  

  # 返回一个list，包括master DT和result
  return(list(resultDT=DT, woeVar=result))
}

woeAutoBin <- function(DT, Y, binning=NULL, events=1, nonevents=0, exclude=c()){
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
  setorderv(result, c("varName", "maxValue"), c(1, -1))
  returnList<-list(binResult=DT, woeTable=result, tooFewList=tooFew)
  return(returnList)
}

# binningDF requires type, varName, WoE at least
woeAssign <- function(DT, X, binningDF){
  assignWoE <- binningDF[varName==X]
  woeTransName <- paste0("w_", X)
  if(nrow(assignWoE)==0){
    print(paste0(X, " is not in binning list!"))
    return(DT)
  }
  if(assignWoE$type[1]=="continuous"){
    assignWoE[, maxValue:=as.numeric(maxValue)]
    setorderv(assignWoE, "maxValue", -1)
    for(i in 1:nrow(assignWoE)){
      DT[get(X) <= assignWoE[i,]$maxValue, woeTransName:=assignWoE[i,]$WoE, with=F]
    }
  }else{
    for(i in 1:nrow(assignWoE)){
      DT[get(X) == assignWoE[i,]$maxValue, woeTransName:=assignWoE[i,]$WoE, with=F]
    }
  }
  return(DT)
}

woeAssignAuto <- function(DT, binningDF){
  for(name in names(DT)){
    print(paste0("Assigning WoE to ", name))
    DT <- woeAssign(DT, name, binningDF)
  }
  return(DT)
}

#################################################
scoreCalc <- function(binningDF, lmModel, b=500, p=70, o=0.2){
  Beta <- lmModel$coefficients
  n <- length(Beta[names(Beta) != '(Intercept)'])
  Intercept <- Beta[names(Beta) == '(Intercept)']  
  Factor <- p/log(2)
  Offset <- b - p*log(o)/log(2)
  
  # append coefficient to binning data frame
  finalVar <- names(Beta[names(Beta) != '(Intercept)'])
  finalVar <- c("(Intercept)", substr(str_extract(finalVar, "w_.*"), 3, 999))
  scoreDF <- binningDF[varName %in% finalVar, ]
  coeff <- as.data.table(cbind(varName=finalVar, coefficient=Beta), stringsAsFactors = F)
  coeff[, coefficient:=as.numeric(coefficient)]
  scoreDF <-merge(scoreDF[, c("maxValue", "type", "WoE", "varName"), with=F], coeff, by.x="varName", by.y="varName", all.x=T)
  
  # 计算每个变量的评分, reverse the WoE part to make lower scores unfavorable
  scoreDF[, varScore:=Factor*(Intercept/n + coefficient*WoE) + Offset/n]
  print(paste0("factor= ", Factor, "; offset= ", Offset))
  return(scoreDF)
}

# scoringDF requires type, varName, varScore at least
scoreAssign <- function(DT, X, scoringDF){
  assignScore <- scoringDF[varName==X]
  scoreTransName <- paste0("s_", X)
  if(nrow(assignScore)==0){
    print(paste0(X, " is not in scoring list!"))
    return(DT)
  }
  print(paste0("Assigning score to ", X))
  if(assignScore$type[1]=="continuous"){
    assignScore[, maxValue:=as.numeric(maxValue)]
    setorderv(assignScore, "maxValue", -1)
    for(i in 1:nrow(assignScore)){
      DT[get(X) <= assignScore[i,]$maxValue, scoreTransName:=assignScore[i,]$varScore, with=F]
    }
  }else{
    for(i in 1:nrow(assignScore)){
      DT[get(X) == assignScore[i,]$maxValue, scoreTransName:=assignScore[i,]$varScore, with=F]
    }
  }
  DT[, "s_totalScore":=s_totalScore + get(scoreTransName), with=F]
  return(DT)
}

scoreAssignAuto <- function(DT, scoringDF){
  DT[, s_totalScore:=0]
  for(name in names(DT)){
    DT <- scoreAssign(DT, name, scoringDF)
  }
  return(DT)
}


######################################################################################################## Impute
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





