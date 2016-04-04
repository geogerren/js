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
banding <- function(DT, columnValue, columnBand, bands=seq(0, 1, 0.1)){
  DT[, eval(columnBand):=as.integer(cut(get(columnValue), quantile(get(columnValue), probs=bands, na.rm=T), include.lowest=TRUE))]
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
##############################################WoE计算
# woeCalc <- function(DT, X, Y, binning=NULL, events=1, nonevents=0){
#   resultCalc<-DT[, c(X,Y), with=F]
#   independent<-unlist(DT[, X, with=F])
#   if(is.null(binning)){
#     tryCatch(resultCalc[, range:=cut_number(independent, 2)], error=function(e) e, 
#              finally=return(X))
#   }else if(is.numeric(binning)){
#     tryCatch(resultCalc[, range:=cut_number(independent, binning)], error=function(e) e, 
#              finally=return(X))
#   }else{
#       resultCalc[, range:=cut(independent, binning)]
#   }
#   resultCalc[, isEvents:=ifelse(get(Y)==events, 1, 0)]
#   resultCalc[, isNonEvents:=ifelse(get(Y)==nonevents, 1, 0)]
#   result<-resultCalc[, .("TotalCnt"=.N, 
#                          "EventsCnt"=sum(isEvents), 
#                          "NonEventsCnt"=sum(isNonEvents)), 
#                      by="range"]
#   result[, EventsPctg:=EventsCnt/sum(EventsCnt)]
#   result[, NonEventsPctg:=NonEventsCnt/sum(NonEventsCnt)]
#   result[, WoE:=log(NonEventsPctg/EventsPctg)]
#   result[, IV:=sum((NonEventsPctg-EventsPctg)*WoE)]
#   result[, varName:=X]
#   result<-result[order(range)]
#   return(result)
# }

woeCalc <- function(DT, X, Y, binning=NULL, events=1, nonevents=0){
  resultCalc<-DT[, c(X,Y), with=F]
  independent<-unlist(DT[, X, with=F])
  if(is.null(binning)){
    tryCatch(resultCalc[, range:=cut_number(independent, 2)], error=function(e) e, 
             finally=return(X))
  }else if(length(binning)==1){
    resultCalc[, range:=cut_number(independent, binning)]
  }else{
    resultCalc[, range:=cut(independent, binning)]
  }
  resultCalc[, isEvents:=ifelse(get(Y)==events, 1, 0)]
  resultCalc[, isNonEvents:=ifelse(get(Y)==nonevents, 1, 0)]
  result<-resultCalc[, .("TotalCnt"=.N, 
                         "EventsCnt"=sum(isEvents), 
                         "NonEventsCnt"=sum(isNonEvents)), 
                     by="range"]
  result[, EventsPctg:=EventsCnt/sum(EventsCnt)]
  result[, NonEventsPctg:=NonEventsCnt/sum(NonEventsCnt)]
  result[, WoE:=log(NonEventsPctg/EventsPctg)]
  result[, IV:=sum((NonEventsPctg-EventsPctg)*WoE)]
  result[, varName:=X]
  result<-result[order(range)]
  return(result)
}

woeTable <- function(DT, Y, binning=NULL, events=1, nonevents=0){
  result<-data.frame()
  tooFew<-c()
  for(name in names(DT)){
    if(name != Y){
      newResult<-woeCalc(DT, name, Y, binning = binning, events = events, nonevents = nonevents)
      if(is.data.frame(newResult)){
        result<-rbind(result, newResult)
      }else{
        tooFew<-append(tooFew, newResult)
      }
    }
  }
  returnList<-list(infoTable=result, tooFewList=tooFew)
  return(returnList)
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





