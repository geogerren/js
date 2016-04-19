########################################################################################################## WoE理论系列
# categoricalDefault set to TRUE if want to automatically bin by the factor levels
# Sample binning data table: data.table(1, c(1,2,3))
woeCalc <- function(DT, X, Y, uniqueID="financingprojectid", binning=NULL, naZeroWoE=F, events=1, nonevents=0, printResult=T){
  isFactor <- FALSE
  isNumeric <- FALSE
  if(!(X %in% names(DT))){
    print("Not a valid variable!")
    return(DT)
  }
  woeVarName<-paste0("w_", X)
  DT[, woeVarName:=NULL, with=F]
  resultCalc<-DT[, c(X,Y,uniqueID), with=F]
  independent<-unlist(DT[, X, with=F])
  if(is.null(binning)){
    if(is.factor(independent)){
      resultCalc[, maxValue:=get(X)]
      resultCalc[, type:="categorical"]
    }else{
      independent<-signif(independent, digits = 2)
      groupsDF <- grouping(independent, groups = 10)
      resultCalc[, X:=signif(get(X),2), with=F]
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
    independent<-signif(independent, digits = 2)
    
    binDF <- data.table(value=independent, range=cut(independent, binning))
    binDFMax <- binDF[, .("groupMax"=max(value)), by="range"]
    binDF <- merge(binDF, binDFMax, by="range")
    binDF <- binDF[!duplicated(value),]

    resultCalc[, X:=signif(get(X),2), with=F]
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
  
  # 要是需要给missing的WoE全都assign成0, 就在这一步
  if(naZeroWoE){
    result[maxValue<MISSING_DEFAULT+1 | maxValue==MISSING_DEFAULT, WoE:=0]
  }
  
  result[, IV:=sum((EventsPctg-NonEventsPctg)*WoE)]
  result[, varName:=X]
  result<-result[order(as.numeric(maxValue))]
  

  # 把woe放进原data frame
  DT<-merge(DT, resultCalc[, c(uniqueID, "maxValue"), with=F], by=uniqueID, all.x=T)
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

################################################# Assigning scores
# input: assiningDF from woeCalc/woeAutoBin; glm model from model development step
# output: assiningDF with scores for each score range attached aside woe
scoreCalc <- function(binningDF, lmModel, neutralForMissing=T, b=500, p=50, o=0.2){
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
  scoreDF <-merge(scoreDF[, c("maxValue", "type", "TotalCnt","WoE", "varName"), with=F], coeff, by.x="varName", by.y="varName", all.x=T)
  
  # 计算每个变量的评分, reverse the WoE part to make lower scores unfavorable
  if(neutralForMissing){
    scoreDF[, varScore:= ifelse(maxValue==MISSING_DEFAULT, 0, -1)*coefficient*WoE*Factor]
  }else{
    scoreDF[, varScore:= -coefficient*WoE*Factor]
  }
  print(paste0("factor= ", Factor, "; 
               offset= ", Offset, ";
               Intercept= ", -Factor*Intercept+Offset))
  return(list(scoreDF=scoreDF, Intercept=-Factor*Intercept+Offset))
}

# scoringDF requires type, varName, varScore at least
scoreAssign <- function(DT, X, scoringDF){
  assignScore <- scoringDF[varName==X]
  scoreTransName <- paste0("s_", X)
  if(nrow(assignScore)==0){
    print(paste0(X, " is not in scoring list."))
    return(DT)
  }
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
  print(paste0("############## Score for ", X, " assigned #####################"))
  
  DT[, "s_totalScore":=s_totalScore + get(scoreTransName), with=F]
  return(DT)
}

scoreAssignAuto <- function(DT, scoringDF, intercept){
  DT[, s_totalScore:=intercept]
  for(name in names(DT)){
    DT <- scoreAssign(DT, name, scoringDF)
  }
  return(DT)
}





