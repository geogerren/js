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

# featureAnalysis <- function(DT) {
#   oneVariable<-DT[, 1, with=F]
#   result<-data.table(names(oneVariable), typeof(unlist(oneVariable[1])),
#                      nrow(oneVariable), mean(oneVariable), sd(oneVariable)), 
#                      sum(is.na(oneVariable)), max(oneVariable), min(oneVariable)), 
#                      quantile(unlist(oneVariable), c(.01)), quantile(unlist(oneVariable), c(.05)),
#                      quantile(unlist(oneVariable), c(.25)), quantile(unlist(oneVariable), c(.50)),
#                      quantile(unlist(oneVariable), c(.75)), quantile(unlist(oneVariable), c(.99)))
#   names(result)<-c("VarName", "Type")
#   for(i in 2:length(names(DT))){
#     
#   }
# }

