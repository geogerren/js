monthDiff <- function(startMth, endMth){
  return((as.numeric(substr(endMth,1,4))-as.numeric(substr(startMth,1,4)))*12+(as.numeric(substr(endMth,5,6))-as.numeric(substr(startMth,5,6))))
}

colonParser <- function(obj, targetText) {
  sub<-str_extract(obj, paste0(targetText, ".*<"))
  positionStart<-str_locate(sub, ":")[1]+1
  positionEnd<-str_locate(sub, "\\(<")[1]-1
  return(substr(sub,positionStart,positionEnd))
}

featureGen <- function(DT, modid, indexid, targetText) {
  DT[mod_id==modid&index_id==indexid, key:=targetText]
  DT[mod_id==modid&index_id==indexid, value:=colonParser(input_val, targetText)]
}
