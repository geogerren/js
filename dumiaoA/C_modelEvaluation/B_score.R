ggImpute(oos, fullImpute = F, removeMassiveMissing = F)
featureAnalysis(oos, exclude = c("service_id", "longTimeShutdown"))


scoreCard<-read.csv(paste0(boxdata, "scoreCard.csv"))
scoreCard<-data.table(scoreCard)
scoreCard[maxValue==-100000, maxValue:=-99999]
oos<-data.table(oos)

scoreCard[, X:=NULL]

scoredOOS<-scoreAssignAuto(oos, scoreCard, intercept=508)



scoredOOS[, scoreBand:=ifelse(s_totalScore<466, 1, 
                               ifelse(s_totalScore<523, 2, 
                                      ifelse(s_totalScore<583, 3, 
                                             ifelse(s_totalScore<637, 4, 5))))]



scoredOOS[, applyid:=as.character(applyid)]

scoredOOS <- merge(scoredOOS, financingprojectidMapping, by.x="applyid",by.y="apply_id",all.x=T)

scoredOOS <- merge(scoredOOS, EverDPD90, by.x="financingprojectid", by.y="project_id", all.x=T)

scoredOOS <- merge(scoredOOS, target, by.x="financingprojectid", by.y="project_id", all.x=T)






View(scoredOOS)
View(scoredOOS[, .("cnt"=.N),by=c("scoreBand")])

write.csv(scoredOOS, paste0(boxdata, "scoredOOS.csv"))


newScore<-read.csv(paste0(boxdata, "newscoretest.csv"))
newScore<-data.table(newScore)
newScore[, result:=as.numeric(result)]
newScoreWide<-dcast.data.table(newScore, applyid ~ reason, fun.agg=max, value.var = "result")
setnames(newScoreWide, 
         c("近6个月餐饮类消费金额", "近6个月疑似多平台借贷金额", 
                         "平均每月通话次数","申请时间","省市信息",
                         "消费频率","长时间关机","芝麻分"), 
         c("RFM_6_var12","RFM_6_var19","avgMonthCall","applyTimeSegment",
           "provinceTransformedBin","consumeFreg","longTimeShutdown","zhimaScore"))
newScoreWide[, applyTimeSegment:=as.numeric(applyTimeSegment)]
newScoreWide[, provinceTransformedBin:=as.factor(as.character(provinceTransformedBin))]
# newScoreWide[, longTimeShutdown:=as.factor(as.character(longTimeShutdown))]

scoredNew<-scoreAssignAuto(newScoreWide, scoreCard, intercept=508)


