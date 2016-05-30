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

##########################################################################################
### 检查新评分卡的部署

# 我的分数
newScoreVar <- read.csv(paste0(boxdata, "testDataNew.csv"))
newScoreVar<-data.table(newScoreVar)
newScoreVar[, var_code:=as.character(var_code)]
newScoreVar[, var_val:=as.character(var_val)]
newScoreVarWide<-dcast.data.table(newScoreVar, applyid ~ var_code, fun.agg=max, value.var = "var_val")

newScoreVarWide[, provinceTransformed:=province]
newScoreVarWide[city %in% c("广州市","深圳市"), provinceTransformed:=city]
newScoreVarWide[, provinceTransformed:=as.factor(provinceTransformed)]
province_binningDF <- data.table(c("甘肃省",
                                   "上海市",
                                   "广州市",
                                   "北京市",
                                   "深圳市",
                                   "山东省",
                                   "云南省",
                                   "湖南省",
                                   "广东省",
                                   "河南省",
                                   "江苏省",
                                   "福建省",
                                   "辽宁省",
                                   "天津市",
                                   "安徽省",
                                   "广西",
                                   "四川省",
                                   "河北省",
                                   "湖北省",
                                   "重庆市",
                                   "贵州省",
                                   "浙江省",
                                   "海南省",
                                   "山西省",
                                   "江西省",
                                   "陕西省",
                                   "吉林省",
                                   "黑龙江省"), c(4,
                                              1,
                                              1,
                                              1,
                                              1,
                                              2,
                                              4,
                                              2,
                                              2,
                                              2,
                                              4,
                                              3,
                                              3,
                                              4,
                                              3,
                                              3,
                                              4,
                                              4,
                                              4,
                                              3,
                                              2,
                                              4,
                                              4,
                                              2,
                                              4,
                                              4,
                                              3,
                                              4))
names(province_binningDF) <- c("maxvalue","provinceTransformedBin")
newScoreVarWide<-merge(newScoreVarWide, province_binningDF, by.x="provinceTransformed", by.y="maxvalue", all.x = T)
newScoreVarWide[, provinceTransformedBin:=as.factor(provinceTransformedBin)]

newScoreVarWide[, c("provinceTransformed","city","province"):=NULL]

# 海哥变量
newScoreVarWide[, applyHour:=as.numeric(applyHour)]
newScoreVarWide[, applyTimeSegment:=ifelse(applyHour>=1 & applyHour<=6, "3", ifelse(applyHour>=9 & applyHour<=20, "1", "2"))]

setnames(newScoreVarWide, c("foodConsumeL6M","multiBorrowAmountP6"), c("RFM_6_var12","RFM_6_var19"))

newScoreVarWideImpute<-ggImpute(newScoreVarWide, fullImpute = F, removeMassiveMissing = F)
featureAnalysis(newScoreVarWide, exclude="applyid")
typeConverter(newScoreVarWide, c("avgMonthCall","consumeFreg","RFM_6_var12","longTimeShutdown","RFM_6_var19","zhimaScore"), "numeric")

newScoreWideNew<-scoreAssignAuto(newScoreVarWide, scoreCard, intercept=508)

# 引擎的分数
newScore<-read.csv(paste0(boxdata, "testResultNew.csv"))
newScore<-data.table(newScore)
newScore[, result:=as.character(result)]
newScoreWide<-dcast.data.table(newScore, applyid ~ reason, fun.agg=max, value.var = "result")
# setnames(newScoreWide,
#          c("近6个月餐饮类消费金额", "近6个月疑似多平台借贷金额",
#                          "平均每月通话次数","申请时间","省市信息",
#                          "消费频率","长时间关机","芝麻分"),
#          c("RFM_6_var12","RFM_6_var19","avgMonthCall","applyTimeSegment",
#            "provinceTransformedBin","consumeFreg","longTimeShutdown","zhimaScore"))
# featureAnalysis(newScoreWide, exclude = c("applyid"))
# newScoreWide<-newScoreWide[, c("applyid","finalScore"), with=F]

jointScore<-merge(newScoreWideNew, newScoreWide, by='applyid')


jointScore[, `总分`:=as.numeric(`总分`)]
jointScore[, check:=ifelse(abs(s_totalScore-`总分`)>5,1,0)]

View(jointScore[check==1,])
