dataTrain <- read.csv(paste0(boxdata, "dumiao_data_set_new.csv"))
dataTrain <- data.table(dataTrain)

# local & tier1 = 1, non-local & tier1 = 2, local & non-tier1 = 3, non-local & non-tier1 = 4
dataTrain[, localFriendsTransformed:=ifelse(localFriends==-1, "-99999", 
                                            ifelse(province %in% c("北京市","上海市") | 
                                                     city %in% c("广州市","深圳市"),
                                                   ifelse(localFriends==1, "1", "2"), 
                                                   ifelse(localFriends==1, "3", "4")
                                            )
)]
dataTrain[, provinceTransformed:=province]
dataTrain[city %in% c("广州市","深圳市"), provinceTransformed:=city]

dataTrain[, financingprojectid:=fi_9999999cingprojectid]

dataTrainTH <- dataTrain[, c("RFM_6_var19","RFM_6_var12","applyTimeSegment","consumeFreg",
                             "longTimeShutdown","unionpayPosConsumeCityRank","zhimaScore","avgMonthCall",
                             "localFriendsTransformed","provinceTransformed","flgDPD","financingprojectid"
), with=F]


# dataTrain$RFM_6_var19
# dataTrain$RFM_6_var12
# dataTrain$applyTimeSegment
# dataTrain$consumeFreg
# dataTrain$longTimeShutdown
# dataTrain$unionpayPosConsumeCityRank
# dataTrain$zhimaScore
# dataTrain$avgMonthCall
# dataTrain$localFriendsTransformed
# dataTrain$provinceTransformed
# dataTrain$flgDPD

dataTrainTHImpute<-ggImpute(dataTrainTH, fullImpute = F, removeMassiveMissing = F)

##########################################################################################
assigningDF <- data.frame()

RFM_6_var12_list<-woeCalc(dataTrainTH, "RFM_6_var12","flgDPD", binning=c(-Inf, -999, 1, Inf))
dataTrainTH <- RFM_6_var12_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var12_list$woeVar)


RFM_6_var19_list<-woeCalc(dataTrainTH, "RFM_6_var19","flgDPD", binning=c(-Inf, -999, 0, Inf))
dataTrainTH <- RFM_6_var19_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var19_list$woeVar)


applyTimeSegment_list<-woeCalc(dataTrainTH, "applyTimeSegment","flgDPD")
dataTrainTH <- applyTimeSegment_list$resultDT
assigningDF <- rbind(assigningDF, applyTimeSegment_list$woeVar)


consumeFreg_list<-woeCalc(dataTrainTH, "consumeFreg","flgDPD", binning = c(-Inf, -999, 2, 5, Inf))
dataTrainTH <- consumeFreg_list$resultDT
assigningDF <- rbind(assigningDF, consumeFreg_list$woeVar)


longTimeShutdown_list<-woeCalc(dataTrainTH, "longTimeShutdown","flgDPD")
dataTrainTH <- longTimeShutdown_list$resultDT
assigningDF <- rbind(assigningDF, longTimeShutdown_list$woeVar)


unionpayPosConsumeCityRank_list<-woeCalc(dataTrainTH, "unionpayPosConsumeCityRank","flgDPD", binning=c(-Inf, -999, 0, 1, Inf))
dataTrainTH <- unionpayPosConsumeCityRank_list$resultDT
assigningDF <- rbind(assigningDF, unionpayPosConsumeCityRank_list$woeVar)


zhimaScore_list<-woeCalc(dataTrainTH, "zhimaScore","flgDPD", binning = c(-Inf, 660, 700, Inf), naZeroWoE=T)
dataTrainTH <- zhimaScore_list$resultDT
assigningDF <- rbind(assigningDF, zhimaScore_list$woeVar)


avgMonthCall_list<-woeCalc(dataTrainTH, "avgMonthCall","flgDPD", binning = c(-Inf, 150, 320, Inf))
dataTrainTH <- avgMonthCall_list$resultDT
assigningDF <- rbind(assigningDF, avgMonthCall_list$woeVar)


localFriendsTransformed_list<-woeCalc(dataTrainTH, "localFriendsTransformed","flgDPD")
dataTrainTH <- localFriendsTransformed_list$resultDT
assigningDF <- rbind(assigningDF, localFriendsTransformed_list$woeVar)



dataTrainTH[, provinceTransformed:=as.factor(provinceTransformed)]
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
dataTrainTH<-merge(dataTrainTH, province_binningDF, by.x="provinceTransformed", by.y="maxvalue", all.x = T)

dataTrainTH[, provinceTransformedBin:=as.factor(provinceTransformedBin)]
provinceTransformed_list<-woeCalc(dataTrainTH, "provinceTransformedBin","flgDPD")
dataTrainTH <- provinceTransformed_list$resultDT
assigningDF <- rbind(assigningDF, provinceTransformed_list$woeVar)

##########################################################################################
binTrain<-dataTrainTH[, c("w_RFM_6_var12","w_RFM_6_var19","w_applyTimeSegment","w_consumeFreg",
                "w_longTimeShutdown","w_zhimaScore",
                "w_avgMonthCall",
                "w_provinceTransformedBin","flgDPD"), with=F]
rawTrain<-dataTrainTH[, c("RFM_6_var19","RFM_6_var12","applyTimeSegment","consumeFreg","longTimeShutdown","unionpayPosConsumeCityRank", "zhimaScore",
          "avgMonthCall","flgDPD","provinceTransformedBin"), with=F]


allDataBin <- copy(binTrain)
allDataRaw <- copy(rawTrain)
set.seed(2016)
for(i in 1:nrow(allDataBin)){
  allDataBin[i, bucket:=ceiling(runif(1, 0, 5))]
}

train <- allDataBin[bucket %in% c(1,2,3,4),]
test <- allDataBin[bucket == 5,]


train[, bucket:=NULL]
mfull<-glm(flgDPD~., data=train, family=binomial())

summary(mfull)



train$score <- predict(mfull, type='response', train)
test$score <- predict(mfull, type='response', test)

ks_stat(train$flgDPD, train$score)
ks_stat(test$flgDPD, test$score)



mBinFinal<-mfull
