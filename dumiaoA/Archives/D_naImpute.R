# IMPROVE
# need to provide a list to impute, then automate that

#################################################################################################
##########################################################
# training data imputation

# viewAllValues(trainDataFinal, 5)
naBlankInfer(trainDataFinal, "amuseConsumeFreq", inferTo= 0)

# viewAllValues(trainDataFinal, 6)
naBlankInfer(trainDataFinal, "applicantContact", inferTo= -1)

# viewAllValues(trainDataFinal, 9)

# viewAllValues(trainDataFinal, 10)
naBlankInfer(trainDataFinal, "callEcpNum", inferTo= 0)


# viewAllValues(trainDataFinal, 12)
# trainDataFinal[, callNetLoanBlank:=NULL]

# viewAllValues(trainDataFinal, "called5")
naBlankInfer(trainDataFinal, "called5", inferTo= 6)

# viewAllValues(trainDataFinal, 14)
# trainDataFinal[, cardClass:=NULL]

# viewAllValues(trainDataFinal, 15)
naBlankInfer(trainDataFinal, "cardTerm", inferTo= -1)

# viewAllValues(trainDataFinal, 16)
naBlankInfer(trainDataFinal, "cardType", inferTo= -1)

# viewAllValues(trainDataFinal, 17)
naBlankInfer(trainDataFinal, "cellphoneAuth", inferTo= 1)

# viewAllValues(trainDataFinal, 19)
trainDataFinal[, consumeFreg:=as.numeric(consumeFreg)]
trainDataFinal[, consumeFreg:=round(consumeFreg)]
# Mode(trainDataFinal$consumeFreg)
# median(trainDataFinal$consumeFreg, na.rm=T)
naBlankInfer(trainDataFinal, "consumeFreg", inferTo= 2)


# viewAllValues(trainDataFinal, 20)
trainDataFinal[, consumeLineRate:=as.numeric(consumeLineRate)]
# median(trainDataFinal$consumeLineRate, na.rm=T)
naBlankInfer(trainDataFinal, "consumeLineRate", inferTo= 0.00428571428571)

# viewAllValues(trainDataFinal, 21)
trainDataFinal[, consumeTop:=as.numeric(consumeTop)]
# median(trainDataFinal$consumeTop, na.rm=T)
naBlankInfer(trainDataFinal, "consumeTop", inferTo= 15)

# viewAllValues(trainDataFinal, 22)
naBlankInfer(trainDataFinal, "creditCashAvg", inferTo= 0)

# viewAllValues(trainDataFinal, 23)
naBlankInfer(trainDataFinal, "creditCashFreq", inferTo= 0)

# viewAllValues(trainDataFinal, 24)
naBlankInfer(trainDataFinal, "creditWD3Months", inferTo= 0)

# viewAllValues(trainDataFinal, 25)
naBlankInfer(trainDataFinal, "creditWDAvg", inferTo= 0)

# viewAllValues(trainDataFinal, 26)
naBlankInfer(trainDataFinal, "creditWDFreq", inferTo= 0)

# 缺失的通通算没车，declareCar在以后会变成zhiceCar
# viewAllValues(trainDataFinal, 28)
trainDataFinal[, zhiceCar:=declareCar]
trainDataFinal[, declareCar:=NULL]
naBlankInfer(trainDataFinal, "zhiceCar", inferTo= 0)


# viewAllValues(trainDataFinal, 29)
# trainDataFinal[, ecpPhoneTag:=NULL]

# viewAllValues(trainDataFinal, 30)
# trainDataFinal[, ecp_eachother:=NULL]

# viewAllValues(trainDataFinal, 31)
naBlankInfer(trainDataFinal, "goOut120", inferTo= -1)

#########################################
# 上海资信系列impute
# viewAllValues(trainDataFinal, 32)
naBlankInfer(trainDataFinal, "hasShCISReport", inferTo= 0)
trainDataFinal[hasShCISReport=='0', c("shCISCreditLine","shCISCurrentOverDueNum",
                                      "shCISLoanQueryNo_3month", "shCISOverdualNum_3month", 
                                      "shCISOverdualNum_24month", "shCISMaxOverdual_24month"):=0]




# viewAllValues(trainDataFinal, 34)
# trainDataFinal[, hightRiskTransAvg1:=NULL]

# viewAllValues(trainDataFinal, 36)
# trainDataFinal[, hightRiskTransNum1:=NULL]

# viewAllValues(trainDataFinal, 37)
# trainDataFinal[, hightRiskTransNum6:=NULL]

# viewAllValues(trainDataFinal, 38)
# trainDataFinal[, inBlanklist:=NULL]

# viewAllValues(trainDataFinal, 39)
# trainDataFinal[, inJulixinBlanklist:=NULL]

# viewAllValues(trainDataFinal, 40)
# trainDataFinal[, inZhimaBlank:=NULL]

# viewAllValues(trainDataFinal, 41)
# trainDataFinal[, juxinliSuccess:=NULL]

# viewAllValues(trainDataFinal, 43)
naBlankInfer(trainDataFinal, "lastMonthOverdrawNum", inferTo= 0)

# viewAllValues(trainDataFinal, "loansCalls1")
# median(as.numeric(trainDataFinal$loansCalls1), na.rm=T)
naBlankInfer(trainDataFinal, "loansCalls1", inferTo= 5)

# viewAllValues(trainDataFinal, 46)
# median(as.numeric(trainDataFinal$loansCalls3), na.rm=T)
naBlankInfer(trainDataFinal, "loansCalls3", inferTo= 5)

# viewAllValues(trainDataFinal, 47)
# trainDataFinal[, local1year:=NULL]

# viewAllValues(trainDataFinal, 49)
naBlankInfer(trainDataFinal, "longTimeShutdown", inferTo= 0)

# viewAllValues(trainDataFinal, 53)
trainDataFinal[, month62ConsumeAvg:=as.numeric(month62ConsumeAvg)]
# median(trainDataFinal$month62ConsumeAvg, na.rm=T)
naBlankInfer(trainDataFinal, "month62ConsumeAvg", inferTo= 4878.712)

# viewAllValues(trainDataFinal, 55)
# trainDataFinal[, multiBorrowNumP1:=NULL]

# viewAllValues(trainDataFinal, 56)
# median(trainDataFinal$networkTime6, na.rm=T)
naBlankInfer(trainDataFinal, "networkTime6", inferTo= 182)

# viewAllValues(trainDataFinal, 57)
naBlankInfer(trainDataFinal, "nightConsumeNum", inferTo= 0)

# viewAllValues(trainDataFinal, 58)
naBlankInfer(trainDataFinal, "noNeedMobileAuthCheck", inferTo= 0)

# viewAllValues(trainDataFinal, 59)
naBlankInfer(trainDataFinal, "normalContact", inferTo= 0)

# viewAllValues(trainDataFinal, 60)
naBlankInfer(trainDataFinal, "post12PMFeeAmount", inferTo= 0)

# viewAllValues(trainDataFinal, 61)
naBlankInfer(trainDataFinal, "post12PMFeeNum", inferTo= 0)

# viewAllValues(trainDataFinal, "post6MonthOverdrawNum")
naBlankInfer(trainDataFinal, "post6MonthOverdrawNum", inferTo= 0)

# viewAllValues(trainDataFinal, 63)
trainDataFinal[, postMonthConsumeFreg:=as.numeric(postMonthConsumeFreg)]
# median(trainDataFinal$postMonthConsumeFreg, na.rm=T)
naBlankInfer(trainDataFinal, "postMonthConsumeFreg", inferTo= 4)

# viewAllValues(trainDataFinal, 64)

# viewAllValues(trainDataFinal, "shCISCreditLine")
naBlankInfer(trainDataFinal, "shCISCreditLine", inferTo = 0)

# viewAllValues(trainDataFinal, 66)
# trainDataFinal[, shCISCurrentOverDueNum:=NULL]
# viewAllValues(trainDataFinal, 68)
# trainDataFinal[, shCISMaxOverdual_24month:=NULL]
# viewAllValues(trainDataFinal, 69)
# trainDataFinal[, shCISOverdualNum_24month:=NULL]
# viewAllValues(trainDataFinal, 70)
# trainDataFinal[, shCISOverdualNum_3month:=NULL]
# viewAllValues(trainDataFinal, 71)
# median(as.numeric(trainDataFinal$tachEcp), na.rm=T)
naBlankInfer(trainDataFinal, "tachEcp", inferTo= 108.5)

# viewAllValues(trainDataFinal, 72)
naBlankInfer(trainDataFinal, "thisCityIsTop3", inferTo= 1)

# viewAllValues(trainDataFinal, 73)
naBlankInfer(trainDataFinal, "transFalsePast6", inferTo= 0)

# viewAllValues(trainDataFinal, 74)
naBlankInfer(trainDataFinal, "transFalsePastMonth", inferTo= 0)

# viewAllValues(trainDataFinal, 75)
naBlankInfer(trainDataFinal, "trustAddr", inferTo= 1)

# viewAllValues(trainDataFinal, 76)
# trainDataFinal[, trustIP:=NULL]

# viewAllValues(trainDataFinal, 77)
# trainDataFinal[, unexpectedApplyTime:=NULL]

# viewAllValues(trainDataFinal, 79)
trainDataFinal[, zhiceHouse:=unionpayHouse]
# trainDataFinal[, unionpayHouse:=NULL]
naBlankInfer(trainDataFinal, "zhiceHouse", inferTo= 0)

# viewAllValues(trainDataFinal, 80)
# trainDataFinal[, unionpayIdCardNameCheck:=NULL]

# viewAllValues(trainDataFinal, 81)
# trainDataFinal[, unionpayMobileCardCheck:=NULL]

# viewAllValues(trainDataFinal, 82)
# trainDataFinal[, unionpayNameCardCheck:=NULL]

# viewAllValues(trainDataFinal, 83)
naBlankInfer(trainDataFinal, "unionpayPosConsumeCityRank", inferTo= 1)

# viewAllValues(trainDataFinal, 84)
trainDataFinal[, useCardAmountAvg:=as.numeric(useCardAmountAvg)]
# median(trainDataFinal$useCardAmountAvg, na.rm=T)
naBlankInfer(trainDataFinal, "useCardAmountAvg", inferTo= 5006.55)

# viewAllValues(trainDataFinal, 85)
trainDataFinal[, useCardLastTime:=as.numeric(useCardLastTime)]
# median(trainDataFinal$useCardLastTime, na.rm=T)
naBlankInfer(trainDataFinal, "useCardLastTime", inferTo= 31)

# viewAllValues(trainDataFinal, 86)
# median(as.numeric(trainDataFinal$useCardNumPost6), na.rm=T)
naBlankInfer(trainDataFinal, "useCardNumPost6", inferTo= 5)

# viewAllValues(trainDataFinal, 87)
trainDataFinal[, useCardPM:=as.numeric(useCardPM)]
# median(trainDataFinal$useCardPM, na.rm=T)
naBlankInfer(trainDataFinal, "useCardPM", inferTo= 4.291667)

# viewAllValues(trainDataFinal, 88)
trainDataFinal[, useCardSumRank:=as.numeric(useCardSumRank)]
# median(trainDataFinal$useCardSumRank, na.rm=T)
naBlankInfer(trainDataFinal, "useCardSumRank", inferTo= 24.16667)

# viewAllValues(trainDataFinal, 89)
naBlankInfer(trainDataFinal, "workCondition", inferTo= -1)

# viewAllValues(trainDataFinal, 94)

#########################################################################################
# featureAnalAfterImpute<-featureAnalysis(trainDataFinal, 
#                                         exclude=c("financingprojectid",
#                                                   "flgDPD","flgTest","createtime.1"))


trainDataFinal[, c("callNetLoanBlank", "cardClass", "ecpPhoneTag", "ecp_eachother", "hightRiskTransAvg1", 
               "hightRiskTransNum1", "hightRiskTransNum6", "inBlanklist", "inJulixinBlanklist", 
               "inZhimaBlank", "juxinliSuccess", "local1year", "multiBorrowNumP1", "shCISCurrentOverDueNum", 
               "shCISMaxOverdual_24month", "shCISOverdualNum_24month", "shCISOverdualNum_3month", "trustIP", 
               "unexpectedApplyTime", "unionpayHouse", "unionpayIdCardNameCheck", "unionpayMobileCardCheck",
               "unionpayNameCardCheck", "hightRiskTransAvg6", "mateNum", "flgTest", "flgTrainTest"):=NULL]
# 
# featureAnalAfterSingleValueRemove<-featureAnalysis(trainDataFinal, 
#                                         exclude=c("financingprojectid","flgDPD","flgTest"))

##########补充impute其他未填满的
# median(as.numeric(trainDataFinal$avgMonthCall), na.rm = T)
naBlankInfer(trainDataFinal, "avgMonthCall", inferTo= 180)
naBlankInfer(trainDataFinal, "zhiceHouse", inferTo= 0)
naBlankInfer(trainDataFinal, "trustAddr", inferTo= 1)
naBlankInfer(trainDataFinal, "cardType", inferTo= -1)


featureAnalTrain<-featureAnalysis(trainDataFinal, exclude=c("financingprojectid","flgDPD","flgTest"))

##########################################################

trainDataFinal[, flgDPD:=as.factor(flgDPD)]
trainDataFinal[, c("NA", "financingprojectid", "rule_name"):=NULL]


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
##########################################################
# test data imputation

# viewAllValues(testData, 5)
naBlankInfer(testData, "amuseConsumeFreq", inferTo= 0)

# viewAllValues(testData, 6)
naBlankInfer(testData, "applicantContact", inferTo= -1)

# viewAllValues(testData, 9)

# viewAllValues(testData, 10)
naBlankInfer(testData, "callEcpNum", inferTo= 0)


# viewAllValues(testData, 12)
# testData[, callNetLoanBlank:=NULL]

# viewAllValues(testData, "called5")
naBlankInfer(testData, "called5", inferTo= 6)

# viewAllValues(testData, 14)
# testData[, cardClass:=NULL]

# viewAllValues(testData, 15)
naBlankInfer(testData, "cardTerm", inferTo= -1)

# viewAllValues(testData, 16)
naBlankInfer(testData, "cardType", inferTo= -1)

# viewAllValues(testData, 17)
naBlankInfer(testData, "cellphoneAuth", inferTo= 1)

# viewAllValues(testData, 19)
testData[, consumeFreg:=as.numeric(consumeFreg)]
testData[, consumeFreg:=round(consumeFreg)]
# Mode(testData$consumeFreg)
# median(testData$consumeFreg, na.rm=T)
naBlankInfer(testData, "consumeFreg", inferTo= 2)


# viewAllValues(testData, 20)
testData[, consumeLineRate:=as.numeric(consumeLineRate)]
# median(testData$consumeLineRate, na.rm=T)
naBlankInfer(testData, "consumeLineRate", inferTo= 0.00428571428571)

# viewAllValues(testData, 21)
testData[, consumeTop:=as.numeric(consumeTop)]
# median(testData$consumeTop, na.rm=T)
naBlankInfer(testData, "consumeTop", inferTo= 15)

# viewAllValues(testData, 22)
naBlankInfer(testData, "creditCashAvg", inferTo= 0)

# viewAllValues(testData, 23)
naBlankInfer(testData, "creditCashFreq", inferTo= 0)

# viewAllValues(testData, 24)
naBlankInfer(testData, "creditWD3Months", inferTo= 0)

# viewAllValues(testData, 25)
naBlankInfer(testData, "creditWDAvg", inferTo= 0)

# viewAllValues(testData, 26)
naBlankInfer(testData, "creditWDFreq", inferTo= 0)

# 缺失的通通算没车，declareCar在以后会变成zhiceCar
# viewAllValues(testData, 28)
testData[, zhiceCar:=declareCar]
testData[, declareCar:=NULL]
naBlankInfer(testData, "zhiceCar", inferTo= 0)


# viewAllValues(testData, 29)
# testData[, ecpPhoneTag:=NULL]

# viewAllValues(testData, 30)
# testData[, ecp_eachother:=NULL]

# viewAllValues(testData, 31)
naBlankInfer(testData, "goOut120", inferTo= -1)

#########################################
# 上海资信系列impute
# viewAllValues(testData, 32)
naBlankInfer(testData, "hasShCISReport", inferTo= 0)
testData[hasShCISReport=='0', c("shCISCreditLine","shCISCurrentOverDueNum",
                                      "shCISLoanQueryNo_3month", "shCISOverdualNum_3month", 
                                      "shCISOverdualNum_24month", "shCISMaxOverdual_24month"):=0]




# viewAllValues(testData, 34)
# testData[, hightRiskTransAvg1:=NULL]

# viewAllValues(testData, 36)
# testData[, hightRiskTransNum1:=NULL]

# viewAllValues(testData, 37)
# testData[, hightRiskTransNum6:=NULL]

# viewAllValues(testData, 38)
# testData[, inBlanklist:=NULL]

# viewAllValues(testData, 39)
# testData[, inJulixinBlanklist:=NULL]

# viewAllValues(testData, 40)
# testData[, inZhimaBlank:=NULL]

# viewAllValues(testData, 41)
# testData[, juxinliSuccess:=NULL]

# viewAllValues(testData, 43)
naBlankInfer(testData, "lastMonthOverdrawNum", inferTo= 0)

# viewAllValues(testData, "loansCalls1")
# median(as.numeric(testData$loansCalls1), na.rm=T)
naBlankInfer(testData, "loansCalls1", inferTo= 5)

# viewAllValues(testData, 46)
# median(as.numeric(testData$loansCalls3), na.rm=T)
naBlankInfer(testData, "loansCalls3", inferTo= 5)

# viewAllValues(testData, 47)
# testData[, local1year:=NULL]

# viewAllValues(testData, 49)
naBlankInfer(testData, "longTimeShutdown", inferTo= 0)

# viewAllValues(testData, 53)
testData[, month62ConsumeAvg:=as.numeric(month62ConsumeAvg)]
# median(testData$month62ConsumeAvg, na.rm=T)
naBlankInfer(testData, "month62ConsumeAvg", inferTo= 4878.712)

# viewAllValues(testData, 55)
# testData[, multiBorrowNumP1:=NULL]

# viewAllValues(testData, 56)
# median(testData$networkTime6, na.rm=T)
naBlankInfer(testData, "networkTime6", inferTo= 182)

# viewAllValues(testData, 57)
naBlankInfer(testData, "nightConsumeNum", inferTo= 0)

# viewAllValues(testData, 58)
naBlankInfer(testData, "noNeedMobileAuthCheck", inferTo= 0)

# viewAllValues(testData, 59)
naBlankInfer(testData, "normalContact", inferTo= 0)

# viewAllValues(testData, 60)
naBlankInfer(testData, "post12PMFeeAmount", inferTo= 0)

# viewAllValues(testData, 61)
naBlankInfer(testData, "post12PMFeeNum", inferTo= 0)

# viewAllValues(testData, "post6MonthOverdrawNum")
naBlankInfer(testData, "post6MonthOverdrawNum", inferTo= 0)

# viewAllValues(testData, 63)
testData[, postMonthConsumeFreg:=as.numeric(postMonthConsumeFreg)]
# median(testData$postMonthConsumeFreg, na.rm=T)
naBlankInfer(testData, "postMonthConsumeFreg", inferTo= 4)

# viewAllValues(testData, 64)

# viewAllValues(testData, "shCISCreditLine")
naBlankInfer(testData, "shCISCreditLine", inferTo = 0)

# viewAllValues(testData, 66)
# testData[, shCISCurrentOverDueNum:=NULL]
# viewAllValues(testData, 68)
# testData[, shCISMaxOverdual_24month:=NULL]
# viewAllValues(testData, 69)
# testData[, shCISOverdualNum_24month:=NULL]
# viewAllValues(testData, 70)
# testData[, shCISOverdualNum_3month:=NULL]
# viewAllValues(testData, 71)
# median(as.numeric(testData$tachEcp), na.rm=T)
naBlankInfer(testData, "tachEcp", inferTo= 108.5)

# viewAllValues(testData, 72)
naBlankInfer(testData, "thisCityIsTop3", inferTo= 1)

# viewAllValues(testData, 73)
naBlankInfer(testData, "transFalsePast6", inferTo= 0)

# viewAllValues(testData, 74)
naBlankInfer(testData, "transFalsePastMonth", inferTo= 0)

# viewAllValues(testData, 75)
naBlankInfer(testData, "trustAddr", inferTo= 1)

# viewAllValues(testData, 76)
# testData[, trustIP:=NULL]

# viewAllValues(testData, 77)
# testData[, unexpectedApplyTime:=NULL]

# viewAllValues(testData, 79)
testData[, zhiceHouse:=unionpayHouse]
# testData[, unionpayHouse:=NULL]
naBlankInfer(testData, "zhiceHouse", inferTo= 0)

# viewAllValues(testData, 80)
# testData[, unionpayIdCardNameCheck:=NULL]

# viewAllValues(testData, 81)
# testData[, unionpayMobileCardCheck:=NULL]

# viewAllValues(testData, 82)
# testData[, unionpayNameCardCheck:=NULL]

# viewAllValues(testData, 83)
naBlankInfer(testData, "unionpayPosConsumeCityRank", inferTo= 1)

# viewAllValues(testData, 84)
testData[, useCardAmountAvg:=as.numeric(useCardAmountAvg)]
# median(testData$useCardAmountAvg, na.rm=T)
naBlankInfer(testData, "useCardAmountAvg", inferTo= 5006.55)

# viewAllValues(testData, 85)
testData[, useCardLastTime:=as.numeric(useCardLastTime)]
# median(testData$useCardLastTime, na.rm=T)
naBlankInfer(testData, "useCardLastTime", inferTo= 31)

# viewAllValues(testData, 86)
# median(as.numeric(testData$useCardNumPost6), na.rm=T)
naBlankInfer(testData, "useCardNumPost6", inferTo= 5)

# viewAllValues(testData, 87)
testData[, useCardPM:=as.numeric(useCardPM)]
# median(testData$useCardPM, na.rm=T)
naBlankInfer(testData, "useCardPM", inferTo= 4.291667)

# viewAllValues(testData, 88)
testData[, useCardSumRank:=as.numeric(useCardSumRank)]
# median(testData$useCardSumRank, na.rm=T)
naBlankInfer(testData, "useCardSumRank", inferTo= 24.16667)

# viewAllValues(testData, 89)
naBlankInfer(testData, "workCondition", inferTo= -1)

# viewAllValues(testData, 94)

#########################################################################################
# featureAnalAfterImpute<-featureAnalysis(testData, 
#                                         exclude=c("financingprojectid",
#                                                   "flgDPD","flgTest","createtime.1"))


testData[, c("callNetLoanBlank", "cardClass", "ecpPhoneTag", "ecp_eachother", "hightRiskTransAvg1", 
                   "hightRiskTransNum1", "hightRiskTransNum6", "inBlanklist", "inJulixinBlanklist", 
                   "inZhimaBlank", "juxinliSuccess", "local1year", "multiBorrowNumP1", "shCISCurrentOverDueNum", 
                   "shCISMaxOverdual_24month", "shCISOverdualNum_24month", "shCISOverdualNum_3month", "trustIP", 
                   "unexpectedApplyTime", "unionpayHouse", "unionpayIdCardNameCheck", "unionpayMobileCardCheck",
                   "unionpayNameCardCheck", "hightRiskTransAvg6", "mateNum", "flgTest", "flgTrainTest"):=NULL]
# 
# featureAnalAfterSingleValueRemove<-featureAnalysis(testData, 
#                                         exclude=c("financingprojectid","flgDPD","flgTest"))

##########补充impute其他未填满的
# median(as.numeric(testData$avgMonthCall), na.rm = T)
naBlankInfer(testData, "avgMonthCall", inferTo= 180)
naBlankInfer(testData, "zhiceHouse", inferTo= 0)
naBlankInfer(testData, "trustAddr", inferTo= 1)
naBlankInfer(testData, "cardType", inferTo= -1)


featureAnalTrain<-featureAnalysis(testData, exclude=c("financingprojectid","flgDPD","flgTest"))

##########################################################

testData[, flgDPD:=as.factor(flgDPD)]
testData[, c("NA", "financingprojectid", "rule_name"):=NULL]



#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
##########################################################
# holdout data imputation

# viewAllValues(holdoutData, 5)
naBlankInfer(holdoutData, "amuseConsumeFreq", inferTo= 0)

# viewAllValues(holdoutData, 6)
naBlankInfer(holdoutData, "applicantContact", inferTo= -1)

# viewAllValues(holdoutData, 9)

# viewAllValues(holdoutData, 10)
naBlankInfer(holdoutData, "callEcpNum", inferTo= 0)


# viewAllValues(holdoutData, 12)
# holdoutData[, callNetLoanBlank:=NULL]

# viewAllValues(holdoutData, "called5")
naBlankInfer(holdoutData, "called5", inferTo= 6)

# viewAllValues(holdoutData, 14)
# holdoutData[, cardClass:=NULL]

# viewAllValues(holdoutData, 15)
naBlankInfer(holdoutData, "cardTerm", inferTo= -1)

# viewAllValues(holdoutData, 16)
naBlankInfer(holdoutData, "cardType", inferTo= -1)

# viewAllValues(holdoutData, 17)
naBlankInfer(holdoutData, "cellphoneAuth", inferTo= 1)

# viewAllValues(holdoutData, 19)
holdoutData[, consumeFreg:=as.numeric(consumeFreg)]
holdoutData[, consumeFreg:=round(consumeFreg)]
# Mode(holdoutData$consumeFreg)
# median(holdoutData$consumeFreg, na.rm=T)
naBlankInfer(holdoutData, "consumeFreg", inferTo= 2)


# viewAllValues(holdoutData, 20)
holdoutData[, consumeLineRate:=as.numeric(consumeLineRate)]
# median(holdoutData$consumeLineRate, na.rm=T)
naBlankInfer(holdoutData, "consumeLineRate", inferTo= 0.00428571428571)

# viewAllValues(holdoutData, 21)
holdoutData[, consumeTop:=as.numeric(consumeTop)]
# median(holdoutData$consumeTop, na.rm=T)
naBlankInfer(holdoutData, "consumeTop", inferTo= 15)

# viewAllValues(holdoutData, 22)
naBlankInfer(holdoutData, "creditCashAvg", inferTo= 0)

# viewAllValues(holdoutData, 23)
naBlankInfer(holdoutData, "creditCashFreq", inferTo= 0)

# viewAllValues(holdoutData, 24)
naBlankInfer(holdoutData, "creditWD3Months", inferTo= 0)

# viewAllValues(holdoutData, 25)
naBlankInfer(holdoutData, "creditWDAvg", inferTo= 0)

# viewAllValues(holdoutData, 26)
naBlankInfer(holdoutData, "creditWDFreq", inferTo= 0)

# 缺失的通通算没车，declareCar在以后会变成zhiceCar
# viewAllValues(holdoutData, 28)
holdoutData[, zhiceCar:=declareCar]
holdoutData[, declareCar:=NULL]
naBlankInfer(holdoutData, "zhiceCar", inferTo= 0)


# viewAllValues(holdoutData, 29)
# holdoutData[, ecpPhoneTag:=NULL]

# viewAllValues(holdoutData, 30)
# holdoutData[, ecp_eachother:=NULL]

# viewAllValues(holdoutData, 31)
naBlankInfer(holdoutData, "goOut120", inferTo= -1)

#########################################
# 上海资信系列impute
# viewAllValues(holdoutData, 32)
naBlankInfer(holdoutData, "hasShCISReport", inferTo= 0)
holdoutData[hasShCISReport=='0', c("shCISCreditLine","shCISCurrentOverDueNum",
                                "shCISLoanQueryNo_3month", "shCISOverdualNum_3month", 
                                "shCISOverdualNum_24month", "shCISMaxOverdual_24month"):=0]




# viewAllValues(holdoutData, 34)
# holdoutData[, hightRiskTransAvg1:=NULL]

# viewAllValues(holdoutData, 36)
# holdoutData[, hightRiskTransNum1:=NULL]

# viewAllValues(holdoutData, 37)
# holdoutData[, hightRiskTransNum6:=NULL]

# viewAllValues(holdoutData, 38)
# holdoutData[, inBlanklist:=NULL]

# viewAllValues(holdoutData, 39)
# holdoutData[, inJulixinBlanklist:=NULL]

# viewAllValues(holdoutData, 40)
# holdoutData[, inZhimaBlank:=NULL]

# viewAllValues(holdoutData, 41)
# holdoutData[, juxinliSuccess:=NULL]

# viewAllValues(holdoutData, 43)
naBlankInfer(holdoutData, "lastMonthOverdrawNum", inferTo= 0)

# viewAllValues(holdoutData, "loansCalls1")
# median(as.numeric(holdoutData$loansCalls1), na.rm=T)
naBlankInfer(holdoutData, "loansCalls1", inferTo= 5)

# viewAllValues(holdoutData, 46)
# median(as.numeric(holdoutData$loansCalls3), na.rm=T)
naBlankInfer(holdoutData, "loansCalls3", inferTo= 5)

# viewAllValues(holdoutData, 47)
# holdoutData[, local1year:=NULL]

# viewAllValues(holdoutData, 49)
naBlankInfer(holdoutData, "longTimeShutdown", inferTo= 0)

# viewAllValues(holdoutData, 53)
holdoutData[, month62ConsumeAvg:=as.numeric(month62ConsumeAvg)]
# median(holdoutData$month62ConsumeAvg, na.rm=T)
naBlankInfer(holdoutData, "month62ConsumeAvg", inferTo= 4878.712)

# viewAllValues(holdoutData, 55)
# holdoutData[, multiBorrowNumP1:=NULL]

# viewAllValues(holdoutData, 56)
# median(holdoutData$networkTime6, na.rm=T)
naBlankInfer(holdoutData, "networkTime6", inferTo= 182)

# viewAllValues(holdoutData, 57)
naBlankInfer(holdoutData, "nightConsumeNum", inferTo= 0)

# viewAllValues(holdoutData, 58)
naBlankInfer(holdoutData, "noNeedMobileAuthCheck", inferTo= 0)

# viewAllValues(holdoutData, 59)
naBlankInfer(holdoutData, "normalContact", inferTo= 0)

# viewAllValues(holdoutData, 60)
naBlankInfer(holdoutData, "post12PMFeeAmount", inferTo= 0)

# viewAllValues(holdoutData, 61)
naBlankInfer(holdoutData, "post12PMFeeNum", inferTo= 0)

# viewAllValues(holdoutData, "post6MonthOverdrawNum")
naBlankInfer(holdoutData, "post6MonthOverdrawNum", inferTo= 0)

# viewAllValues(holdoutData, 63)
holdoutData[, postMonthConsumeFreg:=as.numeric(postMonthConsumeFreg)]
# median(holdoutData$postMonthConsumeFreg, na.rm=T)
naBlankInfer(holdoutData, "postMonthConsumeFreg", inferTo= 4)

# viewAllValues(holdoutData, 64)

# viewAllValues(holdoutData, "shCISCreditLine")
naBlankInfer(holdoutData, "shCISCreditLine", inferTo = 0)

# viewAllValues(holdoutData, 66)
# holdoutData[, shCISCurrentOverDueNum:=NULL]
# viewAllValues(holdoutData, 68)
# holdoutData[, shCISMaxOverdual_24month:=NULL]
# viewAllValues(holdoutData, 69)
# holdoutData[, shCISOverdualNum_24month:=NULL]
# viewAllValues(holdoutData, 70)
# holdoutData[, shCISOverdualNum_3month:=NULL]
# viewAllValues(holdoutData, 71)
# median(as.numeric(holdoutData$tachEcp), na.rm=T)
naBlankInfer(holdoutData, "tachEcp", inferTo= 108.5)

# viewAllValues(holdoutData, 72)
naBlankInfer(holdoutData, "thisCityIsTop3", inferTo= 1)

# viewAllValues(holdoutData, 73)
naBlankInfer(holdoutData, "transFalsePast6", inferTo= 0)

# viewAllValues(holdoutData, 74)
naBlankInfer(holdoutData, "transFalsePastMonth", inferTo= 0)

# viewAllValues(holdoutData, 75)
naBlankInfer(holdoutData, "trustAddr", inferTo= 1)

# viewAllValues(holdoutData, 76)
# holdoutData[, trustIP:=NULL]

# viewAllValues(holdoutData, 77)
# holdoutData[, unexpectedApplyTime:=NULL]

# viewAllValues(holdoutData, 79)
holdoutData[, zhiceHouse:=unionpayHouse]
# holdoutData[, unionpayHouse:=NULL]
naBlankInfer(holdoutData, "zhiceHouse", inferTo= 0)

# viewAllValues(holdoutData, 80)
# holdoutData[, unionpayIdCardNameCheck:=NULL]

# viewAllValues(holdoutData, 81)
# holdoutData[, unionpayMobileCardCheck:=NULL]

# viewAllValues(holdoutData, 82)
# holdoutData[, unionpayNameCardCheck:=NULL]

# viewAllValues(holdoutData, 83)
naBlankInfer(holdoutData, "unionpayPosConsumeCityRank", inferTo= 1)

# viewAllValues(holdoutData, 84)
holdoutData[, useCardAmountAvg:=as.numeric(useCardAmountAvg)]
# median(holdoutData$useCardAmountAvg, na.rm=T)
naBlankInfer(holdoutData, "useCardAmountAvg", inferTo= 5006.55)

# viewAllValues(holdoutData, 85)
holdoutData[, useCardLastTime:=as.numeric(useCardLastTime)]
# median(holdoutData$useCardLastTime, na.rm=T)
naBlankInfer(holdoutData, "useCardLastTime", inferTo= 31)

# viewAllValues(holdoutData, 86)
# median(as.numeric(holdoutData$useCardNumPost6), na.rm=T)
naBlankInfer(holdoutData, "useCardNumPost6", inferTo= 5)

# viewAllValues(holdoutData, 87)
holdoutData[, useCardPM:=as.numeric(useCardPM)]
# median(holdoutData$useCardPM, na.rm=T)
naBlankInfer(holdoutData, "useCardPM", inferTo= 4.291667)

# viewAllValues(holdoutData, 88)
holdoutData[, useCardSumRank:=as.numeric(useCardSumRank)]
# median(holdoutData$useCardSumRank, na.rm=T)
naBlankInfer(holdoutData, "useCardSumRank", inferTo= 24.16667)

# viewAllValues(holdoutData, 89)
naBlankInfer(holdoutData, "workCondition", inferTo= -1)

# viewAllValues(holdoutData, 94)

#########################################################################################
# featureAnalAfterImpute<-featureAnalysis(holdoutData, 
#                                         exclude=c("financingprojectid",
#                                                   "flgDPD","flgTest","createtime.1"))


holdoutData[, c("callNetLoanBlank", "cardClass", "ecpPhoneTag", "ecp_eachother", "hightRiskTransAvg1", 
             "hightRiskTransNum1", "hightRiskTransNum6", "inBlanklist", "inJulixinBlanklist", 
             "inZhimaBlank", "juxinliSuccess", "local1year", "multiBorrowNumP1", "shCISCurrentOverDueNum", 
             "shCISMaxOverdual_24month", "shCISOverdualNum_24month", "shCISOverdualNum_3month", "trustIP", 
             "unexpectedApplyTime", "unionpayHouse", "unionpayIdCardNameCheck", "unionpayMobileCardCheck",
             "unionpayNameCardCheck", "hightRiskTransAvg6", "mateNum", "flgTest", "flgTrainTest"):=NULL]
# 
# featureAnalAfterSingleValueRemove<-featureAnalysis(holdoutData, 
#                                         exclude=c("financingprojectid","flgDPD","flgTest"))

##########补充impute其他未填满的
# median(as.numeric(holdoutData$avgMonthCall), na.rm = T)
naBlankInfer(holdoutData, "avgMonthCall", inferTo= 180)
naBlankInfer(holdoutData, "zhiceHouse", inferTo= 0)
naBlankInfer(holdoutData, "trustAddr", inferTo= 1)
naBlankInfer(holdoutData, "cardType", inferTo= -1)


featureAnalTrain<-featureAnalysis(holdoutData, exclude=c("financingprojectid","flgDPD","flgTest"))

##########################################################

holdoutData[, flgDPD:=as.factor(flgDPD)]
holdoutData[, c("NA", "financingprojectid", "rule_name"):=NULL]





write.csv(trainDataFinal, paste0(boxdata, "trainData.csv"), row.names = F)
write.csv(testData, paste0(boxdata, "testData.csv"), row.names = F)
write.csv(holdoutData, paste0(boxdata, "holdoutData.csv"), row.names = F)