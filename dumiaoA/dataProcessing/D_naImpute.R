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

# viewAllValues(trainDataFinal, 13)
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
naBlankInfer(trainDataFinal, "consumeLineRate", inferTo= 0.005714286)

# viewAllValues(trainDataFinal, 21)
trainDataFinal[, consumeTop:=as.numeric(consumeTop)]
# median(trainDataFinal$consumeTop, na.rm=T)
naBlankInfer(trainDataFinal, "consumeTop", inferTo= 20)

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

# viewAllValues(trainDataFinal, 35)
naBlankInfer(trainDataFinal, "hightRiskTransAvg6", inferTo= 0)

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
# median(trainDataFinal$month62ConsumeAvg, na.rm=T)
naBlankInfer(trainDataFinal, "month62ConsumeAvg", inferTo= 4625.812)

# viewAllValues(trainDataFinal, 55)
# trainDataFinal[, multiBorrowNumP1:=NULL]

# viewAllValues(trainDataFinal, 56)
# median(trainDataFinal$networkTime6, na.rm=T)
naBlankInfer(trainDataFinal, "networkTime6", inferTo= 509)

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
naBlankInfer(trainDataFinal, "useCardAmountAvg", inferTo= 4830.633)

# viewAllValues(trainDataFinal, 85)
trainDataFinal[, useCardLastTime:=as.numeric(useCardLastTime)]
# median(trainDataFinal$useCardLastTime, na.rm=T)
naBlankInfer(trainDataFinal, "useCardLastTime", inferTo= 24)

# viewAllValues(trainDataFinal, 86)
median(as.numeric(trainDataFinal$useCardNumPost6), na.rm=T)
naBlankInfer(trainDataFinal, "useCardNumPost6", inferTo= 5)

# viewAllValues(trainDataFinal, 87)
trainDataFinal[, useCardPM:=as.numeric(useCardPM)]
# median(trainDataFinal$useCardPM, na.rm=T)
naBlankInfer(trainDataFinal, "useCardPM", inferTo= 4)

# viewAllValues(trainDataFinal, 88)
trainDataFinal[, useCardSumRank:=as.numeric(useCardSumRank)]
# median(trainDataFinal$useCardSumRank, na.rm=T)
naBlankInfer(trainDataFinal, "useCardSumRank", inferTo= 25.41667)

viewAllValues(trainDataFinal, 89)
naBlankInfer(trainDataFinal, "workCondition", inferTo= -1)

# viewAllValues(trainDataFinal, 94)

#########################################################################################
featureAnalAfterImpute<-featureAnalysis(trainDataFinal, 
                                        exclude=c("financingprojectid",
                                                  "flgDPD","flgTest","createtime.1"))


trainDataFinal[, callNetLoanBlank:=NULL]
trainDataFinal[, cardClass:=NULL]
trainDataFinal[, ecpPhoneTag:=NULL]
trainDataFinal[, ecp_eachother:=NULL]
trainDataFinal[, hightRiskTransAvg1:=NULL]
trainDataFinal[, hightRiskTransNum1:=NULL]
trainDataFinal[, hightRiskTransNum6:=NULL]
trainDataFinal[, inBlanklist:=NULL]
trainDataFinal[, inJulixinBlanklist:=NULL]
trainDataFinal[, inZhimaBlank:=NULL]
trainDataFinal[, juxinliSuccess:=NULL]
trainDataFinal[, local1year:=NULL]
trainDataFinal[, multiBorrowNumP1:=NULL]
trainDataFinal[, shCISCurrentOverDueNum:=NULL]
trainDataFinal[, shCISMaxOverdual_24month:=NULL]
trainDataFinal[, shCISOverdualNum_24month:=NULL]
trainDataFinal[, shCISOverdualNum_3month:=NULL]
trainDataFinal[, trustIP:=NULL]
trainDataFinal[, unexpectedApplyTime:=NULL]
trainDataFinal[, unionpayHouse:=NULL]
trainDataFinal[, unionpayIdCardNameCheck:=NULL]
trainDataFinal[, unionpayMobileCardCheck:=NULL]
trainDataFinal[, unionpayNameCardCheck:=NULL]
trainDataFinal[, hightRiskTransAvg6:=NULL]
trainDataFinal[, mateNum:=NULL]
trainDataFinal[, flgTest:=NULL]





featureAnalAfterSingleValueRemove<-featureAnalysis(trainDataFinal, 
                                        exclude=c("financingprojectid","flgDPD","flgTest"))

##########补充impute其他未填满的
naBlankInfer(trainDataFinal, "avgMonthCall", inferTo= 182)
naBlankInfer(trainDataFinal, "zhiceHouse", inferTo= 0)
naBlankInfer(trainDataFinal, "trustAddr", inferTo= 1)
naBlankInfer(trainDataFinal, "cardType", inferTo= -1)



#################################################################################################
##########################################################
# testing data default assignment
testData[, callNetLoanBlank:=NULL]
testData[, cardClass:=NULL]
testData[, ecpPhoneTag:=NULL]
testData[, ecp_eachother:=NULL]
testData[, hightRiskTransAvg1:=NULL]
testData[, hightRiskTransNum1:=NULL]
testData[, hightRiskTransNum6:=NULL]
testData[, inBlanklist:=NULL]
testData[, inJulixinBlanklist:=NULL]
testData[, inZhimaBlank:=NULL]
testData[, juxinliSuccess:=NULL]
testData[, local1year:=NULL]
testData[, multiBorrowNumP1:=NULL]
testData[, shCISCurrentOverDueNum:=NULL]
testData[, shCISMaxOverdual_24month:=NULL]
testData[, shCISOverdualNum_24month:=NULL]
testData[, shCISOverdualNum_3month:=NULL]
testData[, trustIP:=NULL]
testData[, unexpectedApplyTime:=NULL]
testData[, unionpayHouse:=NULL]
testData[, unionpayIdCardNameCheck:=NULL]
testData[, unionpayMobileCardCheck:=NULL]
testData[, unionpayNameCardCheck:=NULL]
testData[, hightRiskTransAvg6:=NULL]
testData[, mateNum:=NULL]
testData[, flgTest:=NULL]




naBlankInfer(testData, )
