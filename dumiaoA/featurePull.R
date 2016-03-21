source("~/jimu/sourceFile.R")
source("~/jimu/generalFunction.R")
features<-ruleq("select 
p.financingprojectid, 
t.mod_id,
t.index_id,
t.rule_id,
t.input_val
from t_mod_score t
join project_detail p
on t.service_id = p.service_id
")


featureGen(features, 10004, 1000005.1, "ecpNum")
features<-featureGen(features, 10004, 1000005.1, "mateNum", 2)
featureGen(features, 10004, 1000005.3, "addrUsedNum")
featureGen(features, 10004, 1000006, "ipUsedNum")
featureGen(features, 10004, 1000007, "browserUsedNum")
featureGen(features, 10004, 1000007.1, "ecp_eachother")
featureGen(features, 10004, 1000007.2, "unexpectedApplyTime")

featureGen(features, 10005, 1000008, "workCondition")
featureGen(features, 10007, 1000011, "inBlanklist")

featureGen(features, 10008, 1000012, "zhimaScore")
featureGen(features, 10008, 1000014, "inZhimaBlank")
featureGen(features, 10008, 1000015, "trustAddr")
features<-featureGen(features, 10008, 1000015, "trustIP", 2)

featureGen(features, 10010, 1000016, "noNeedMobileAuthCheck")
features<-featureGen(features, 10010, 1000016, "cellphoneAuth", 2)
featureGen(features, 10010, 1000017, "networkTime6")
featureGen(features, 10010, 1000018, "called5")
features<-featureGen(features, 10010, 1000018, "avgMonthCallTime", 2)
features<-featureGen(features, 10010, 1000018, "avgMonthCall", 3)
featureGen(features, 10010, 1000019, "thisCityIsTop3")
featureGen(features, 10010, 1000020.3, "callEcpNum")
features<-featureGen(features, 10010, 1000020.3, "ecpPhoneTag", 2)
featureGen(features, 10010, 1000020.1, "normalContact")
featureGen(features, 10010, 1000020.2, "callNetLoanBlank")
featureGen(features, 10010, 1000021, "goOut120")
featureGen(features, 10010, 1000023, "inJulixinBlanklist")
featureGen(features, 10010, 1000024, "applicantContact")
featureGen(features, 10010, 1000025, "loansCalls3")
features<-featureGen(features, 10010, 1000025, "loansCalls1", 2)


featureGen(features, 10013, "1000026.0.1", "tongdunIdMultiLoanNum")
features<-featureGen(features, 10013, "1000026.0.1", "tongdunPhoneMultiLoanNum", 2)
featureGen(features, 10013, "1000026.0.2", "tongdunIdDiscredit")
features<-featureGen(features, 10013, "1000026.0.2", "tongdunPhoneDiscredit", 2)
# featureGen(features, 10013, 1000026.1, "cellphoneAuth")   # too many missings
featureGen(features, 10013, 1000026.1, "unionpayNameCardCheck")
features<-featureGen(features, 10013, 1000026.1, "unionpayIdCardNameCheck", 2)
features<-featureGen(features, 10013, 1000026.1, "unionpayMobileCardCheck", 3)
featureGen(features, 10013, 1000026.2, "cardClass")
featureGen(features, 10013, 1000026.3, "cardTerm")
featureGen(features, 10013, 1000026.4, "unionpayPosConsumeCityRank")
featureGen(features, 10013, 1000027, "useCardNumPost6")
features<-featureGen(features, 10013, 1000027, "useCardLastTime", 2)
featureGen(features, 10013, 1000028, "useCardPM")
featureGen(features, 10013, 1000029, "post6MonthOverdrawNum")
features<-featureGen(features, 10013, 1000029, "lastMonthOverdrawNum", 2)
featureGen(features, 10013, 1000030, "useCardSumRank")
features<-featureGen(features, 10013, 1000030, "useCardAmountAvg", 2)
featureGen(features, 10013, 1000030.2, "hightRiskTransAvg1")
features<-featureGen(features, 10013, 1000030.2, "hightRiskTransNum1", 2)

# 身份特质
featureGen(features, 10016, 1000032, "sex")
featureGen(features, 10016, 1000033, "age")
featureGen(features, 10016, 1000034, "marry")
features<-featureGen(features, 10016, 1000034, "childrenNum", 2)
featureGen(features, 10016, 1000035, "local1year")
features<-featureGen(features, 10016, 1000035, "postConsume02", 2)
features<-featureGen(features, 10016, 1000035, "postConsume24", 3)
features<-featureGen(features, 10016, 1000035, "postConsume46", 4)
featureGen(features, 10016, 1000036.1, "localFriends")
featureGen(features, 10016, 1000036.2, "longTimeShutdown")
# featureGen(features, 10016, 1000036.3, "workCondition") #No value
featureGen(features, 10016, 1000036.4, "tachEcp")

# 信用历史
featureGen(features, 10017, 1000037, "consumeTop")
featureGen(features, 10017, 1000038, "consumeLineRate")
featureGen(features, 10017, 1000039, "consumeFreg")
featureGen(features, 10017, 1000040, "creditCashFreq")
features<-featureGen(features, 10017, 1000040, "creditCashAvg", 2)
featureGen(features, 10017, 1000041, "creditWDFreq")
features<-featureGen(features, 10017, 1000041, "creditWDAvg", 2)
featureGen(features, 10017, 1000041.2, "creditWD3Months")

featureGen(features, 10018, 1000042, "zhiceHouse")
features<-featureGen(features, 10018, 1000042, "unionpayHouse", 2) #等价zhiceHouse
features<-featureGen(features, 10018, 1000042, "post12PMFeeAmount", 3)
features<-featureGen(features, 10018, 1000042, "post12PMFeeNum", 4)
featureGen(features, 10018, 1000043, "zhiceCar")
features<-featureGen(features, 10018, 1000043, "declareCar", 2) #登记zhiceCar
features<-featureGen(features, 10018, 1000043, "carConsumePart5Freq", 3)
features<-featureGen(features, 10018, 1000043, "carConsume5Freq", 4)
features<-featureGen(features, 10018, 1000043, "carConsume4sSum", 5)
features<-featureGen(features, 10018, 1000043, "carConsumeFreq", 6)
features<-featureGen(features, 10018, 1000043, "carConsume7Sum", 7)
features<-featureGen(features, 10018, 1000044, "currentJobyear")
features<-featureGen(features, 10018, 1000045, "monthIncome")
# featureGen(features, 10018, 1000046, "loanPurpose")
featureGen(features, 10018, 1000047, "month62ConsumeAvg")
featureGen(features, 10018, 1000047, "postMonthConsumeFreg", 2)
featureGen(features, 10018, 1000048, "cardType")

# 行为偏好
featureGen(features, 10019, 1000049, "amuseConsumeFreq")
featureGen(features, 10019, 1000050, "callLaws")
featureGen(features, 10019, 1000051, "callBlacklist")
featureGen(features, 10019, 1000052, "mealsNum")
featureGen(features, 10019, 1000053, "nightConsumeNum")
featureGen(features, 10019, 1000054, "unionpayAnswer")
featureGen(features, 10019, 1000056, "transFalsePast6")
features<-featureGen(features, 10019, 1000056, "transFalsePastMonth", 2)
featureGen(features, 10019, 1000058, "hightRiskTransAvg1")
features<-featureGen(features, 10019, 1000058, "hightRiskTransNum1", 2)
features<-featureGen(features, 10019, 1000058, "hightRiskTransNum6", 3)
features<-featureGen(features, 10019, 1000058, "hightRiskTransAvg6", 4)
featureGen(features, 10019, 1000059, "loansCalls3")
features<-featureGen(features, 10019, 1000059, "loansCalls1", 2)
features<-featureGen(features, 10019, 1000059, "juxinliSuccess", 3)


# 不使用调额数据
# featureGen(features, 10000, 1000031.5, "creditLineDB")
# featureGen(features, 10000, 1000031.5, "applyMonthsMax")


# 不使用平台上数据
# featureGen(features, 10002, 1000001, "age")
# featureGen(features, 10002, 1000002, "idcardno")
# featureGen(features, 10002, 1000002.2, "overdueLoansNum")
# featureGen(features, 10002, 1000002.2, "dumiaoOverdueLoansDays", 2)
# featureGen(features, 10002, 1000003, "cellphoneInService")
# featureGen(features, 10002, 1000003, "caller", 2)



##################################################################################
featuresWide<-dcast.data.table(features, financingprojectid ~ key, fun.agg=max, value.var = "value")
featuresWide<-merge(featuresWide, target[, c("project_id","Loan_Date","tenor","flgDPD","flgTest"), with=F], by.x="financingprojectid", by.y="project_id")
# 去除182个大面积缺失的样本
featuresWide<-featuresWide[!is.na(zhiceCar),]


viewAllValues(featuresWide, 1)
viewAllValues(featuresWide, 2)
viewAllValues(featuresWide, 3)

viewAllValues(featuresWide, 4)
naBlankInfer(featuresWide, "amuseConsumeFreq", c(NA, '0.0'), 0)

# Note: can't determine what -1 means. 
viewAllValues(featuresWide, 5)
naBlankInfer(featuresWide, "applicantContact", inferTo=0)

viewAllValues(featuresWide, 6)
naBlankInfer(featuresWide, "avgMonthCall", inferTo=0)

# seems duplicate of avgMonthCall
viewAllValues(featuresWide, 7)
featuresWide[, avgMonthCallTime:=NULL] 

viewAllValues(featuresWide, 7)

viewAllValues(featuresWide, 8)         

viewAllValues(featuresWide, 9)
naBlankInfer(featuresWide, "callEcpNum", inferTo=0)

# Note: can't determine what -1 means. 
viewAllValues(featuresWide, 10)

# all 0 or NA
viewAllValues(featuresWide, 11)
featuresWide[, callNetLoanBlank:=NULL]

viewAllValues(featuresWide, 12)
naBlankInfer(featuresWide, "carConsume4sSum", inferTo=0)

viewAllValues(featuresWide, 13)
naBlankInfer(featuresWide, "carConsume5Freq", inferTo=0)

viewAllValues(featuresWide, 14)
naBlankInfer(featuresWide, "carConsume7Sum", inferTo=0)

viewAllValues(featuresWide, 15)
naBlankInfer(featuresWide, "carConsumeFreq", inferTo=0)

viewAllValues(featuresWide, 16)
naBlankInfer(featuresWide, "carConsumePart5Freq", inferTo=0)

# no info provided
# 02: 1237, 03:1, NA:182
viewAllValues(featuresWide, 17)
featuresWide[, cardClass:=NULL]

viewAllValues(featuresWide, 17)
naBlankInfer(featuresWide, "cardTerm", NA, -1)

viewAllValues(featuresWide, 18)
naBlankInfer(featuresWide, "cardType", c("白金卡","钛金卡"), 5)
naBlankInfer(featuresWide, "cardType", NA, -1)

viewAllValues(featuresWide, 19)
naBlankInfer(featuresWide, "cellphoneAuth", NA, 0)

viewAllValues(featuresWide, 20)
viewAllValues(featuresWide, 21)
viewAllValues(featuresWide, 22)
viewAllValues(featuresWide, 23)
viewAllValues(featuresWide, 24)
viewAllValues(featuresWide, 25)
viewAllValues(featuresWide, 26)
naBlankInfer(featuresWide, "creditWD3Months", NA, 0)

viewAllValues(featuresWide, 27)
naBlankInfer(featuresWide, "creditWDAvg", NA, 0)

viewAllValues(featuresWide, 28)
naBlankInfer(featuresWide, "creditWDFreq", NA, 0)

viewAllValues(featuresWide, 29)
viewAllValues(featuresWide, 30)
viewAllValues(featuresWide, 31)

# no info provided
viewAllValues(featuresWide, 32)
# 0:1080 未知:1 NA:157
featuresWide[, ecpPhoneTag:=NULL]

# no info provided
viewAllValues(featuresWide, 32)
# 0:31 NA:1207
featuresWide[, ecp_eachother:=NULL]

viewAllValues(featuresWide, 32)
naBlankInfer(featuresWide, "goOut120", NA, 0)

# no info provided
viewAllValues(featuresWide, 33)
# NA:93 0:rest
featuresWide[, hightRiskTransAvg1:=NULL]

viewAllValues(featuresWide, 33)
naBlankInfer(featuresWide, "hightRiskTransAvg6", NA, 0)

# no info provided
viewAllValues(featuresWide, 34)
# NA:93 1:1 0:rest
featuresWide[, hightRiskTransNum1:=NULL]

viewAllValues(featuresWide, 34)
naBlankInfer(featuresWide, "hightRiskTransNum6", NA, 0)

# all 0
viewAllValues(featuresWide, 35)
featuresWide[, inBlanklist:=NULL]

# all 0 or NULL
viewAllValues(featuresWide, 35)
featuresWide[, inJulixinBlanklist:=NULL]

# all 0 or F
viewAllValues(featuresWide, 35)
featuresWide[, inZhimaBlank:=NULL]

viewAllValues(featuresWide, 35)
viewAllValues(featuresWide, 36)
naBlankInfer(featuresWide, "juxinliSuccess", NA, 0)

viewAllValues(featuresWide, 37)
naBlankInfer(featuresWide, "lastMonthOverdrawNum", NA, 0)

viewAllValues(featuresWide, 38)
naBlankInfer(featuresWide, "loansCalls1", NA, 0)

viewAllValues(featuresWide, 39)
naBlankInfer(featuresWide, "loansCalls3", NA, 0)

viewAllValues(featuresWide, 40)
viewAllValues(featuresWide, 41)
viewAllValues(featuresWide, 42)
viewAllValues(featuresWide, 43)

# all 0
viewAllValues(featuresWide, 44)
featuresWide[, mateNum:=NULL]

viewAllValues(featuresWide, 44)
viewAllValues(featuresWide, 45)
viewAllValues(featuresWide, 46)

viewAllValues(featuresWide, 47)
median(as.numeric(featuresWide[flgTest==0,]$networkTime6), na.rm=T)
featuresWide[flgTest==0 & is.na(networkTime6), networkTime6:="645"]
median(as.numeric(featuresWide[flgTest==1,]$networkTime6), na.rm=T)
featuresWide[flgTest==1 & is.na(networkTime6), networkTime6:="797"]

viewAllValues(featuresWide, 48)

viewAllValues(featuresWide, 49)
naBlankInfer(featuresWide, "noNeedMobileAuthCheck", NA, 0)

viewAllValues(featuresWide, 50)
naBlankInfer(featuresWide, "normalContact", NA, -1)

# no info provided
viewAllValues(featuresWide, 51)
# 918:1 0:rest
featuresWide[, post12PMFeeAmount:=NULL]

viewAllValues(featuresWide, 51)
featuresWide[, post12PMFeeNum:=NULL]

viewAllValues(featuresWide, 51)
naBlankInfer(featuresWide, "post6MonthOverdrawNum", NA, 0)

viewAllValues(featuresWide, 52)
viewAllValues(featuresWide, 53)
viewAllValues(featuresWide, 54)

viewAllValues(featuresWide, 55)
naBlankInfer(featuresWide, "sex", "M", 1)
naBlankInfer(featuresWide, "sex", "F", 0)

viewAllValues(featuresWide, 56)
naBlankInfer(featuresWide, "tachEcp", NA, 0)

viewAllValues(featuresWide, 57)
naBlankInfer(featuresWide, "thisCityIsTop3", NA, -1)

# all NA
viewAllValues(featuresWide, 58)
featuresWide[,tongdunIdDiscredit:=NULL]

viewAllValues(featuresWide, 58)
featuresWide[,tongdunIdMultiLoanNum:=NULL]

viewAllValues(featuresWide, 58)
featuresWide[,tongdunPhoneDiscredit:=NULL]

viewAllValues(featuresWide, 58)
featuresWide[,tongdunPhoneMultiLoanNum:=NULL]

viewAllValues(featuresWide, 58)
naBlankInfer(featuresWide, "transFalsePast6", NA, 0)

viewAllValues(featuresWide, 59)
naBlankInfer(featuresWide, "transFalsePastMonth", NA, 0)

viewAllValues(featuresWide, 60)
viewAllValues(featuresWide, 61)

# all 0 or NA
viewAllValues(featuresWide, 62)
featuresWide[, unexpectedApplyTime:=NULL]

viewAllValues(featuresWide, 62)
viewAllValues(featuresWide, 63)
naBlankInfer(featuresWide, "unionpayHouse", "True", 1)
naBlankInfer(featuresWide, "unionpayHouse", "False", 0)

# 1:1 NA:1237
viewAllValues(featuresWide, 64)
featuresWide[, unionpayIdCardNameCheck:=NULL]

viewAllValues(featuresWide, 64)
featuresWide[, unionpayMobileCardCheck:=NULL]

viewAllValues(featuresWide, 64)
featuresWide[, unionpayNameCardCheck:=NULL]

viewAllValues(featuresWide, 64)
