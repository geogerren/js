source("~/jimu/sourceFile.R")
features<-ruleq("select 
p.financingprojectid, 
p.createtime,
t.mod_id,
t.index_id,
t.rule_id,
t.input_val
from t_mod_score t
join project_detail p
on t.service_id = p.service_id
")

########################################################################################
# Pull出现有的变量

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
featureGen(features, 10008, 1000013, "zhimaAddr")
featureGen(features, 10008, 1000014, "inZhimaBlank")
featureGen(features, 10008, 1000015, "trustAddr")
features<-featureGen(features, 10008, 1000015, "trustIP", 2)
features<-featureGen(features, 10008, 1000015, "highZhimaScore", 3)

featureGen(features, 10010, 1000016, "noNeedMobileAuthCheck")
features<-featureGen(features, 10010, 1000016, "cellphoneAuth", 2)
featureGen(features, 10010, 1000017, "networkTime6")
featureGen(features, 10010, 1000018, "called5")
# features<-featureGen(features, 10010, 1000018, "avgMonthCallTime", 2)
features<-featureGen(features, 10010, 1000018, "avgMonthCall", 2)
featureGen(features, 10010, 1000019, "thisCityIsTop3")
featureGen(features, 10010, 1000020.1, "normalContact")
featureGen(features, 10010, 1000020.2, "callNetLoanBlank")
featureGen(features, 10010, 1000020.3, "callEcpNum")
features<-featureGen(features, 10010, 1000020.3, "ecpPhoneTag", 2)
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
featureGen(features, 10013, 1000030.3, "hasShCISReport")
features<-featureGen(features, 10013, 1000030.3, "shCISCurrentOverDueNum", 2)
featureGen(features, 10013, 1000030.4, "shCISOverdualNum_3month")
featureGen(features, 10013, 1000030.5, "shCISMaxOverdual_24month")

# 身份特质
featureGen(features, 10016, 1000032, "sex")
featureGen(features, 10016, 1000033, "age")
featureGen(features, 10016, 1000034, "marry")
features<-featureGen(features, 10016, 1000034, "childrenNum", 2)
featureGen(features, 10016, 1000035, "local1year")
# features<-featureGen(features, 10016, 1000035, "postConsume02", 2)
# features<-featureGen(features, 10016, 1000035, "postConsume24", 3)
# features<-featureGen(features, 10016, 1000035, "postConsume46", 4)
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
features<-featureGen(features, 10018, 1000043, "declareCar", 2) #等价zhiceCar

features<-featureGen(features, 10018, 1000044, "currentJobyear")
features<-featureGen(features, 10018, 1000045, "monthIncome")
featureGen(features, 10018, 1000046, "loanPurpose")
featureGen(features, 10018, 1000047, "month62ConsumeAvg")
features<-featureGen(features, 10018, 1000047, "postMonthConsumeFreg", 2)
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
featureGen(features, 10019, 1000061, "shCISOverdualNum_24month")
featureGen(features, 10019, 1000062, "shCISCreditLine")
featureGen(features, 10019, 1000063, "shCISLoanQueryNo_3month")
featureGen(features, 10019, 1000064, "multiBorrowNumP6")
features<-featureGen(features, 10019, 1000064, "multiBorrowNumP1")

##################################################################################
# 加上target
featuresWide<-dcast.data.table(features, financingprojectid + createtime ~ key, fun.agg=min, value.var = "value")
featuresWide<-merge(featuresWide, target[, c("project_id","Loan_Date","tenor","flgDPD","flgTest"), with=F], by.x="financingprojectid", by.y="project_id")
# test<-featureAnalysis(featuresWideU, c("financingprojectid","NA","createtime","Loan_Date","flgDPD","flgTest"))
featuresWide[, createtime:=as.POSIXct(createtime)]

# 补全银联缺失
# source("~/jimu/dumiaoA/rebuildUnionpay.R")
names(unionPayRebuilt)<-paste0(names(unionPayRebuilt), ".1")
##
featuresWideU<-merge(featuresWide, unionPayRebuilt, by.x="financingprojectid", by.y="financingprojectid.1", all.x=T)
featuresWideU[!is.na(createtime.1), creditWD3Months:=creditWD3Months.1]
featuresWideU[!is.na(createtime.1), hightRiskTransNum6:=hightRiskTransNum6.1]
featuresWideU[!is.na(createtime.1), hightRiskTransAvg6:=hightRiskTransAvg6Total.1/(useCardNumPost6+0.00000000001)]
featuresWideU[!is.na(createtime.1), post6MonthOverdrawNum:=post6MonthOverdrawNum.1]
featuresWideU[!is.na(createtime.1), transFalsePast6:=transFalsePast6.1]
featuresWideU[!is.na(createtime.1), useCardPM:=useCardNumPMTotal.1/(useCardNumPost6+0.00000000001)] #这个变量根本没出现6，可是却是6个月平均刷卡消费频率
featuresWideU[!is.na(createtime.1), post12PMFeeAmount:=post12PMFeeAmount.1]
featuresWideU[!is.na(createtime.1), post12PMFeeNum:=post12PMFeeNum.1]
featuresWideU[!is.na(createtime.1), hightRiskTransAvg1:=highRiskTransAvg1Total.1]
featuresWideU[!is.na(createtime.1), hightRiskTransNum1:=highRiskTransNum1.1]
featuresWideU[!is.na(createtime.1), multiBorrowNumP1:=multiBorrowNumP1.1]
featuresWideU[!is.na(createtime.1), lastMonthOverdrawNum:=lastMonthOverdrawNum.1]
featuresWideU[!is.na(createtime.1), transFalsePastMonth:=transFalsePastMonth.1]
featuresWideU[!is.na(createtime.1), transFalsePast6:=transFalsePast6.1]

featuresWideU[, c("creditWD3Months.1","hightRiskTransNum6.1","hightRiskTransAvg6Total.1","post6MonthOverdrawNum.1",
                  "transFalsePast6.1","useCardNumPMTotal.1","post12PMFeeAmount.1","post12PMFeeNum.1",
                  "highRiskTransAvg1Total.1","highRiskTransNum1.1","multiBorrowNumP1.1","lastMonthOverdrawNum.1",
                  "transFalsePastMonth.1"):=NULL]
featuresWideU[, transFalsePast6.1:=NULL]

# 删掉一个啥资料都没有的人
featuresWideU<-featuresWideU[!financingprojectid==121237, ]

# 补全同盾缺失
tongdun<-ruleq("select pd.financingprojectid
                ,DATE(pd.createtime) as createtime
                ,td.rule_score
                ,td.rule_name
                ,td.final_score
                from t_pat_tongdun_blank td
                join project_detail pd
                on td.service_id = pd.service_id
                ")
tongdun[rule_name=='3个月内身份证在多个平台进行借款', rule_name:='tongdunIdMultiLoanNum']
tongdun[rule_name=='3个月内手机在多个平台进行借款', rule_name:='tongdunPhoneMultiLoanNum']
tongdun[rule_name=='3个月内银行卡在多个平台进行借款', rule_name:='tongdunBankCardMultiLoanNum']
tongdun[rule_name=='借款人身份证命中法院执行法院失信证据库', rule_name:='tongdunIdDiscredit']
tongdun[, ruleCnt:=ifelse(rule_name=='', 0, 1)]

test<-dcast.data.table(tongdun, financingprojectid ~ rule_name, fun.agg=max, value.var = "ruleCnt")
test[, V1:=NULL]
test[test==-Inf]<-0


featuresWideU[, c("tongdunIdDiscredit","tongdunIdMultiLoanNum","tongdunPhoneDiscredit","tongdunPhoneMultiLoanNum"):=NULL]
featuresWideU<-merge(featuresWideU, test, by="financingprojectid")

###################################################################################
trainData <- featuresWideU[flgTest==0,]
sampleAnal<-sampleMissingAnalysis(trainData, "financingprojectid")

# 删掉一些缺失太多的
trainDataFinal <- trainData[!(financingprojectid %in% c(217073,207891,206019,207993,208577,213320,214510)),]





