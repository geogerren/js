# features<-read.csv(paste0(boxdata, "dumiao.csv"), stringsAsFactors = F)
# features<-data.table(features)

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

cities<-ruleq("select b.financingprojectid, a.val as city from t_cust_ser_data a
join project_detail b
              on a.service_id = b.service_id
              where a.class_1='applyCity' and a.data_source='jisudai'")

tvardict<-ruleq("select pd.financingprojectid
,DATE(pd.createtime) as createtime
                  ,td.var_code
                  ,td.var_val
                  from t_variable_dict td
                  join project_detail pd
                  on td.applyid = pd.apply_id
                  ")
########################################################################################
# t_mod_score
featureGen(features, 10014, '1000031.0', "modelScore")

featureGen(features, 10005, 1000008, "workCondition")
featureGen(features, 10008, 1000012, "zhimaScore")
featureGen(features, 10010, 1000017, "networkTime6")
featureGen(features, 10010, 1000018, "avgMonthCall")
featureGen(features, 10010, 1000020.1, "normalContact")
featureGen(features, 10010, 1000020.2, "callNetLoanBlank")
featureGen(features, 10010, 1000020.3, "ecpPhoneTag")
featureGen(features, 10010, 1000021, "goOut120")
featureGen(features, 10010, 1000023, "inJulixinBlanklist")
featureGen(features, 10010, 1000024, "applicantContact")
featureGen(features, 10010, 1000025, "loansCalls3")
features<-featureGen(features, 10010, 1000025, "loansCalls1", 2)
featureGen(features, 10013, 1000026.3, "cardTerm")
featureGen(features, 10013, 1000026.4, "unionpayPosConsumeCityRank")
featureGen(features, 10013, 1000027, "useCardNumPost6")
features<-featureGen(features, 10013, 1000027, "useCardLastTime", 2)
featureGen(features, 10013, 1000028, "useCardPM")
featureGen(features, 10013, 1000029, "post6MonthOverdrawNum")
features<-featureGen(features, 10013, 1000029, "lastMonthOverdrawNum", 2)
features<-featureGen(features, 10013, 1000029, "post12MonthOverdrawNum", 3)
featureGen(features, 10013, 1000030, "useCardSumRank")
features<-featureGen(features, 10013, 1000030, "useCardAmountAvg", 2)
featureGen(features, 10013, 1000030.2, "hightRiskTransAvg1")
features<-featureGen(features, 10013, 1000030.2, "hightRiskTransNum1", 2)
featureGen(features, 10013, 1000030.3, "hasShCISReport")
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
featureGen(features, 10018, 1000047, "postMonthConsumeFreg")
featureGen(features, 10018, 1000048, "cardType")
featureGen(features, 10019, 1000049, "amuseConsumeFreq")
featureGen(features, 10019, 1000050, "callLaws")
featureGen(features, 10019, 1000051, "callBlacklist")
featureGen(features, 10019, 1000052, "mealsNum")
featureGen(features, 10019, 1000053, "nightConsumeNum")
featureGen(features, 10019, 1000058, "hightRiskTransAvg1")
features<-featureGen(features, 10019, 1000058, "hightRiskTransNum1", 2)
features<-featureGen(features, 10019, 1000058, "hightRiskTransNum6", 3)
features<-featureGen(features, 10019, 1000058, "hightRiskTransAvg6", 4)
featureGen(features, 10019, 1000059, "loansCalls3")
features<-featureGen(features, 10019, 1000059, "loansCalls1", 2)
features<-featureGen(features, 10019, 1000059, "juxinliSuccess", 3)
featureGen(features, 10019, 1000064, "multiBorrowNumP6")
features<-featureGen(features, 10019, 1000064, "multiBorrowNumP1",2)


featuresWide<-dcast.data.table(features, financingprojectid + createtime ~ key, fun.agg=min, value.var = "value")

##################################################################################################
# t_variable_dict
tvardictWide <- dcast.data.table(tvardict, financingprojectid + createtime ~ var_code, fun.agg=min, value.var = "var_val")
names(tvardictWide) <- paste0(names(tvardictWide), ".1")

featuresWide <- merge(featuresWide, tvardictWide, by.x="financingprojectid", by.y="financingprojectid.1", all.x=T)

featuresWide[, age:=ifelse(is.na(age), age.1, age)]
featuresWide[, amuseConsumeFreq:=ifelse(is.na(amuseConsumeFreq), amuseConsumeFreq.1, amuseConsumeFreq)]
featuresWide[, applicantContact:=ifelse(is.na(applicantContact), applicantContact.1, applicantContact)]
featuresWide[, avgMonthCall:=ifelse(is.na(avgMonthCall), avgMonthCall.1, avgMonthCall)]
featuresWide[, callBlacklist:=ifelse(is.na(callBlacklist), callBlacklist.1, callBlacklist)]
featuresWide[, callLaws:=ifelse(is.na(callLaws), callLaws.1, callLaws)]
featuresWide[, callNetLoanBlank:=ifelse(is.na(callNetLoanBlank), callNetLoanBlank.1, callNetLoanBlank)]
featuresWide[, cardTerm:=ifelse(is.na(cardTerm), cardTerm.1, cardTerm)]
featuresWide[, cardType:=ifelse(is.na(cardType), cardType.1, cardType)]
featuresWide[, childrenNum:=ifelse(is.na(childrenNum), childrenNum.1, childrenNum)]
featuresWide[, consumeFreg:=ifelse(is.na(consumeFreg), consumeFreg.1, consumeFreg)]
featuresWide[, consumeLineRate:=ifelse(is.na(consumeLineRate), consumeLineRate.1, consumeLineRate)]
featuresWide[, consumeTop:=ifelse(is.na(consumeTop), consumeTop.1, consumeTop)]
featuresWide[, creditCashAvg:=ifelse(is.na(creditCashAvg), creditCashAvg.1, creditCashAvg)]
featuresWide[, creditCashFreq:=ifelse(is.na(creditCashFreq), creditCashFreq.1, creditCashFreq)]
featuresWide[, creditWD3Months:=ifelse(is.na(creditWD3Months), creditWD3Months.1, creditWD3Months)]
featuresWide[, creditWDAvg:=ifelse(is.na(creditWDAvg), creditWDAvg.1, creditWDAvg)]
featuresWide[, creditWDFreq:=ifelse(is.na(creditWDFreq), creditWDFreq.1, creditWDFreq)]
featuresWide[, currentJobyear:=ifelse(is.na(currentJobyear), currentJobyear.1, currentJobyear)]
featuresWide[, declareCar:=ifelse(is.na(declareCar), declareCar.1, declareCar)]
featuresWide[, ecpPhoneTag:=ifelse(is.na(ecpPhoneTag), ecpPhoneTag.1, ecpPhoneTag)]
featuresWide[, goOut120:=ifelse(is.na(goOut120), goOut120.1, goOut120)]
featuresWide[, hasShCISReport:=ifelse(is.na(hasShCISReport), hasShCISReport.1, hasShCISReport)]
featuresWide[, hightRiskTransAvg1:=ifelse(is.na(hightRiskTransAvg1), hightRiskTransAvg1.1, hightRiskTransAvg1)]
featuresWide[, hightRiskTransAvg6:=ifelse(is.na(hightRiskTransAvg6), hightRiskTransAvg6.1, hightRiskTransAvg6)]
featuresWide[, hightRiskTransNum1:=ifelse(is.na(hightRiskTransNum1), hightRiskTransNum1.1, hightRiskTransNum1)]
featuresWide[, hightRiskTransNum6:=ifelse(is.na(hightRiskTransNum6), hightRiskTransNum6.1, hightRiskTransNum6)]
featuresWide[, inJulixinBlanklist:=ifelse(is.na(inJulixinBlanklist), inJulixinBlanklist.1, inJulixinBlanklist)]
featuresWide[, juxinliSuccess:=ifelse(is.na(juxinliSuccess), juxinliSuccess.1, juxinliSuccess)]
featuresWide[, lastMonthOverdrawNum:=ifelse(is.na(lastMonthOverdrawNum), lastMonthOverdrawNum.1, lastMonthOverdrawNum)]
featuresWide[, loansCalls1:=ifelse(is.na(loansCalls1), loansCalls1.1, loansCalls1)]
featuresWide[, loansCalls3:=ifelse(is.na(loansCalls3), loansCalls3.1, loansCalls3)]
featuresWide[, local1year:=ifelse(is.na(local1year), local1year.1, local1year)]
featuresWide[, localFriends:=ifelse(is.na(localFriends), localFriends.1, localFriends)]
featuresWide[, longTimeShutdown:=ifelse(is.na(longTimeShutdown), longTimeShutdown.1, longTimeShutdown)]
featuresWide[, marry:=ifelse(is.na(marry), marry.1, marry)]
featuresWide[, mealsNum:=ifelse(is.na(mealsNum), mealsNum.1, mealsNum)]
featuresWide[, modelScore:=ifelse(is.na(modelScore), modelScore.1, modelScore)]
featuresWide[, monthIncome:=ifelse(is.na(monthIncome), monthIncome.1, monthIncome)]
featuresWide[, multiBorrowNumP1:=ifelse(is.na(multiBorrowNumP1), multiBorrowNumP1.1, multiBorrowNumP1)]
featuresWide[, multiBorrowNumP6:=ifelse(is.na(multiBorrowNumP6), multiBorrowNumP6.1, multiBorrowNumP6)]
featuresWide[, networkTime6:=ifelse(is.na(networkTime6), networkTime6.1, networkTime6)]
featuresWide[, nightConsumeNum:=ifelse(is.na(nightConsumeNum), nightConsumeNum.1, nightConsumeNum)]
featuresWide[, normalContact:=ifelse(is.na(normalContact), normalContact.1, normalContact)]
featuresWide[, post12MonthOverdrawNum:=ifelse(is.na(post12MonthOverdrawNum), post12MonthOverdrawNum.1, post12MonthOverdrawNum)]
featuresWide[, post12PMFeeAmount:=ifelse(is.na(post12PMFeeAmount), post12PMFeeAmount.1, post12PMFeeAmount)]
featuresWide[, post12PMFeeNum:=ifelse(is.na(post12PMFeeNum), post12PMFeeNum.1, post12PMFeeNum)]
featuresWide[, post6MonthOverdrawNum:=ifelse(is.na(post6MonthOverdrawNum), post6MonthOverdrawNum.1, post6MonthOverdrawNum)]
featuresWide[, postConsume02:=ifelse(is.na(postConsume02), postConsume02.1, postConsume02)]
featuresWide[, postConsume24:=ifelse(is.na(postConsume24), postConsume24.1, postConsume24)]
featuresWide[, postConsume46:=ifelse(is.na(postConsume46), postConsume46.1, postConsume46)]
featuresWide[, postMonthConsumeFreg:=ifelse(is.na(postMonthConsumeFreg), postMonthConsumeFreg.1, postMonthConsumeFreg)]
featuresWide[, sex:=ifelse(is.na(sex), sex.1, sex)]
featuresWide[, unionpayHouse:=ifelse(is.na(unionpayHouse), unionpayHouse.1, unionpayHouse)]
featuresWide[, unionpayPosConsumeCityRank:=ifelse(is.na(unionpayPosConsumeCityRank), unionpayPosConsumeCityRank.1, unionpayPosConsumeCityRank)]
featuresWide[, useCardAmountAvg:=ifelse(is.na(useCardAmountAvg), useCardAmountAvg.1, useCardAmountAvg)]
featuresWide[, useCardLastTime:=ifelse(is.na(useCardLastTime), useCardLastTime.1, useCardLastTime)]
featuresWide[, useCardNumPost6:=ifelse(is.na(useCardNumPost6), useCardNumPost6.1, useCardNumPost6)]
featuresWide[, useCardPM:=ifelse(is.na(useCardPM), useCardPM.1, useCardPM)]
featuresWide[, useCardSumRank:=ifelse(is.na(useCardSumRank), useCardSumRank.1, useCardSumRank)]
# featuresWide[, workCondition:=ifelse(is.na(workCondition), workCondition.1, workCondition)]
featuresWide[, zhiceCar:=ifelse(is.na(zhiceCar), zhiceCar.1, zhiceCar)]
featuresWide[, zhiceHouse:=ifelse(is.na(zhiceHouse), zhiceHouse.1, zhiceHouse)]
featuresWide[, zhimaScore:=ifelse(is.na(zhimaScore), zhimaScore.1, zhimaScore)]

featuresWide[, names(tvardictWide):=NULL]

##################################################################################
# 加上target

featuresWide<-merge(featuresWide, target[, c("project_id","Loan_Date","tenor","flgDPD"), with=F], by.x="financingprojectid", by.y="project_id")
# test<-featureAnalysis(featuresWideU, c("financingprojectid","NA","createtime","Loan_Date","flgDPD","flgTest"))
featuresWide[, createtime:=as.POSIXct(createtime)]
featuresWide[, "NA":=NULL]


# 海哥要再造2个变量
featuresWide[, applyHour:=hour(createtime)]
featuresWide[, applyTimeSegment:=ifelse(applyHour>=1 & applyHour<=6, "3", ifelse(applyHour>=9 & applyHour<=20, "1", "2"))]


featuresWide<-merge(featuresWide, cities, by.x="financingprojectid", by.y="financingprojectid", all.x=T)


#####################################################
# not run
# endproduct:
featuresWide


