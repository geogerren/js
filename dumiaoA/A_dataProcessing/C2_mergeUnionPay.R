# source("E:/Seafiles/Jimu/Code/js/sourceFile.R")

# options(warn=-1)
featuresWideU<-merge(featuresWide, unionpayAgg, by="financingprojectid", all.x=T)

# 6个月内有刷卡消费的月份数
featuresWideU[, useCardNumPost6:=ifelse(!is.na(useCardNumPost6), useCardNumPost6, MON_6_var1)]

# 最近一次消费距申请日间隔
featuresWideU[, useCardLastTime:=ifelse(!is.na(useCardLastTime), useCardLastTime, difftime(as.POSIXct(createtime), RFM_h_var2, units = "days"))]
featuresWideU[, useCardLastTime:=ceiling(as.numeric(ifelse(useCardLastTime>0, useCardLastTime, 0)))]

# 消费频率
featuresWideU[, consumeFreg:=ifelse(!is.na(consumeFreg), consumeFreg, as.numeric(RFM_6_var2)/6)]

# 过去12个月透支取现次数不得高于4次，且最近1个月无透支取现记录
featuresWideU[, post12MonthOverdrawNum:=ifelse(!is.na(post12MonthOverdrawNum), post12MonthOverdrawNum, RFM_12_var29)]


featuresWideU[, lastMonthOverdrawNum:=ifelse(!is.na(lastMonthOverdrawNum), lastMonthOverdrawNum, RFM_1_var4)]

# 过去6个月银联信用卡交易金额本市排名 (第六个月?)
featuresWideU[, useCardSumRank:=ifelse(!is.na(useCardSumRank), useCardSumRank, as.numeric(RFM_1_var10)*5)]

# 月均消费金额
featuresWideU[, useCardAmountAvg:=ifelse(!is.na(useCardAmountAvg), useCardAmountAvg, as.numeric(RFM_6_var1)/6)]

# 近一个月高危商户交易次数
featuresWideU[, hightRiskTransNum1:=ifelse(!is.na(hightRiskTransNum1), hightRiskTransNum1, RFM_1_var11)]

# 根过去6个月信用卡交易金额本市排名有何区别           ?是不是跟上面23行的一样?
featuresWideU[, consumeTop:=ifelse(!is.na(consumeTop), consumeTop, as.numeric(RFM_1_var10)*5)]

# 近12个月信用卡取现笔数 ?与17行的一样?
featuresWideU[, creditCashFreq:=ifelse(!is.na(creditCashFreq), creditCashFreq, RFM_12_var29)]

# 近12个月信用卡取现平均金额
featuresWideU[, creditCashAvg:=ifelse(!is.na(creditCashAvg), creditCashAvg, RFM_12_var30/RFM_12_var29)]

# 信用卡套现3出现的月份数
featuresWideU[, creditWD3Months:=ifelse(!is.na(creditWD3Months), creditWD3Months, ifelse(FLAG_6_var11>FLAG_6_var12, FLAG_6_var11, FLAG_6_var12))]

# 银联声明房产, 车产
featuresWideU[, zhiceHouse:=ifelse(!is.na(zhiceHouse), zhiceHouse, ifelse(!is.na(unionpayHouse), unionpayHouse, flag_h_var1))]
featuresWideU[, zhiceCar:=ifelse(!is.na(zhiceCar), zhiceCar, ifelse(!is.na(declareCar), declareCar, FLAG_12_var1))]

# 信用卡最近一月消费频率
featuresWideU[, postMonthConsumeFreg:=ifelse(!is.na(postMonthConsumeFreg), postMonthConsumeFreg, RFM_1_var1)]

# 卡等级
featuresWideU[, cardType:=ifelse(!is.na(cardType), cardType, ifelse(card_tp=='platinum_card', 4, ifelse(card_tp=='golden_card', 3, ifelse(card_tp=='ordinary_card', 1, 0))))]

# 近12个月缴纳物业费笔数
featuresWideU[, post12PMFeeNum:=ifelse(!is.na(post12PMFeeNum), post12PMFeeNum, RFM_12_var58)]

# 近12个月缴纳物业费金额
featuresWideU[, post12PMFeeAmount:=ifelse(!is.na(post12PMFeeAmount), post12PMFeeAmount, RFM_12_var59)]

# 近6个月娱乐消费频率
featuresWideU[, amuseConsumeFreq:=ifelse(!is.na(amuseConsumeFreq), amuseConsumeFreq, RFM_6_var21)]

# 近3个月夜消费频率
featuresWideU[, nightConsumeNum:=ifelse(!is.na(nightConsumeNum), nightConsumeNum, RFM_3_var6/3)]

# 近6个月疑似多平台借贷笔数
featuresWideU[, multiBorrowNumP6:=ifelse(!is.na(multiBorrowNumP6), multiBorrowNumP6, RFM_6_var20)]

# 近1个月疑似多平台借贷笔数
featuresWideU[, multiBorrowNumP1:=ifelse(!is.na(multiBorrowNumP1), multiBorrowNumP1, RFM_1_var14)]

# 近6个月高危商户消费笔数
featuresWideU[, hightRiskTransNum6:=ifelse(!is.na(hightRiskTransNum6), hightRiskTransNum6, RFM_6_var17)]

# 近6个月高危商户消费平均金额
featuresWideU[, hightRiskTransAvg6:=ifelse(!is.na(hightRiskTransAvg6), hightRiskTransAvg6, RFM_6_var18/RFM_6_var17)]



# 申请城市在个人所有消费的城市中排名第几(1,2,3,0=非前三)
featuresWideU[, unionpayPosConsumeCityRank:=ifelse(city==LOC_6_var12, 1, ifelse(city==LOC_6_var13, 2, ifelse(city==LOC_6_var14, 3, 0)))]


featuresWideU[, card_tp:=NULL]
featuresWideU[, MON_6_var1:=NULL]
featuresWideU[, RFM_h_var2:=NULL]
featuresWideU[, RFM_6_var2:=NULL]
featuresWideU[, RFM_1_var4:=NULL]
featuresWideU[, RFM_6_var1:=NULL]
featuresWideU[, RFM_1_var11:=NULL]
featuresWideU[, RFM_12_var29:=NULL]
featuresWideU[, RFM_12_var30:=NULL]
featuresWideU[, FLAG_6_var11:=NULL]
featuresWideU[, FLAG_6_var12:=NULL]
featuresWideU[, unionpayHouse:=NULL]
featuresWideU[, flag_h_var1:=NULL]
featuresWideU[, declareCar:=NULL]
featuresWideU[, FLAG_12_var1:=NULL]
featuresWideU[, RFM_1_var1:=NULL]
featuresWideU[, RFM_12_var58:=NULL]
featuresWideU[, RFM_12_var59:=NULL]
featuresWideU[, RFM_6_var21:=NULL]
featuresWideU[, RFM_3_var6:=NULL]
featuresWideU[, RFM_6_var20:=NULL]
featuresWideU[, RFM_6_var17:=NULL]
featuresWideU[, RFM_6_var18:=NULL]
featuresWideU[, LOC_6_var12:=NULL]
featuresWideU[, LOC_6_var13:=NULL]
featuresWideU[, LOC_6_var14:=NULL]


featuresWideU[, c("cnp_score","cot_cluster","cot_score","rsk_cluster","rsk_score","wlp_score"):=NULL]



# 智策回传的CSV有duplicate(3个)
featuresWideU<-featuresWideU[!duplicated(financingprojectid),]

#####################################################
# not run
# endproduct:
featuresWideU # featuresWide with unionPay and tongdun added, hence featuresWide(U)

