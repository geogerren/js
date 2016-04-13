train <- copy(trainData)
train[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
          "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
          "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
      ]


autoBin<-woeAutoBin(train, "flgDPD")
binningDF<-autoBin$woeTable
write.csv(binningDF, paste0(boxdata, "autoBin.csv"))


#####################################################################################################################
assigningDF <- data.frame()

# 是否有车，0无1有或推测有2无法判断或缺失
FLAG_12_var1_binningDF <- data.table(c("0","1","1","2","2"), c(0,1,2,3,-99999))
FLAG_12_var1_list <- woeCalc(train, "FLAG_12_var1","flgDPD", binning=FLAG_12_var1_binningDF)
# 不打算放进去

# 近6个月'月累计消费超过3000元的商户数占月总消费商户数比例>= 50%'行为出现月份数. 有较高比例的大额消费, 有套现嫌疑, 容易逾期. 
FLAG_6_var11_list<-woeCalc(train, "FLAG_6_var11","flgDPD", binning=c(-Inf, -999, 2, Inf))
train <- FLAG_6_var11_list$resultDT
assigningDF <- rbind(assigningDF, FLAG_6_var11_list$woeVar)


# 近6个月'月累计消费超过3000元的商户消费金额占月总消费金额比例>= 95%'行为出现月份数. 有高比例的大额消费, 有套现嫌疑, 容易逾期. 
FLAG_6_var12_list<-woeCalc(train, "FLAG_6_var12","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- FLAG_6_var12_list$resultDT
assigningDF <- rbind(assigningDF, FLAG_6_var12_list$woeVar)


# 近6个月发生交易月份数. 近6个月无连续交易的, 卡不活跃,缺乏理财能力,容易逾期.
MON_6_var1_list<-woeCalc(train, "MON_6_var1","flgDPD", binning=c(-Inf, 4, Inf))
train <- MON_6_var1_list$resultDT
assigningDF <- rbind(assigningDF, MON_6_var1_list$woeVar)


# 近12个月交易笔数. 近1年交易不过20的, 卡不活跃,或为中介养卡,缺乏理财能力,容易逾期.
RFM_12_var2_list<-woeCalc(train, "RFM_12_var2","flgDPD", binning=c(-Inf, 20, 50, Inf))
train <- RFM_12_var2_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var2_list$woeVar)


# 近12个月取现笔数. 12个月有取现的容易逾期. 
RFM_12_var29_list<-woeCalc(train, "RFM_12_var29","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_12_var29_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var29_list$woeVar)


# 近12个月午夜交易笔数. 有午夜交易习惯的容易逾期. 
RFM_12_var55_list<-woeCalc(train, "RFM_12_var55","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_12_var55_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var55_list$woeVar)


# 近1个月交易笔数. 交易多的容易逾期. 
RFM_1_var1_list<-woeCalc(train, "RFM_1_var1","flgDPD", binning=c(-Inf, -999, 3, Inf))
train <- RFM_1_var1_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var1_list$woeVar)


# 近1个月借贷笔数. 近1个月有借贷的容易逾期. 
RFM_1_var14_list<-woeCalc(train, "RFM_1_var14","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_1_var14_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var14_list$woeVar)


# 近1个月取现笔数. 近1个月有取现的容易逾期. 
RFM_1_var4_list<-woeCalc(train, "RFM_1_var4","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_1_var4_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var4_list$woeVar)


# 三个变量相互矛盾, 无法解释, 不列入. 
# # 近1个月交易金额在所在城市以及卡种的20分位区间. 缺失的容易逾期.
# RFM_1_var5_list<-woeCalc(train, "RFM_1_var5","flgDPD", binning=c(-Inf, -999, 3, Inf))
# train <- RFM_1_var5_list$resultDT
# assigningDF <- rbind(assigningDF, RFM_1_var5_list$woeVar)
# 

# # 过去第3个月交易金额在所在城市以及卡种的20分位区间
# RFM_1_var7_list<-woeCalc(train, "RFM_1_var7","flgDPD", binning=c(-Inf, -999, 8, Inf))
# train <- RFM_1_var7_list$resultDT
# assigningDF <- rbind(assigningDF, RFM_1_var7_list$woeVar)
# 
# 
# # 过去第5个月交易金额在所在城市以及卡种的20分位区间
# RFM_1_var9_list<-woeCalc(train, "RFM_1_var9","flgDPD", binning=c(-Inf, -999, 10, Inf))
# train <- RFM_1_var9_list$resultDT
# assigningDF <- rbind(assigningDF, RFM_1_var9_list$woeVar)


# 近6个月餐饮类交易笔数. 半年无餐饮消费的,无信用生活习惯,卡不活跃,生活轨迹可疑,可能为中介养卡,容易逾期.
RFM_6_var12_list<-woeCalc(train, "RFM_6_var12","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_6_var12_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var12_list$woeVar)


# 近6个月交易笔数. 半年消费不过10笔的,卡不活跃,或为中介养卡,缺乏理财能力,容易逾期.
RFM_6_var2_list<-woeCalc(train, "RFM_6_var2","flgDPD", binning=c(-Inf, 10, Inf))
train <- RFM_6_var2_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var2_list$woeVar)


# 近6个月借贷交易笔数. 半年无借贷消费的,无信用生活习惯,生活轨迹可疑,可能为中介养卡,容易逾期.
RFM_6_var20_list<-woeCalc(train, "RFM_6_var20","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_6_var20_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var20_list$woeVar)


# 近6个月休闲娱乐类交易笔数. 半年无娱乐消费的,生活轨迹可疑,可能为中介养卡,容易逾期.
RFM_6_var21_list<-woeCalc(train, "RFM_6_var21","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_6_var21_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var21_list$woeVar)


# 最近消费日期距今月份数. 消费日期距今较长的容易逾期.
RFM_h_var2_Derived_list<-woeCalc(train, "RFM_h_var2_Derived","flgDPD", binning=c(-Inf, -999, 0.03, 0.3, Inf))
train <- RFM_h_var2_Derived_list$resultDT
assigningDF <- rbind(assigningDF, RFM_h_var2_Derived_list$woeVar)


# 年龄. 有一定工作年限的依然需要借贷,缺乏理财能力,容易逾期.
age_Derived_list<-woeCalc(train, "age","flgDPD", binning=c(-Inf, 27, Inf))
train <- age_Derived_list$resultDT
assigningDF <- rbind(assigningDF, age_Derived_list$woeVar)


# 申请时段，UnexpectedTime	1,WorkHour	2,NonWorkHour	3. 非正常时段贷款容易逾期.
applyTimeSegment_binningDF <- data.table(c("1","1","2"), c(1,2,3))
applyTimeSegment_list<-woeCalc(train, "applyTimeSegment","flgDPD", binning=applyTimeSegment_binningDF)
train <- applyTimeSegment_list$resultDT
assigningDF <- rbind(assigningDF, applyTimeSegment_list$woeVar)


# 平均每月通话数量
avgMonthCall_list<-woeCalc(train, "avgMonthCall","flgDPD", binning=c(-Inf, 150, 270, Inf))
# 不打算放进去


# 孩子数量.汉子数量多,经济压力大,事务繁多,容易逾期.
childrenNum_list<-woeCalc(train, "childrenNum","flgDPD", binning=c(-Inf, 0, Inf))
train <- childrenNum_list$resultDT
assigningDF <- rbind(assigningDF, childrenNum_list$woeVar)



# 长时间关机.长时间关机容易逾期.
longTimeShutdown_list<-woeCalc(train, "longTimeShutdown","flgDPD")
train <- longTimeShutdown_list$resultDT
assigningDF <- rbind(assigningDF, longTimeShutdown_list$woeVar)


# 婚否.已婚却需要借贷,而无法从家庭储蓄满足,说明缺乏理财能力,或背着配偶借贷,目的可疑,容易逾期.
marry_list<-woeCalc(train, "marry","flgDPD")
train <- marry_list$resultDT
assigningDF <- rbind(assigningDF, marry_list$woeVar)


# 智策当月逾期评分.低分容易逾期.
rsk_score_list<-woeCalc(train, "rsk_score","flgDPD", binning=c(-Inf, -999, 620, Inf))
train <- rsk_score_list$resultDT
assigningDF <- rbind(assigningDF, rsk_score_list$woeVar)


# 性别.男性容易逾期.
sex_list<-woeCalc(train, "sex","flgDPD")
train <- sex_list$resultDT
assigningDF <- rbind(assigningDF, sex_list$woeVar)


# 芝麻分.低分容易逾期.
zhimaScore_list<-woeCalc(train, "zhimaScore","flgDPD", binning=c(-Inf, 660, 680, Inf))
train <- zhimaScore_list$resultDT
assigningDF <- rbind(assigningDF, zhimaScore_list$woeVar)


# 卡类型. 如果持有高级卡,还需要小额借贷,有可能是中介养卡,或生活条件剧变,容易逾期.
card_tp_binningDF <- data.table(c("-99999","1","1","2","3"), c(-99999, 0, 1, 3, 4))
card_tp_list<-woeCalc(train, "card_tp","flgDPD", binning=card_tp_binningDF)
train <- card_tp_list$resultDT
assigningDF <- rbind(assigningDF, card_tp_list$woeVar)


# 当前工作年限. 如果在当前工作持续工作,但仍需借贷,说明缺乏理财能力,容易逾期.
currentJobyear_list<-woeCalc(train, "currentJobyear","flgDPD", binning=c(-Inf, 0, 2, Inf))
train <- currentJobyear_list$resultDT
assigningDF <- rbind(assigningDF, currentJobyear_list$woeVar)


# 近一个月与其他贷款机构通话次数. 因为同盾已经筛掉多头借贷,如果客户与其他贷款公司有通话,但是还需要我们的贷款,那我们是唯一一家,且客户急需用钱,他不愿意对唯一财源逾期.
loansCalls1_list<-woeCalc(train, "loansCalls1","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- loansCalls1_list$resultDT
assigningDF <- rbind(assigningDF, loansCalls1_list$woeVar)


# 本地朋友圈. 有本地朋友圈,仍需借贷,说明无理财能力,人缘差,人品差,或朋友穷,容易违约.
localFriends_list_binningDF <- data.table(c("-99999","-99999","0","1"), c(-99999, -1, 0, 1))
localFriends_list<-woeCalc(train, "localFriends","flgDPD", binning=localFriends_list_binningDF)
train <- localFriends_list$resultDT
assigningDF <- rbind(assigningDF, localFriends_list$woeVar)

# 同盾多平台
tongdunMultiLoanNum_list<-woeCalc(train, "tongdunMultiLoanNum","flgDPD")
train <- tongdunMultiLoanNum_list$resultDT
assigningDF <- rbind(assigningDF, tongdunMultiLoanNum_list$woeVar)


write.csv(assigningDF, paste0(boxdata, "assigning.csv"))
###############################################################################################
# not run
# endproduct:
View(assigningDF)
train



