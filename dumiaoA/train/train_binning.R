# 定义查询
# 数据字典 (如果没有)-> C1_featureGen -> 读秒规则V8 -> 数据字典


train <- copy(trainData)
train[, tachEcp:=as.integer(tachEcp)]
train[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
          "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
          "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
      ]
# test<-copy(testData)
# holdout <- copy(holdoutData)

autoBin<-autoWoE(train, "flgDPD")
write.csv(autoBin$woeTable, paste0(boxdata, "autoBin.csv"))



































#####################################################################################################################

train<-woeCalc(train, "age","flgDPD", binning=c(-Inf, 24, 29, Inf))$resultDT

train<-woeCalc(train, "avgMonthCall","flgDPD", binning=c(-Inf, -999, 180, 300, Inf))$resultDT
# train<-woeCalc(train, "callBlacklist","flgDPD")

# 近6个月申请人与紧急联系人的通话次数
train<-woeCalc(train, "callEcpNum","flgDPD", binning=c(-Inf, -999, 30, 150, Inf))$resultDT
# train<-woeCalc(train, "callLaws","flgDPD", binning=c(-Inf, , Inf))
# train<-woeCalc(train, "callNetLoanBlank","flgDPD", binning=c(-Inf, , Inf))


train<-woeCalc(train, "card_tp","flgDPD")$resultDT

# train<-woeCalc(train, "cellphoneAuth","flgDPD", binning=c(-Inf, , Inf))


train<-woeCalc(train, "childrenNum","flgDPD", binning=c(-Inf, 0, Inf))$resultDT

train<-woeCalc(train, "cnp_score","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "cot_cluster","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "cot_score","flgDPD", binning=c(-Inf, , Inf))$resultDT

train<-woeCalc(train, "currentJobyear","flgDPD", binning=c(-Inf, 1, Inf))$resultDT
# train<-woeCalc(train, "ecpPhoneTag","flgDPD", binning=c(-Inf, , Inf))
# train<-woeCalc(train, "ecp_eachother","flgDPD", binning=c(-Inf, , Inf))
train<-woeCalc(train, "goOut120","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "hasShCISReport","flgDPD")$resultDT
# train<-woeCalc(train, "juxinliSuccess","flgDPD", binning=c(-Inf, , Inf))
# train<-woeCalc(train, "loanPurpose","flgDPD", binning=c(-Inf, 0, 1, Inf))
train<-woeCalc(train, "loansCalls1","flgDPD", binning=c(-Inf, -999, 2, 10, Inf))$resultDT
train<-woeCalc(train, "loansCalls3","flgDPD", binning=c(-Inf, -999, 2, 8, Inf))$resultDT
# train<-woeCalc(train, "local1year","flgDPD", binning=c(-Inf, , Inf))
localFriendsBin<-data.table(c("-1, 0", "-1, 0", "1", "-99999"), c(-1, 0, 1, -99999))
train<-woeCalc(train, "localFriends","flgDPD", binning = localFriendsBin)$resultDT
train<-woeCalc(train, "longTimeShutdown","flgDPD")$resultDT
train<-woeCalc(train, "marry","flgDPD")$resultDT
train<-woeCalc(train, "monthIncome","flgDPD", binning=c(-Inf, 1, 3, Inf))$resultDT
train<-woeCalc(train, "networkTime6","flgDPD", binning=c(-Inf, -999, 180, 720, Inf))$resultDT
# train<-woeCalc(train, "normalContact","flgDPD", binning=c(-Inf, , Inf))
train<-woeCalc(train, "sex","flgDPD")$resultDT
# train<-woeCalc(train, "shCISCreditLine","flgDPD", binning=c(-Inf, , Inf))
# train<-woeCalc(train, "shCISCurrentOverDueNum","flgDPD", binning=c(-Inf, -999, Inf))$resultDT
# train<-woeCalc(train, "shCISLoanQueryNo_3month","flgDPD", binning=c(-Inf, -999, Inf))$resultDT
# train<-woeCalc(train, "shCISMaxOverdual_24month","flgDPD", binning=c(-Inf, -999, Inf))$resultDT
# train<-woeCalc(train, "shCISOverdualNum_24month","flgDPD", binning=c(-Inf, -999, Inf))$resultDT
# train<-woeCalc(train, "shCISOverdualNum_3month","flgDPD", binning=c(-Inf, -999, Inf))$resultDT
train<-woeCalc(train, "tachEcp","flgDPD", binning=c(-Inf, -999, 150, Inf))
train<-woeCalc(train, "workCondition","flgDPD", binning=c(-Inf, -999, Inf))$resultDT
train<-woeCalc(train, "zhimaScore","flgDPD", binning=c(-Inf, 670, 700, Inf))$resultDT
train<-woeCalc(train, "tenor","flgDPD", binning=c(-Inf, 3, Inf))$resultDT

# 是否有车，1有2可能3无法判断
FLAG12_V1Cate<-data.table(c("0","1","2,3", "2,3", "-99999"), c(0, 1, 2, 3, -99999))
train<-woeCalc(train, "FLAG_12_var1","flgDPD", binning=FLAG12_V1Cate)$resultDT


train<-woeCalc(train, "RFM_12_var1","flgDPD", binning=c(-Inf, -999, 50000, Inf))$resultDT
train<-woeCalc(train, "RFM_12_var2","flgDPD", binning=c(-Inf, -999, 80, Inf))$resultDT
train<-woeCalc(train, "RFM_12_var29","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "RFM_12_var30","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "RFM_12_var55","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "RFM_12_var56","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
# train<-woeCalc(train, "RFM_12_var58","flgDPD", binning=c(-Inf, , Inf))
# train<-woeCalc(train, "RFM_12_var59","flgDPD", binning=c(-Inf, , Inf))
train<-woeCalc(train, "RFM_6_var1","flgDPD", binning=c(-Inf, -999, 20000, Inf))$resultDT
train<-woeCalc(train, "RFM_6_var2","flgDPD", binning=c(-Inf, -999, 36, Inf))$resultDT
train<-woeCalc(train, "RFM_6_var12","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "RFM_6_var13","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "RFM_6_var14","flgDPD", binning=c(-Inf, -999, 0, Inf))$resultDT
train<-woeCalc(train, "RFM_6_var15","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_6_var17","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_6_var18","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_6_var19","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_6_var20","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_6_var21","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "FLAG_6_var11","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "FLAG_6_var12","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "MON_6_var1","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_3_var6","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_3_var7","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var1","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var2","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var3","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var4","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var5","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var6","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var7","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var8","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var9","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var10","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var11","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var12","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var13","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_1_var14","flgDPD", binning=c(-Inf, , Inf))$resultDT

train<-woeCalc(train, "flag_h_var1","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_56_var1","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "RFM_h_var2","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "rsk_cluster","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "rsk_score","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "wlp_score","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "final_score","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "w_addrUsedNum","flgDPD", binning=c(-Inf, , Inf))$resultDT
train<-woeCalc(train, "w_age","flgDPD", binning=c(-Inf, , Inf))$resultDT
