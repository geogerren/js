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
binningDF<-autoBin$woeTable



write.csv(autoBin$woeTable, paste0(boxdata, "autoBin.csv"))




#####################################################################################################################
train<-woeCalc(train, "FLAG_12_var1","flgDPD", binning=c(-Inf, -999, 1, Inf))$resultDT

train<-woeCalc(train, "FLAG_12_var1","flgDPD", binning=c(-Inf, -999, , Inf))$resultDT
