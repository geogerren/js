# 定义查询
# 数据字典 (如果没有)-> C1_featureGen -> 读秒规则V8 -> 数据字典


train <- copy(trainData)
train[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
          "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
          "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
      ]


autoBin<-autoWoE(train, "flgDPD")
binningDF<-autoBin$woeTable
write.csv(binningDF, paste0(boxdata, "autoBin.csv"))



train
c("w_age",	"w_callLaws",	"w_callNetLoanBlank",	"w_local1year",	"w_localFriends",	"w_longTimeShutdown",
"w_ecpPhoneTag",	"w_goOut120",	"w_hasShCISReport",	"w_monthIncome",	"w_networkTime6",	"w_normalContact",
"w_juxinliSuccess",	"w_loanPurpose",	"w_loansCalls1",	"w_postConsume24",	"w_postConsume46",	"w_sex",
"w_shCISCurrentOverDueNum",	"w_shCISLoanQueryNo_3month",	"w_shCISMaxOverdual_24month",	"w_RFM_1_var4",	"w_RFM_1_var5",	"w_RFM_1_var6",
"w_shCISOverdualNum_3month",	"w_workCondition",	"w_zhimaScore",	"w_RFM_1_var8",	"w_RFM_1_var9",	"w_RFM_1_var10",
"w_flgValidation",	"w_RFM_1_var1",	"w_RFM_1_var2",	"w_RFM_1_var12",	"w_RFM_1_var13",	"w_RFM_1_var14",
"w_RFM_3_var7",	"w_RFM_6_var1",	"w_RFM_6_var2",	"w_FLAG_12_var1",	"w_RFM_12_var1",	"w_RFM_12_var2",
"w_RFM_6_var13",	"w_RFM_6_var14",	"w_RFM_6_var15",	"w_RFM_12_var30",	"w_RFM_12_var55",	"w_RFM_12_var56",
"w_RFM_6_var18",	"w_RFM_6_var19",	"w_RFM_6_var20",	"w_RFM_12_var59",	"w_MON_6_var1",	"w_FLAG_6_var11",
"w_card_tp",	"w_cnp_score",	"w_cot_cluster",	"w_cot_score",	"w_applyTimeSegment",	"flgDPD",
"w_flag_h_var1",	"w_RFM_56_var1",	"w_rsk_cluster",	"w_rsk_score",
"w_wlp_score",	"w_RFM_h_var2_Derived",	"w_tongdunMultiLoanNum",	"w_applyHour"
)


#####################################################################################################################
train<-woeCalc(train, "FLAG_12_var1","flgDPD", binning=c(-Inf, -999, 1, Inf))$resultDT

train<-woeCalc(train, "FLAG_12_var1","flgDPD", binning=c(-Inf, -999, , Inf))$resultDT









#####################################################################################################################
# validation data and binning
test <- copy(validateData)
test[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
         "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
         "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
     ]

# test2 <- woeAssignAuto(test, binningDF)


