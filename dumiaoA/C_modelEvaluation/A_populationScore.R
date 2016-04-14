# scoredTrain<-scoreAssignAuto(rawTrain, scoreCard, intercept = 512)
# scoredTest <-scoreAssignAuto(rawTest, scoreCard, intercept = 512)

# trainScoreBand <- banding(scoredTrain, "s_totalScore", "scoreBand")

# Step A. 
# A1. generate features
applicants<-ruleq("select 
t.mod_id,
t.index_id,
t.rule_id,
t.input_val
from t_mod_score t
join project_detail p
on t.service_id = p.service_id
")
featuresWide<-dcast.data.table(features, financingprojectid + createtime ~ key, fun.agg=min, value.var = "value")

# A2. merge unionpay and tongdun
# run 
# /dumiaoA/A_dataProcessing/C2_mergeUnionPay.R 
                          # C3_mergeTongdun.R
                          # D1_moreVars.R
# A3. data impute
# run /dumiaoA/A_dataProcessing/D2_naImpute.R until the following command

typeConverter(featuresWideU, c("callBlacklist", "callLaws", "callNetLoanBlank",
                               "card_tp", "ecpPhoneTag",  "hasShCISReport", 
                               "inJulixinBlanklist",  "juxinliSuccess",
                               "marry", "longTimeShutdown", "localFriends",  "sex", "normalContact", 
                               "FLAG_12_var1"), "factor")
typeConverter(featuresWideU, c("tachEcp"), "integer")

featuresWideU[juxinliSuccess=='None', juxinliSuccess:="0"]
featuresWideU[FLAG_12_var1=='None', FLAG_12_var1:="0"]

fullPopulationImpute<-ggImpute(featuresWideU, fullImpute = F, removeMassiveMissing = F)




