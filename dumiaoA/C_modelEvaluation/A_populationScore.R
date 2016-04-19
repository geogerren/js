
# Step A. 
# A1. generate features
mval_applicants<-ruleq("select 
t.mod_id,
t.index_id,
t.rule_id,
t.input_val
from t_mod_score t
")



featureGen(features, 10016, 1000036.2, "longTimeShutdown")
featureGen(features, 10008, 1000012, "zhimaScore")
featureGen(features, 10016, 1000036.1, "localFriends")













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




