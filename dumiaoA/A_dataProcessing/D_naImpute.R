
featuresWideU[financingprojectid %in% c(113187,120779), card_tp:="platinum_card"]
featuresWideU[card_tp=="golden_card", card_tp:="3"]
featuresWideU[card_tp=="platinum_card", card_tp:="4"]
featuresWideU[card_tp=="ordinary_card", card_tp:="0"]


##############################################################################

featuresWideU[, c("Loan_Date"):=NULL]

# printTable(featuresWideU)

featuresWideU[, sex:=ifelse(sex=='M', 1, 0)]

featuresWideU[, c("LOC_6_var12","LOC_6_var13", "LOC_6_var14", "dc_flag"):=NULL]


featuresResult <- featureAnalysis(featuresWideU, exclude=c("financingprojectid", "createtime", "callBlacklist", "callLaws", "callNetLoanBlank",
                                         "cellphoneAuth", "ecpPhoneTag", "ecp_eachother", "hasShCISReport",
                                         "inBlanklist", "highZhimaScore", "inJulixinBlanklist", "inZhimaBlank", "juxinliSuccess",
                                         "marry", "longTimeShutdown", "localFriends", "noNeedMobileAuthCheck", "normalContact",
                                         "tachEcp", "trustAddr", "trustIP", "flgDPD", "LOC_6_var12","LOC_6_var13"
                                         ,"LOC_6_var14", "dc_flag", "flgTest", "flgTrainTest"))

# write.csv(featuresResult, paste0(boxdata, "featuresAnal.csv"))



typeConverter(featuresWideU, c("callBlacklist", "callLaws", "callNetLoanBlank",
                                "card_tp", "ecpPhoneTag",  "hasShCISReport", 
                                 "inJulixinBlanklist",  
                               "marry", "longTimeShutdown", "localFriends",  "sex", "normalContact", 
                                 "flgDPD"), "factor")


typeConverter(featuresWideU, c("tachEcp"), "integer")



###################################################################################


trainData <- featuresWideU[is.na(flgValidation),]
testData<- featuresWideU[flgValidation==1,]


trainImpute<-ggImpute(trainData, fullImpute = F, removeMassiveMissing = F)
testImpute<-ggImpute(testData, fullImpute = F, removeMassiveMissing = F)

# table(trainData$consumeFreg, useNA = "ifany")

# 
write.csv(trainData, paste0(boxdata, "train.csv"))
# 
# write.csv(validateData, paste0(boxdata, "test.csv"))
# 
# 
# 
# trainData[, c("financingprojectid", "createtime"):=NULL]
# testData[, c("financingprojectid", "createtime"):=NULL]

###############################################################################################
# not run
# endproduct:
trainData
testData


