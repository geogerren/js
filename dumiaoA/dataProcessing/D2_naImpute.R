


featuresWideU[financingprojectid %in% c(113187,120779), card_tp:="platinum_card"]
featuresWideU[card_tp=="golden_card", card_tp:="3"]
featuresWideU[card_tp=="platinum_card", card_tp:="4"]
featuresWideU[card_tp=="ordinary_card", card_tp:="0"]


##############################################################################

featuresWideU[, c("Loan_Date"):=NULL]

# printTable(featuresWideU)

featuresWideU[, sex:=ifelse(sex=='M', 1, 0)]

featuresWideU[, c("LOC_6_var12","LOC_6_var13", "LOC_6_var14", "dc_flag"):=NULL]
# 
# featuresResult<-featureAnalysis(featuresWideU, exclude = c("financingprojectid", "createtime", "card_tp"))
# 
# write.csv(featuresResult, paste0(boxdata, "featuresAnal.csv"))

featureAnalysis(featuresWideU, exclude=c("financingprojectid", "createtime", "callBlacklist", "callLaws", "callNetLoanBlank",
                                         "cellphoneAuth", "card_tp", "ecpPhoneTag", "ecp_eachother", "hasShCISReport",
                                         "inBlanklist", "highZhimaScore", "inJulixinBlanklist", "inZhimaBlank", "juxinliSuccess",
                                         "marry", "longTimeShutdown", "localFriends", "noNeedMobileAuthCheck", "sex", "normalContact",
                                         "tachEcp", "trustAddr", "trustIP", "flgDPD", "FLAG_12_var1","LOC_6_var12","LOC_6_var13"
                                         ,"LOC_6_var14", "dc_flag", "flgTest", "flgTrainTest"))




typeConverter(featuresWideU, c("callBlacklist", "callLaws", "callNetLoanBlank",
                                "card_tp", "ecpPhoneTag",  "hasShCISReport", 
                                 "inJulixinBlanklist",  "juxinliSuccess",
                               "marry", "longTimeShutdown", "localFriends",  "sex", "normalContact", 
                                 "flgDPD","FLAG_12_var1"), "factor")


typeConverter(featuresWideU, c("tachEcp"), "integer")






###################################################################################
featuresWideU[juxinliSuccess=='None', juxinliSuccess:="0"]
featuresWideU[FLAG_12_var1=='None', FLAG_12_var1:="0"]

trainData <- featuresWideU[is.na(flgValidation),]
validateData<- featuresWideU[flgValidation==1,]


imputeResult<-ggImpute(trainData, fullImpute = F, removeMassiveMissing = F)
testImpute<-ggImpute(validateData, fullImpute = F, removeMassiveMissing = F)

# featuresWideU[, imputeResult$removeList:=NULL]



foreign::write.foreign(trainData, paste0(boxdata, "train.txt"),paste0(boxdata, "train.sas"),package = "SAS")
foreign::write.foreign(trainData, paste0(boxdata, "test.txt"),paste0(boxdata, "test.sas"),package = "SAS")


# write.csv(trainData, paste0(boxdata, "train.csv"))
# 
# write.csv(validateData, paste0(boxdata, "test.csv"))
# 

