
featuresWideU[, c("Loan_Date"):=NULL]

# printTable(featuresWideU)

featuresWideU[, sex:=ifelse(sex=='M', 1, 0)]

featuresWideU[, c("LOC_6_var12","LOC_6_var13", "LOC_6_var14", "dc_flag"):=NULL]

featuresResult<-featureAnalysis(featuresWideU, exclude = c("financingprojectid", "createtime", "card_tp"))

write.csv(featuresResult, paste0(boxdata, "featuresAnal.csv"))

# featureAnalysis(featuresWideU, exclude=c("financingprojectid", "createtime", "callBlacklist", "callLaws", "callNetLoanBlank",
#                                          "cellphoneAuth", "card_tp", "ecpPhoneTag", "ecp_eachother", "hasShCISReport", 
#                                          "inBlanklist", "highZhimaScore", "inJulixinBlanklist", "inZhimaBlank", "juxinliSuccess",
#                                          "marry", "longTimeShutdown", "localFriends", "noNeedMobileAuthCheck", "sex", "normalContact", 
#                                          "tachEcp", "trustAddr", "trustIP", "flgDPD", "FLAG_12_var1","LOC_6_var12","LOC_6_var13"
#                                          ,"LOC_6_var14", "dc_flag", "flgTest", "flgTrainTest"))




typeConverter(featuresWideU, c("callBlacklist", "callLaws", "callNetLoanBlank",
                               "cellphoneAuth", "card_tp", "ecpPhoneTag", "ecp_eachother", "hasShCISReport", 
                               "inBlanklist", "highZhimaScore", "inJulixinBlanklist", "inZhimaBlank", "juxinliSuccess",
                               "marry", "longTimeShutdown", "localFriends", "noNeedMobileAuthCheck", "sex", "normalContact", 
                               "trustAddr", "trustIP", "flgDPD","FLAG_12_var1"), "factor")


typeConverter(featuresWideU, c("tachEcp"), "integer")


###################################################################################
trainData <- featuresWideU[is.na(flgValidation),]
validateData<- featuresWideU[flgValidation==1,]


imputeResult<-ggImpute(trainData, fullImpute = F, removeMassiveMissing = F)

featuresWideU[, imputeResult$removeList:=NULL]



write.csv(trainData, paste0(boxdata, "train.csv"))
