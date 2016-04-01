
featuresWideU[, c("financingprojectid","createtime","Loan_Date"):=NULL]

printTable(featuresWideU)

featuresWideU[, sex:=ifelse(sex=='M', 1, 0)]

featureAnalysis(featuresWideU, exclude=c("financingprojectid", "createtime", "callBlacklist", "callLaws", "callNetLoanBlank",
                                         "cellphoneAuth", "card_tp", "ecpPhoneTag", "ecp_eachother", "hasShCISReport", 
                                         "inBlanklist", "highZhimaScore", "inJulixinBlanklist", "inZhimaBlank", "juxinliSuccess",
                                         "marry", "longTimeShutdown", "localFriends", "noNeedMobileAuthCheck", "sex", "normalContact", 
                                         "tachEcp", "trustAddr", "trustIP", "flgDPD", "FLAG_12_var1","LOC_6_var12","LOC_6_var13"
                                         ,"LOC_6_var14", "dc_flag"))


featuresWideU[, c("LOC_6_var12","LOC_6_var13", "LOC_6_var14", "dc_flag"):=NULL]


typeConverter(featuresWideU, c("callBlacklist", "callLaws", "callNetLoanBlank",
                               "cellphoneAuth", "card_tp", "ecpPhoneTag", "ecp_eachother", "hasShCISReport", 
                               "inBlanklist", "highZhimaScore", "inJulixinBlanklist", "inZhimaBlank", "juxinliSuccess",
                               "marry", "longTimeShutdown", "localFriends", "noNeedMobileAuthCheck", "sex", "normalContact", 
                               "tachEcp", "trustAddr", "trustIP", "flgDPD","FLAG_12_var1"), "factor")

imputeResult<-ggImpute(featuresWideU)

featuresWideU[, imputeResult$removeList:=NULL]



###################################################################################
trainData <- featuresWideU[flgTest==0&is.na(flgTrainTest),]
testData <- featuresWideU[flgTest==0&flgTrainTest==1,]
holdoutData<- featuresWideU[flgTest==1,]









ggImmpute(featuresWide)
