
featuresWideU[financingprojectid %in% c(113187,120779), cardType:="4"]


##############################################################################

# printTable(featuresWideU)

featuresWideU[, sex:=ifelse(sex=='M', 1, 0)]


featuresResult <- featureAnalysis(featuresWideU, exclude=c("financingprojectid", "createtime"))


write.csv(featuresResult, paste0(boxdata, "featuresAnal.csv"))



typeConverter(featuresWideU, c("callBlacklist", "callLaws", "callNetLoanBlank",
                                "ecpPhoneTag",  "hasShCISReport", 
                                 "inJulixinBlanklist",  
                               "marry", "longTimeShutdown", "localFriends",  "sex", "normalContact", 
                                 "flgDPD"), "factor")


typeConverter(featuresWideU, c("tachEcp"), "integer")



###################################################################################


# trainData <- featuresWideU[is.na(flgValidation),]
# testData<- featuresWideU[flgValidation==1,]
# 
# 
# trainImpute<-ggImpute(trainData, fullImpute = F, removeMassiveMissing = F)
# testImpute<-ggImpute(testData, fullImpute = F, removeMassiveMissing = F)

featuresWideUImpute<-ggImpute(featuresWideU, fullImpute = F, removeMassiveMissing = F)

# 
# write.csv(trainData, paste0(boxdata, "train.csv"))
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


