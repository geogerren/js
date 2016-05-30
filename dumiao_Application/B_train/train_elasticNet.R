################################################
# 3. Elastic Net
# traineNet <- copy(trainDataFinal)
# 
# traineNet[, applicantContact:=ifelse(applicantContact==-1, 999, applicantContact)]
# traineNet[, applicantContact:=as.factor(applicantContact)]
# 
# traineNet[, callBlacklist:=ifelse(callBlacklist==-1, 999, callBlacklist)]
# traineNet[, callBlacklist:=as.factor(callBlacklist)]
# 
# traineNet[, callBlacklist:=ifelse(cardTerm=='-1', 999, cardTerm)]
# traineNet[, cardTerm:=as.factor(cardTerm)]
# 
# traineNet[, cellphoneAuth:=ifelse(cellphoneAuth==-1, 999, cellphoneAuth)]
# traineNet[, cellphoneAuth:=as.factor(cellphoneAuth)]
# 
# traineNet[, noNeedMobileAuthCheck:=ifelse(noNeedMobileAuthCheck==-1, 999, noNeedMobileAuthCheck)]
# traineNet[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
# 
# traineNet[, normalContact:=ifelse(normalContact==-1, 999, normalContact)]
# traineNet[, normalContact:=as.factor(normalContact)]
# 
# traineNet[, sex:=ifelse(sex==-1, 999, sex)]
# traineNet[, sex:=as.factor(sex)]
# 
# traineNet[, workCondition:=ifelse(workCondition==-1, 999, workCondition)]
# traineNet[, workCondition:=as.factor(workCondition)]
# 
# traineNet[, cardType:=ifelse(cardType==-1, 999, cardType)]
# traineNet[, cardType:=as.factor(cardType)]
# 
# traineNet[, callLaws:=ifelse(callLaws==-1, 999, callLaws)]
# traineNet[, callLaws:=as.factor(callLaws)]
# 
# traineNet[, rule_name:=ifelse(rule_name=='tongdunIdMultiLoanNum', 1, ifelse(rule_name=='tongdunPhoneMultiLoanNum', 2, 0))]
# traineNet[, rule_name:=as.factor(rule_name)]
# 
# traineNet[, flgDPD:=ifelse(flgDPD==1, 'bad', 'good')]
# traineNet[, flgDPD:=as.factor(flgDPD)]
# 
# 
# 
# set.seed(2016)
# eNetTrControl<-trainControl(method = "cv", number = 10, search = "random", classProbs = TRUE,
#                             summaryFunction = twoClassSummary, selectionFunction = "oneSE" 
#                             )
# eNet<-train(flgDPD ~ ., data=traineNet, 
#             method="glmnet", trControl=eNetTrControl, 
#             metric="ROC", maximize=TRUE, preProc=c("center","scale"), 
#             tuneGrid = expand.grid(.alpha=seq(.05, 1, length=15), 
#                                    .lambda=c((1:5)/19)))
# 
# testeNet<-copy(testData)
# testeNet[, applicantContact:=ifelse(applicantContact==-1, 999, applicantContact)]
# testeNet[, applicantContact:=as.factor(applicantContact)]
# 
# testeNet[, callBlacklist:=ifelse(callBlacklist==-1, 999, callBlacklist)]
# testeNet[, callBlacklist:=as.factor(callBlacklist)]
# 
# testeNet[, callBlacklist:=ifelse(cardTerm=='-1', 999, cardTerm)]
# testeNet[, cardTerm:=as.factor(cardTerm)]
# 
# testeNet[, cellphoneAuth:=ifelse(cellphoneAuth==-1, 999, cellphoneAuth)]
# testeNet[, cellphoneAuth:=as.factor(cellphoneAuth)]
# 
# testeNet[, noNeedMobileAuthCheck:=ifelse(noNeedMobileAuthCheck==-1, 999, noNeedMobileAuthCheck)]
# testeNet[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
# 
# testeNet[, normalContact:=ifelse(normalContact==-1, 999, normalContact)]
# testeNet[, normalContact:=as.factor(normalContact)]
# 
# testeNet[, sex:=ifelse(sex==-1, 999, sex)]
# testeNet[, sex:=as.factor(sex)]
# 
# testeNet[, workCondition:=ifelse(workCondition==-1, 999, workCondition)]
# testeNet[, workCondition:=as.factor(workCondition)]
# 
# testeNet[, cardType:=ifelse(cardType==-1, 999, cardType)]
# testeNet[, cardType:=as.factor(cardType)]
# 
# testeNet[, callLaws:=ifelse(callLaws==-1, 999, callLaws)]
# testeNet[, callLaws:=as.factor(callLaws)]
# 
# testeNet[, rule_name:=ifelse(rule_name=='tongdunIdMultiLoanNum', 1, ifelse(rule_name=='tongdunPhoneMultiLoanNum', 2, 0))]
# testeNet[, rule_name:=as.factor(rule_name)]
# 
# testeNet[, flgDPD:=ifelse(flgDPD==1, 'bad', 'good')]
# testeNet[, flgDPD:=as.factor(flgDPD)]
# 
# testeNet[, flgDPD:=NULL]
# 
# 
# testeNetPredict<-predict(eNet, testeNet)











