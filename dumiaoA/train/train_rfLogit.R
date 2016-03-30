################################################
# 4. random forest based logistic

trainRF <- copy(trainDataFinal)
trainRF[, applicantContact:=as.factor(applicantContact)]
trainRF[, callBlacklist:=as.factor(callBlacklist)]
trainRF[, cardTerm:=as.factor(cardTerm)]
trainRF[, cellphoneAuth:=as.factor(cellphoneAuth)]
trainRF[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
trainRF[, normalContact:=as.factor(normalContact)]
trainRF[, sex:=as.factor(sex)]
trainRF[, workCondition:=as.factor(workCondition)]
trainRF[, cardType:=as.factor(cardType)]
trainRF[, callLaws:=as.factor(callLaws)]
trainRF[, rule_name:=as.factor(rule_name)]
trainRF[, flgDPD:=as.factor(flgDPD)]


rf.fit<-randomForest(flgDPD~., data=trainRF, importance=TRUE, proximity=TRUE, ntree=1500, keep.forest=TRUE)
rfImp<-importance(rf.fit)
rfImpdf<-as.data.frame(rfImp)
rfDelete<-row.names(rfImpdf[rfImpdf$MeanDecreaseAccuracy<=0, ])

trainRF[, flgDPD:=as.numeric(flgDPD)-1]


trainRFLogit<-copy(trainRF)
trainRFLogit[, rfDelete$rfDelete:=NULL]

logitRF<-glm(flgDPD~., data=trainRFLogit, family=binomial())



testRFLogit<-copy(testData)
testRFLogit[, applicantContact:=as.factor(applicantContact)]
levels(testRFLogit$applicantContact)<-levels(trainRF$applicantContact)

testRFLogit[, callBlacklist:=as.factor(callBlacklist)]
levels(testRFLogit$callBlacklist)<-levels(trainRF$callBlacklist)

testRFLogit[, cardTerm:=as.factor(cardTerm)]
levels(testRFLogit$cardTerm)<-levels(trainRF$cardTerm)

testRFLogit[, cellphoneAuth:=as.factor(cellphoneAuth)]
levels(testRFLogit$cellphoneAuth)<-levels(trainRF$cellphoneAuth)

testRFLogit[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
levels(testRFLogit$noNeedMobileAuthCheck)<-levels(trainRF$noNeedMobileAuthCheck)

testRFLogit[, normalContact:=as.factor(normalContact)]
levels(testRFLogit$normalContact)<-levels(trainRF$normalContact)

testRFLogit[, sex:=as.factor(sex)]
levels(testRFLogit$sex)<-levels(trainRF$sex)

testRFLogit[, workCondition:=as.factor(workCondition)]
levels(testRFLogit$workCondition)<-levels(trainRF$workCondition)

testRFLogit[, cardType:=as.factor(cardType)]
levels(testRFLogit$cardType)<-levels(trainRF$cardType)

testRFLogit[, callLaws:=as.factor(callLaws)]
levels(testRFLogit$callLaws)<-levels(trainRF$callLaws)

testRFLogit[, rule_name:=as.factor(rule_name)]
levels(testRFLogit$rule_name)<-levels(trainRF$rule_name)



#############################

# testRFLogit[, rfDelete$rfDelete:=NULL]

testRFLogit$score<-predict(logitRF, type='response', testRFLogit)

RFcurve<-roc(testRFLogit$flgDPD, testRFLogit$score)
plot(RFcurve)
# auc(RFcurve)
# 0.566

rfImpdf<-rfImpdf[order(rfImpdf$MeanDecreaseAccuracy),]


###############recursively find deletions
rfDelete<-row.names(rfImpdf[1:67, ])
trainRFLogit<-copy(trainRF)
rfDelete<-as.data.table(rfDelete)
trainRFLogit[, rfDelete$rfDelete:=NULL]
logitRF<-glm(flgDPD~., data=trainRFLogit, family=binomial())


# testRFLogit[, rfDelete$rfDelete:=NULL]
testRFLogit$score<-predict(logitRF, type='response', testRFLogit)

RFcurve<-roc(testRFLogit$flgDPD, testRFLogit$score)
plot(RFcurve)


