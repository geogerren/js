trainWoE <- copy(trainDataFinal)
testWoE<-copy(testData)

################################################
# 2. WOE based Logistic
trainWoE[, applicantContact:=as.factor(applicantContact)]
trainWoE[, callBlacklist:=as.factor(callBlacklist)]
trainWoE[, cardTerm:=as.factor(cardTerm)]
trainWoE[, cellphoneAuth:=as.factor(cellphoneAuth)]
trainWoE[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
trainWoE[, normalContact:=as.factor(normalContact)]
trainWoE[, sex:=as.factor(sex)]
trainWoE[, workCondition:=as.factor(workCondition)]
trainWoE[, cardType:=as.factor(cardType)]
trainWoE[, callLaws:=as.factor(callLaws)]
trainWoE[, rule_name:=as.factor(rule_name)]
trainWoE[, flgDPD:=as.numeric(flgDPD)-1]
infoTable <- create_infotables(data=trainWoE, y="flgDPD", parallel=F)
IV <- data.table(infoTable$Summary)
# IV[, power:=ifelse(IV>0.3, 2, ifelse(IV>0.1, 1, ifelse(IV>0.02, 0, -1)))]
IVDelete<-IV[IV<0.1, ]

trainWoE[, IVDelete$Variable:=NULL]
trainWoE[, flgDPD:=as.factor(flgDPD)]

testWoE[, applicantContact:=as.factor(applicantContact)]
testWoE[, callBlacklist:=as.factor(callBlacklist)]
testWoE[, cardTerm:=as.factor(cardTerm)]
testWoE[, cellphoneAuth:=as.factor(cellphoneAuth)]
testWoE[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
testWoE[, normalContact:=as.factor(normalContact)]
testWoE[, sex:=as.factor(sex)]
testWoE[, workCondition:=as.factor(workCondition)]
testWoE[, cardType:=as.factor(cardType)]
testWoE[, callLaws:=as.factor(callLaws)]
testWoE[, rule_name:=as.factor(rule_name)]
testWoE[, flgDPD:=as.factor(flgDPD)]

testWoE[, IVDelete$Variable:=NULL]


# logitWoE<-glm(flgDPD~., data=trainWoE, weight=wgt, family=binomial())

logitWoE<-glm(flgDPD~., data=trainWoE, family=binomial())

testWoE$score<-predict(logitWoE, type='response', testWoE)

WoEcurve<-roc(testWoE$flgDPD, testWoE$score)
plot(WoEcurve)
# auc(WoEcurve)
# 0.6162

plot(fitted(logitWoE), residuals(logitWoE),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(logitWoE), residuals(logitWoE)))

HMeasure(testWoE$flgDPD, testWoE$score, threshold = 0.90)$metrics






plot_infotables(infoTable, "")

