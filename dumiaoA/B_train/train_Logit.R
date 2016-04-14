source("~/jimu/sourceFile.R")

trainVL <- copy(trainDataFinal)
testVL<-copy(testData)

###########################################
# Variable Selection & Model training
options(sqldf.driver="SQLite")

# 1. vanilla logistic
trainVL[, applicantContact:=as.factor(applicantContact)]
trainVL[, callBlacklist:=as.factor(callBlacklist)]
trainVL[, cardTerm:=as.factor(cardTerm)]
trainVL[, cellphoneAuth:=as.factor(cellphoneAuth)]
trainVL[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
trainVL[, normalContact:=as.factor(normalContact)]
trainVL[, sex:=as.factor(sex)]
trainVL[, workCondition:=as.factor(workCondition)]
trainVL[, cardType:=as.factor(cardType)]
trainVL[, callLaws:=as.factor(callLaws)]
trainVL[, rule_name:=as.factor(rule_name)]
trainVL[, flgDPD:=as.factor(flgDPD)]

testVL[, applicantContact:=as.factor(applicantContact)]
testVL[, callBlacklist:=as.factor(callBlacklist)]
testVL[, cardTerm:=as.factor(cardTerm)]
testVL[, cellphoneAuth:=as.factor(cellphoneAuth)]
testVL[, noNeedMobileAuthCheck:=as.factor(noNeedMobileAuthCheck)]
testVL[, normalContact:=as.factor(normalContact)]
testVL[, sex:=as.factor(sex)]
testVL[, workCondition:=as.factor(workCondition)]
testVL[, cardType:=as.factor(cardType)]
testVL[, callLaws:=as.factor(callLaws)]
testVL[, rule_name:=as.factor(rule_name)]
testVL[, flgDPD:=as.factor(flgDPD)]



logitVL<-glm(flgDPD~., data=trainVL, family=binomial())
testVL$score<-predict(logitVL, type='response', testVL)

VLcurve<-roc(testVL$flgDPD, testVL$score)
plot(VLcurve)
auc(VLcurve)
# 0.5809
# pred<-prediction(testVL$score, testVL$flgDPD)
# perf<-performance(pred, "tpr", "fpr")
# max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])

HMeasure(testVL$flgDPD, testVL$score)

plot(fitted(logitVL), residuals(logitVL),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(logitVL), residuals(logitVL)))


