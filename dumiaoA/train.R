source("~/jimu/sourceFile.R")
trainDataFinal[, flgDPD:=as.factor(flgDPD)]
trainDataFinal[, c("NA", "financingprojectid"):=NULL]
testData[, c("NA", "financingprojectid"):=NULL]


###########################################
# Variable Selection & Model training
options(sqldf.driver="SQLite")

# 1. vanilla logistic
trainVL <- copy(trainDataFinal)
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


logitVL<-glm(flgDPD~., data=trainVL, family=binomial())

testVL<-copy(testData)
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

testVL$score<-predict(logitVL, type='response', testVL)

VLcurve<-roc(testVL$flgDPD, testVL$score)
plot(VLcurve)
auc(VLcurve)
# 0.5809


################################################
# 2. WOE based Logistic
trainWoE <- copy(trainDataFinal)
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
IV[, power:=ifelse(IV>0.3, 2, ifelse(IV>0.1, 1, ifelse(IV>0.02, 0, -1)))]
IVDelete<-IV[power<0, ]

trainWoE[, IVDelete$Variable:=NULL]
trainWoE[, flgDPD:=as.factor(flgDPD)]

logitWoE<-glm(flgDPD~., data=trainWoE, family=binomial())

testWoE<-copy(testData)
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

testWoE$score<-predict(logitWoE, type='response', testWoE)

WoEcurve<-roc(testWoE$flgDPD, testWoE$score)
plot(WoEcurve)
auc(WoEcurve)
# 0.6162


################################################
# 3. Elastic Net










# 4. random forest based logistic











