trainDataFinal[, flgDPD:=as.factor(flgDPD)]
trainDataFinal[, "NA":=NULL]
trainDataFinal[, financingprojectid:=NULL]
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








# 2. WOE based Logistic
infoTable <- create_infotables(data=trainDataFinal, y="flgDPD", parallel=F)
IV <- data.table(infoTable$Summary)
IV[, power:=ifelse(IV>0.3, 2, ifelse(IV>0.1, 1, ifelse(IV>0.02, 0, -1)))]













# 3. random forest based logistic











