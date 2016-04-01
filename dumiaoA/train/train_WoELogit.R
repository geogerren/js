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
IVKeep <- IV[IV>=0.1, ]

# plot_infotables(infoTable, "consumeLineRate")

trainWoE[, IVDelete$Variable:=NULL]
trainWoE[, flgDPD:=as.numeric(flgDPD)]

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


###################################################################
# Binning non-linear variables
# trainBin <- copy(trainWoE)
# 
# infoTable$Table$networkTime6$networkTime6
# trainBin$networkTime6 <- cut(trainBin$networkTime6, 
#                                 c(-Inf,160,170,179,
#                                  203,490,785, 993, 1371,
#                                  2091,Inf))
# infoTable$Table$tenor$tenor
# trainBin$tenor <- cut(trainBin$tenor, c(-Inf,2,3,6,Inf))
# 
# infoTable$Table$useCardAmountAvg$useCardAmountAvg
# trainBin$useCardAmountAvg <- cut(trainBin$useCardAmountAvg, 
#                                  c(-Inf, 940.18, 
#                                   1650.7, 2619.07, 3882.9, 
#                                   4824.55, 5840.28, 8311.45,
#                                   11687.43, 20158.24, Inf))
# 
# infoTable$Table$callEcpNum$callEcpNum
# trainBin$callEcpNum <- cut(trainBin$callEcpNum, 
#                            c(-Inf,9,23,45,74,110,178,264,379,Inf))
# 
# infoTable$Table$consumeLineRate$consumeLineRate
# trainBin$consumeLineRate <- cut(trainBin$consumeLineRate, 
#                                 c(-Inf,-0.02,-0.01,0,0.01,0.03,0.09,0.13,Inf))
# 
# infoTable$Table$consumeTop$consumeTop
# trainBin$consumeTop <- cut(trainBin$consumeTop, 
#                                 c(-Inf,10,15,20,25,40,Inf))
# 
# infoTable$Table$loansCalls3$loansCalls3
# trainBin$loansCalls3 <- cut(trainBin$loansCalls3, 
#                            c(-Inf,2,4,6,8,Inf))
# 
# infoTable$Table$month62ConsumeAvg$month62ConsumeAvg
# trainBin$month62ConsumeAvg <- cut(trainBin$month62ConsumeAvg, 
#                             c(-Inf,1292.5,2860.83,4620.38,8672,21021.06,Inf))
# 
# infoTable$Table$loansCalls1$loansCalls1
# trainBin$loansCalls1 <- cut(trainBin$loansCalls1, 
#                                   c(-Inf,1,2,4,5,8,11,18,Inf))
# 
# infoTable$Table$avgMonthCall$avgMonthCall
# trainBin$avgMonthCall <- cut(trainBin$avgMonthCall, 
#                             c(-Inf,63,94,122,148,180,210,260,330,455,Inf))
# 
# infoTable$Table$age$age
# trainBin$age <- cut(trainBin$age, 
#                              c(-Inf,23,242,26,27,28,29,32,35,Inf))
# 
# infoTable$Table$useCardSumRank$useCardSumRank
# trainBin$useCardSumRank <- cut(trainBin$useCardSumRank, 
#                     c(-Inf,8.33,11.67,16.67,22.5,25,26.67,32.5,38.33,46.67,Inf))
# 
# infoTable$Table$consumeFreg$consumeFreg
# trainBin$consumeFreg <- cut(trainBin$consumeFreg, 
#                                c(-Inf,1,2,3,4,6,9,Inf))
# 
# 
# #######################
# testBin <- copy(testWoE)
# 
# testBin$networkTime6 <- cut(testBin$networkTime6, 
#                              c(-Inf,160,170,179,
#                                203,490,785, 993, 1371,
#                                2091,Inf))
# testBin$tenor <- cut(testBin$tenor, c(-Inf,2,3,6,Inf))
# 
# testBin$useCardAmountAvg <- cut(testBin$useCardAmountAvg, 
#                                  c(-Inf, 940.18, 
#                                    1650.7, 2619.07, 3882.9, 
#                                    4824.55, 5840.28, 8311.45,
#                                    11687.43, 20158.24, Inf))
# 
# testBin$callEcpNum <- cut(testBin$callEcpNum, 
#                            c(-Inf,9,23,45,74,110,178,264,379,Inf))
# 
# testBin$consumeLineRate <- cut(testBin$consumeLineRate, 
#                                 c(-Inf,-0.02,-0.01,0,0.01,0.03,0.09,0.13,Inf))
# 
# testBin$consumeTop <- cut(testBin$consumeTop, 
#                            c(-Inf,10,15,20,25,40,Inf))
# 
# testBin$loansCalls3 <- cut(testBin$loansCalls3, 
#                             c(-Inf,2,4,6,8,Inf))
# 
# testBin$month62ConsumeAvg <- cut(testBin$month62ConsumeAvg, 
#                                   c(-Inf,1292.5,2860.83,4620.38,8672,21021.06,Inf))
# 
# testBin$loansCalls1 <- cut(testBin$loansCalls1, 
#                             c(-Inf,1,2,4,5,8,11,18,Inf))
# 
# testBin$avgMonthCall <- cut(testBin$avgMonthCall, 
#                              c(-Inf,63,94,122,148,180,210,260,330,455,Inf))
# 
# testBin$age <- cut(testBin$age, 
#                     c(-Inf,23,242,26,27,28,29,32,35,Inf))
# 
# testBin$useCardSumRank <- cut(testBin$useCardSumRank, 
#                                c(-Inf,8.33,11.67,16.67,22.5,25,26.67,32.5,38.33,46.67,Inf))
# 
# testBin$consumeFreg <- cut(testBin$consumeFreg, 
#                             c(-Inf,1,2,3,4,6,9,Inf))



###############################################################################

logitWoE<-glm(flgDPD~., data=trainWoE, family=binomial())

testBin$score<-predict(logitWoE, type='response', testWoE)

WoEcurve<-roc(testWoE$flgDPD, testWoE$score)
plot(WoEcurve)
# auc(WoEcurve)
# 0.6162

plot(fitted(logitWoE), residuals(logitWoE),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(logitWoE), residuals(logitWoE)))

HMeasure(testBin$flgDPD, testBin$score, threshold = 0.90)$metrics


