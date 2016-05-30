###############################################################################
#  binned full model
binTrain[, score:=NULL]
binTest[, score:=NULL]

mBin<-glm(flgDPD~., data=binTrain, family=binomial())
summary(mBin)

binTrain$score <- predict(mBin, type='response', binTrain)
binTest$score <- predict(mBin, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


########################################################
binTrain[, score:=NULL]
binTest[, score:=NULL]

m0glmnetSelected <- glm(flgDPD~w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                          w_avgMonthCall + w_childrenNum + w_consumeLineRate + w_lastMonthOverdrawNum + 
                          w_currentJobyear + w_loansCalls3 + w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
                          w_postMonthConsumeFreg + w_useCardAmountAvg + w_useCardLastTime + w_useCardNumPost6 + 
                          w_zhiceHouse + w_zhimaScore, data=binTrain, family=binomial())


binTrain$score <- predict(m0glmnetSelected, type='response', binTrain)
binTest$score <- predict(m0glmnetSelected, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


#######################################################
binTrain[, score:=NULL]
binTest[, score:=NULL]

m0 = step(m0glmnetSelected, method="both")

binTrain$score <- predict(m0, type='response', binTrain)
binTest$score <- predict(m0, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m0)

m0validate <- glm(flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                    w_avgMonthCall + w_consumeLineRate + w_lastMonthOverdrawNum + 
                    w_loansCalls3 + w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
                    w_postMonthConsumeFreg + w_useCardAmountAvg + w_useCardLastTime + 
                    w_useCardNumPost6 + w_zhiceHouse + w_zhimaScore, data=binTest, family=binomial())

summary(m0validate)

#####################################################
# remove w_useCardNumPost6, pvalue>0.15

m1 <- glm(flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
            w_avgMonthCall + w_consumeLineRate + w_lastMonthOverdrawNum + 
            w_loansCalls3 + w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
            w_postMonthConsumeFreg + w_useCardAmountAvg + w_useCardLastTime + 
            w_zhiceHouse + w_zhimaScore, data=binTrain, family=binomial())

binTrain$score <- predict(m1, type='response', binTrain)
binTest$score <- predict(m1, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m1)

m1validate <- glm(flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                    w_avgMonthCall + w_consumeLineRate + w_lastMonthOverdrawNum + 
                    w_loansCalls3 + w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
                    w_postMonthConsumeFreg + w_useCardAmountAvg + w_useCardLastTime + 
                    w_zhiceHouse + w_zhimaScore, data=binTest, family=binomial())


summary(m1validate)


#####################################################
# remove localFriends, w_age, w_zhiceHouse, RFM_6_var12      (not explainable)
# 
m2 <- glm(flgDPD ~ w_applyTimeSegment + 
            w_avgMonthCall + w_consumeLineRate + w_lastMonthOverdrawNum + 
            w_loansCalls3 + w_longTimeShutdown + w_multiBorrowNumP6 + w_postMonthConsumeFreg +
            w_useCardAmountAvg + w_useCardLastTime + 
            w_zhimaScore, data=binTrain, family=binomial())

binTrain$score <- predict(m2, type='response', binTrain)
binTest$score <- predict(m2, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m2)

auc(as.numeric(as.character(binTrain$flgDPD)), binTrain$score)
auc(as.numeric(as.character(binTest$flgDPD)), binTest$score)
#####################################################



mBinFinal<-m21
# not run
# endproduct:
mBinFinal


