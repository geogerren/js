###############################################################################
#  binned model
mBin<-glm(flgDPD~., data=binTrain, family=binomial())
summary(mBin)

binTrain$score <- predict(mBin, type='response', binTrain)
binTest$score <- predict(mBin, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


########################################################
stepwise = step(mBin, method="both")

binTrain$score <- predict(stepwise, type='response', binTrain)
binTest$score <- predict(stepwise, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(backwards)

#####################################################
# remove RFM_1_var1, negative coefficient
# w_RFM_1_var4, w_RFM_6_var12, pvalue too small
m1 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + w_age + 
            w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore + 
            w_localFriends + w_tongdunMultiLoanNum, data=binTrain, family=binomial())

binTrain$score <- predict(m1, type='response', binTrain)
binTest$score <- predict(m1, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m1)

m1validate <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + w_age + 
                    w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore + 
                    w_localFriends + w_tongdunMultiLoanNum, data=binTest, family=binomial())


summary(m1validate)



#####################################################
# remove tongdun
m2 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + w_age + 
            w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore + 
            w_localFriends, data=binTrain, family=binomial())

binTrain$score <- predict(m2, type='response', binTrain)
binTest$score <- predict(m2, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m2)

m2validate <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + w_age + 
            w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore + 
            w_localFriends, data=binTest, family=binomial())


summary(m2validate)



#####################################################
# remove age, discrepancy in train and validate
m3 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore + 
            w_localFriends, data=binTrain, family=binomial())

binTrain$score <- predict(m3, type='response', binTrain)
binTest$score <- predict(m3, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m3)

m3validate <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
                    w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore + 
                    w_localFriends, data=binTest, family=binomial())


summary(m3validate)


#####################################################
# remove age, local freinds
m4 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore, data=binTrain, family=binomial())

binTrain$score <- predict(m4, type='response', binTrain)
binTest$score <- predict(m4, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m4)

m4validate <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
                    w_applyTimeSegment + w_longTimeShutdown + w_zhimaScore
                    , data=binTest, family=binomial())


summary(m4validate)



mBinFinal<-m4
auc(as.numeric(as.character(binTrain$flgDPD)), binTrain$score)
auc(as.numeric(as.character(binTest$flgDPD)), binTest$score)
#####################################################
# not run
# endproduct:
mBinFinal


