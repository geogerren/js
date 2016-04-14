###############################################################################
#  binned model
mBin<-glm(flgDPD~., data=binTrain, family=binomial())
summary(mBin)

binTrain$score <- predict(mBin, type='response', binTrain)
binTest$score <- predict(mBin, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)
auc(as.numeric(as.character(binTrain$flgDPD)), binTrain$score)
auc(as.numeric(as.character(binTest$flgDPD)), binTest$score)


########################################################
backwards = step(mBin)

mBinBack <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_1_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
                  w_applyTimeSegment + w_childrenNum + w_longTimeShutdown + 
                  w_zhimaScore + w_localFriends, data=binTrain, family=binomial())

binTrain$score <- predict(mBinBack, type='response', binTrain)
binTest$score <- predict(mBinBack, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

auc(as.numeric(as.character(binTrain$flgDPD)), binTrain$score)
auc(as.numeric(as.character(binTest$flgDPD)), binTest$score)

########################################################
both = step(mBin, direction="both")

mBinBoth <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_1_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
                  w_applyTimeSegment + w_childrenNum + w_longTimeShutdown + 
                  w_zhimaScore + w_localFriends, data=binTrain, family=binomial())

binTrain$score <- predict(mBinBoth, type='response', binTrain)
binTest$score <- predict(mBinBoth, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(mBinBoth)

#####################################################
# remove localFriends
m1 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_1_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_childrenNum + w_longTimeShutdown + 
            w_zhimaScore , data=binTrain, family=binomial())

binTrain$score <- predict(m1, type='response', binTrain)
binTest$score <- predict(m1, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m1)

m1validate <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_1_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_childrenNum + w_longTimeShutdown + 
            w_zhimaScore + w_tongdunMultiLoanNum, data=binTest, family=binomial())

binTrain$score <- predict(m1validate, type='response', binTrain)
binTest$score <- predict(m1validate, type='response', binTest)

summary(m1validate)




#####################################################
# remove RFM_1_var1, negative coefficient
m2 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_childrenNum + w_longTimeShutdown + 
            w_zhimaScore, data=binTrain, family=binomial())

binTrain$score <- predict(m2, type='response', binTrain)
binTest$score <- predict(m2, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m2)


#####################################################
# remove childrenNum, 用户自填
m3 <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_longTimeShutdown + 
            w_zhimaScore, data=binTrain, family=binomial())

binTrain$score <- predict(m3, type='response', binTrain)
binTest$score <- predict(m3, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


m3validate <- glm(flgDPD ~ w_MON_6_var1 + w_RFM_6_var20 + w_RFM_h_var2_Derived + 
                    w_applyTimeSegment + w_longTimeShutdown + 
                    w_zhimaScore, data=binTest, family=binomial())


summary(m3validate)




mBinFinal<-m2
#####################################################
# not run
# endproduct:
mBinFinal


