###############################################################################
#  binned model
mBin<-glm(flgDPD~., data=binTrain, family=binomial())
summary(mBin)

binTrain$score <- predict(mBin, type='response', binTrain)
binTest$score <- predict(mBin, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)
# 0.3074

########################################################
backwards = step(mBin)

mBinBack <- glm(flgDPD ~ w_RFM_12_var29 + w_RFM_12_var55 + w_RFM_1_var1 + w_RFM_1_var4 + 
                  w_RFM_6_var12 + w_RFM_6_var20 + w_RFM_6_var21 + w_RFM_h_var2_Derived + 
                  w_applyTimeSegment + w_childrenNum + w_rsk_score + w_card_tp + 
                  w_currentJobyear + w_loansCalls1 + w_localFriends, data=binTrain, family=binomial())

binTrain$score <- predict(mBinBack, type='response', binTrain)
binTest$score <- predict(mBinBack, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


########################################################
both = step(mBin, direction="both")

mBinBoth <- glm(flgDPD ~ w_RFM_12_var29 + w_RFM_12_var55 + w_RFM_1_var1 + w_RFM_1_var4 + 
                  w_RFM_6_var12 + w_RFM_6_var20 + w_RFM_6_var21 + w_RFM_h_var2_Derived + 
                  w_applyTimeSegment + w_childrenNum + w_rsk_score + w_card_tp + 
                  w_currentJobyear + w_loansCalls1 + w_localFriends, data=binTrain, family=binomial())

binTrain$score <- predict(mBinBoth, type='response', binTrain)
binTest$score <- predict(mBinBoth, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


#####################################################
mBinTD <- glm(flgDPD ~w_RFM_12_var29 + w_RFM_12_var55 + w_RFM_1_var1 + w_RFM_1_var4 + 
                   w_RFM_6_var12 + w_RFM_6_var20 + w_RFM_6_var21 + w_RFM_h_var2_Derived + 
                   w_applyTimeSegment + w_childrenNum + w_rsk_score + w_card_tp + 
                   w_currentJobyear + w_loansCalls1 + w_localFriends + w_tongdunMultiLoanNum, data=binTrain, family=binomial())

binTrain$score <- predict(mBinTD, type='response', binTrain)
binTest$score <- predict(mBinTD, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


summary(mBinTD)

#####################################################
m1 <- glm(flgDPD ~w_RFM_12_var55 + w_RFM_1_var4 + 
                w_RFM_6_var12 + w_RFM_6_var20 + w_RFM_6_var21 + w_RFM_h_var2_Derived + 
                w_applyTimeSegment + w_childrenNum + w_rsk_score + w_card_tp + 
                w_localFriends + w_tongdunMultiLoanNum, data=binTrain, family=binomial())

binTrain$score <- predict(m1, type='response', binTrain)
binTest$score <- predict(m1, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)

summary(m1)

#####################################################
m2 <- glm(flgDPD ~w_RFM_12_var55 + w_RFM_1_var4 + 
            w_RFM_6_var12 + w_RFM_6_var21 + w_RFM_h_var2_Derived + 
            w_applyTimeSegment + w_childrenNum + w_card_tp + w_tongdunMultiLoanNum, data=binTrain, family=binomial())

binTrain$score <- predict(m2, type='response', binTrain)
binTest$score <- predict(m2, type='response', binTest)

ks_stat(binTrain$flgDPD, binTrain$score)
ks_stat(binTest$flgDPD, binTest$score)


m2validate <- glm(flgDPD ~w_RFM_12_var55 + w_RFM_1_var4 + 
                    w_RFM_6_var12 + w_RFM_6_var21 + w_RFM_h_var2_Derived + 
                    w_applyTimeSegment + w_childrenNum + w_card_tp + w_tongdunMultiLoanNum, data=binTest, family=binomial())

summary(m2validate)






#####################################################
# not run
# endproduct:
mBinFinal


