allDataBin <- copy(binTrain)
allDataRaw <- copy(rawTrain)

# bglm<-bestglm(allDataBin, family=binomial, IC="CV")  

for(i in 1:nrow(allDataBin)){
  allDataBin[i, bucket:=ceiling(runif(1, 0, 5))]
}

# without银联数据
# allDataBin[, c("w_RFM_12_var55","w_RFM_6_var12","w_amuseConsumeFreq","w_cardType","w_consumeLineRate",
#                "w_creditCashFreq","w_creditWD3Months","w_goOut120","w_lastMonthOverdrawNum","w_multiBorrowNumP6",
#                "w_postMonthConsumeFreg","w_useCardLastTime","w_useCardNumPost6","w_zhiceHouse"):=NULL]
###############################################################################
#  binned full model 1
train1 <- allDataBin[bucket %in% c(1,2,3,4),]
test1 <- allDataBin[bucket == 5,]

m1full<-glm(flgDPD~., data=train1, family=binomial())
m1 <- step(m1full, direction = "both", trace=10)

summary(m1)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.90524    0.09991 -19.070  < 2e-16 ***
#   w_RFM_6_var12       0.90463    0.27967   3.235 0.001218 ** 
#   w_age               0.83926    0.42776   1.962 0.049764 *  
#   w_applyTimeSegment  0.92698    0.42371   2.188 0.028685 *  
#   w_avgMonthCall      0.86721    0.56445   1.536 0.124447    
# w_creditCashFreq    1.97516    0.93662   2.109 0.034961 *  
#   w_localFriends      1.33281    0.50787   2.624 0.008683 ** 
#   w_longTimeShutdown  1.57157    0.43277   3.631 0.000282 ***
#   w_multiBorrowNumP6  1.34042    0.40027   3.349 0.000812 ***
#   w_zhiceHouse        1.08441    0.27711   3.913 9.11e-05 ***
#   w_zhimaScore        0.97866    0.28798   3.398 0.000678 ***


m1validate <- glm(formula = flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                    w_avgMonthCall + w_creditCashFreq + w_localFriends + w_longTimeShutdown + 
                    w_multiBorrowNumP6 + w_zhiceHouse + w_zhimaScore, data=test1, family=binomial())

# m1validate <- glm(formula = flgDPD ~ w_age + w_applyTimeSegment + w_childrenNum + 
#                     w_localFriends + w_longTimeShutdown + w_zhimaScore, family = binomial(), 
#                   data = train1)


summary(m1validate)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -2.003367   0.230969  -8.674   <2e-16 ***
#   w_RFM_6_var12       2.095500   0.854199   2.453   0.0142 *
#   w_age               2.239381   1.005887   2.226   0.0260 *
#   w_applyTimeSegment  2.470686   0.968374   2.551   0.0107 *
#   w_avgMonthCall      2.578264   1.249953   2.063   0.0391 *
#   w_creditCashFreq   -2.084365   2.151381  -0.969   0.3326
# w_localFriends     -0.004944   0.966219  -0.005   0.9959
# w_longTimeShutdown  0.084913   1.303815   0.065   0.9481
# w_multiBorrowNumP6  0.507809   0.916001   0.554   0.5793
# w_zhiceHouse        1.512835   0.670057   2.258   0.0240 *
#   w_zhimaScore        1.555039   0.658372   2.362   0.0182 *


train1$score <- predict(m1, type='response', train1)
test1$score <- predict(m1, type='response', test1)

ks_stat(train1$flgDPD, train1$score)
ks_stat(test1$flgDPD, test1$score)

###############################################################################
#  binned full model 2
train2 <- allDataBin[bucket %in% c(1,2,3,5),]
test2 <- allDataBin[bucket == 4,]

m2full<-glm(flgDPD~., data=train2, family=binomial())
m2 <- step(m2full, direction = "both", trace=0)

summary(m2)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -1.8899     0.1037 -18.225  < 2e-16 ***
#   w_RFM_6_var12            1.1304     0.3153   3.585 0.000337 ***
#   w_age                    0.9309     0.4943   1.883 0.059687 .  
# w_applyTimeSegment       1.2419     0.4528   2.743 0.006089 ** 
#   w_avgMonthCall           1.1966     0.5878   2.036 0.041780 *  
#   w_childrenNum            1.1492     0.5288   2.173 0.029777 *  
#   w_lastMonthOverdrawNum   1.6653     0.6831   2.438 0.014769 *  
#   w_localFriends           0.7305     0.4852   1.506 0.132138    
# w_longTimeShutdown       1.5910     0.4615   3.448 0.000565 ***
#   w_multiBorrowNumP6       1.5317     0.4060   3.772 0.000162 ***
#   w_zhiceHouse             1.0984     0.2958   3.713 0.000204 ***
#   w_zhimaScore             1.2742     0.2990   4.262 2.03e-05 ***


m2validate <- glm(formula = flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                    w_avgMonthCall + w_childrenNum + w_lastMonthOverdrawNum + 
                    w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
                    w_zhiceHouse + w_zhimaScore, data=test2, family=binomial())

m2validate <- glm(formula = flgDPD ~ w_age + w_applyTimeSegment + w_avgMonthCall + 
                    w_localFriends + w_longTimeShutdown + w_zhimaScore + bucket, 
                  family = binomial(), data = train2)

summary(m2validate)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.10441    0.22891  -9.193   <2e-16 ***
#   w_RFM_6_var12           0.62196    0.50569   1.230   0.2187    
# w_age                   0.38195    0.94195   0.405   0.6851    
# w_applyTimeSegment      1.07226    0.75864   1.413   0.1575    
# w_avgMonthCall          0.54116    1.07756   0.502   0.6155    
# w_childrenNum          -1.74305    1.22431  -1.424   0.1545    
# w_lastMonthOverdrawNum -0.44834    1.56897  -0.286   0.7751    
# w_localFriends          2.57041    1.28394   2.002   0.0453 *  
#   w_longTimeShutdown      0.11671    0.99301   0.118   0.9064    
# w_multiBorrowNumP6     -0.01321    1.02080  -0.013   0.9897    
# w_zhiceHouse            1.30501    0.52608   2.481   0.0131 *  
#   w_zhimaScore            0.32597    0.57217   0.570   0.5689    


train2$score <- predict(m2, type='response', train2)
test2$score <- predict(m2, type='response', test2)

ks_stat(train2$flgDPD, train2$score)
ks_stat(test2$flgDPD, test2$score)

###############################################################################
#  binned full model 3
train3 <- allDataBin[bucket %in% c(1,2,4,5),]
test3 <- allDataBin[bucket == 3,]
train3[, bucket:=NULL]

m3full<-glm(flgDPD~., data=train3, family=binomial())
m3 <- step(m3full, direction = "both", trace=10)

summary(m3)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.8367     0.1252 -14.675  < 2e-16 ***
#   w_RFM_12_var55       1.5807     1.0026   1.577 0.114866    
# w_RFM_6_var12        0.9813     0.2842   3.452 0.000556 ***
#   w_age                1.0616     0.4377   2.425 0.015300 *  
#   w_applyTimeSegment   1.1853     0.4127   2.872 0.004079 ** 
#   w_avgMonthCall       1.0847     0.5679   1.910 0.056132 .  
# w_localFriends       1.0840     0.5016   2.161 0.030680 *  
#   w_longTimeShutdown   0.7384     0.4832   1.528 0.126450    
# w_multiBorrowNumP6   1.1158     0.4241   2.631 0.008512 ** 
#   w_useCardLastTime    2.0695     0.9798   2.112 0.034682 *  
#   w_zhiceHouse         0.9309     0.2904   3.205 0.001350 ** 
#   w_zhimaScore         1.0262     0.2922   3.512 0.000445 ***

m3validate <- glm(formula = flgDPD ~ w_RFM_12_var55 + w_RFM_6_var12 + w_age + 
                    w_applyTimeSegment + w_avgMonthCall + w_localFriends + w_longTimeShutdown + 
                    w_multiBorrowNumP6 + w_useCardLastTime + w_zhiceHouse + w_zhimaScore, data=test3, family=binomial())

m3validate <- glm(formula = flgDPD ~ w_age + w_applyTimeSegment + w_loansCalls3 + 
                    w_localFriends + w_longTimeShutdown + w_zhimaScore + bucket, 
                  family = binomial(), data = train3)
summary(m3validate)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.8390     0.2579  -7.130 1.01e-12 ***
#   w_RFM_12_var55       1.0518     2.1025   0.500  0.61691    
# w_RFM_6_var12        1.7418     0.7704   2.261  0.02377 *  
#   w_age                1.3488     0.9143   1.475  0.14015    
# w_applyTimeSegment   0.5143     1.1133   0.462  0.64411    
# w_avgMonthCall       0.9398     1.1889   0.791  0.42922    
# w_localFriends       1.4112     1.0321   1.367  0.17153    
# w_longTimeShutdown   3.4172     0.8741   3.910 9.25e-05 ***
#   w_multiBorrowNumP6   1.7406     0.8469   2.055  0.03984 *  
#   w_useCardLastTime   -2.9794     1.5674  -1.901  0.05733 .  
# w_zhiceHouse         1.6539     0.5737   2.883  0.00394 ** 
#   w_zhimaScore         1.5119     0.6324   2.391  0.01681 *   


train3$score <- predict(m3, type='response', train3)
test3$score <- predict(m3, type='response', test3)

ks_stat(train3$flgDPD, train3$score)
ks_stat(test3$flgDPD, test3$score)


###############################################################################
#  binned full model 4
train4 <- allDataBin[bucket %in% c(1,3,4,5),]
test4 <- allDataBin[bucket == 2,]

train4[, bucket:=NULL]
m4full<-glm(flgDPD~., data=train4, family=binomial())
m4 <- step(m4full, direction = "both", trace=0)

summary(m4)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -1.7719     0.1011 -17.520  < 2e-16 ***
#   w_RFM_6_var12            1.1092     0.3027   3.665 0.000248 ***
#   w_age                    1.2235     0.4293   2.850 0.004370 ** 
#   w_applyTimeSegment       1.1295     0.4261   2.651 0.008033 ** 
#   w_avgMonthCall           1.1947     0.5625   2.124 0.033677 *  
#   w_lastMonthOverdrawNum   1.1950     0.6619   1.805 0.071029 .  
# w_loansCalls3            2.5805     1.3470   1.916 0.055391 .  
# w_localFriends           1.0716     0.4749   2.257 0.024038 *  
#   w_longTimeShutdown       1.5849     0.4464   3.550 0.000385 ***
#   w_multiBorrowNumP6       0.9090     0.4236   2.146 0.031892 *  
#   w_useCardNumPost6        1.5265     0.8843   1.726 0.084309 .  
# w_zhiceHouse             1.2479     0.2784   4.482  7.4e-06 ***
#   w_zhimaScore             0.9798     0.2887   3.393 0.000690 ***

m4validate <- glm(formula = flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                    w_avgMonthCall + w_lastMonthOverdrawNum + w_loansCalls3 + 
                    w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
                    w_useCardNumPost6 + w_zhiceHouse + w_zhimaScore, data=test4, family=binomial())

m4validate <- glm(formula = flgDPD ~ w_age + w_applyTimeSegment + w_avgMonthCall + 
                    w_localFriends + w_longTimeShutdown + w_zhimaScore, family = binomial(), 
                  data = train4)
summary(m4validate)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -2.2593     0.2782  -8.122 4.57e-16 ***
#   w_RFM_6_var12            0.7081     0.5802   1.221  0.22224    
# w_age                    0.1338     0.9700   0.138  0.89031    
# w_applyTimeSegment       1.0518     0.9319   1.129  0.25908    
# w_avgMonthCall           0.1828     1.3136   0.139  0.88932    
# w_lastMonthOverdrawNum  -0.3531     1.9686  -0.179  0.85763    
# w_loansCalls3           -0.7611     2.4966  -0.305  0.76048    
# w_localFriends           1.4722     1.3826   1.065  0.28695    
# w_longTimeShutdown       0.9111     1.0780   0.845  0.39804    
# w_multiBorrowNumP6       2.1520     0.8078   2.664  0.00772 ** 
#   w_useCardNumPost6       -0.4142     2.1060  -0.197  0.84407    
# w_zhiceHouse             0.7802     0.7332   1.064  0.28730    
# w_zhimaScore             1.5725     0.6668   2.358  0.01836 *  


train4$score <- predict(m4, type='response', train4)
test4$score <- predict(m4, type='response', test4)

ks_stat(train4$flgDPD, train4$score)
ks_stat(test4$flgDPD, test4$score)



###############################################################################
#  binned full model 5
train5 <- allDataBin[bucket %in% c(5,2,3,4),]
test5 <- allDataBin[bucket == 1,]

train5[, bucket:=NULL]
m5full<-glm(flgDPD~., data=train5, family=binomial())
m5 <- step(m5full, direction = "both", trace=10)


summary(m5)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.7424     0.1278 -13.632  < 2e-16 ***
#   w_RFM_12_var55       1.8475     1.0037   1.841 0.065662 .  
# w_RFM_6_var12        1.1243     0.3098   3.630 0.000284 ***
#   w_applyTimeSegment   0.9847     0.4362   2.257 0.023981 *  
#   w_avgMonthCall       0.8550     0.5787   1.478 0.139518    
# w_childrenNum        0.9071     0.4817   1.883 0.059696 .  
# w_consumeLineRate    1.5475     0.8588   1.802 0.071572 .  
# w_loansCalls3        2.8118     1.3709   2.051 0.040267 *  
#   w_localFriends       1.1203     0.5106   2.194 0.028245 *  
#   w_longTimeShutdown   1.5211     0.4742   3.208 0.001337 ** 
#   w_multiBorrowNumP6   1.1060     0.4240   2.608 0.009100 ** 
#   w_useCardNumPost6    1.5252     0.9149   1.667 0.095499 .  
# w_zhiceHouse         1.3667     0.2953   4.628 3.69e-06 ***
#   w_zhimaScore         0.9702     0.2996   3.238 0.001203 ** 

m5validate <- glm(formula = flgDPD ~ w_RFM_12_var55 + w_RFM_6_var12 + w_applyTimeSegment + 
                    w_avgMonthCall + w_childrenNum + w_consumeLineRate + w_loansCalls3 + 
                    w_localFriends + w_longTimeShutdown + w_multiBorrowNumP6 + 
                    w_useCardNumPost6 + w_zhiceHouse + w_zhimaScore, data=test5, family=binomial())

m5validate <- glm(formula = flgDPD ~ w_age + w_applyTimeSegment + w_avgMonthCall + 
                    w_localFriends + w_longTimeShutdown + w_zhimaScore, family = binomial(), 
                  data = train5)
summary(m5validate)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.78141    0.25698  -6.932 4.14e-12 ***
#   w_RFM_12_var55     -0.22199    2.09913  -0.106   0.9158    
# w_RFM_6_var12       0.83825    0.54693   1.533   0.1254    
# w_applyTimeSegment  1.15676    0.87155   1.327   0.1844    
# w_avgMonthCall      0.98416    1.15134   0.855   0.3927    
# w_childrenNum       1.08219    0.88032   1.229   0.2190    
# w_consumeLineRate  -0.02517    1.48057  -0.017   0.9864    
# w_loansCalls3      -0.47545    2.41163  -0.197   0.8437    
# w_localFriends      0.85349    0.89450   0.954   0.3400    
# w_longTimeShutdown  1.20097    0.86719   1.385   0.1661    
# w_multiBorrowNumP6  1.29269    0.84399   1.532   0.1256    
# w_useCardNumPost6  -0.17353    1.77993  -0.097   0.9223    
# w_zhiceHouse        0.77552    0.56603   1.370   0.1707    
# w_zhimaScore        1.13930    0.56177   2.028   0.0426 *  




train5$score <- predict(m5, type='response', train5)
test5$score <- predict(m5, type='response', test5)

ks_stat(train5$flgDPD, train5$score)
ks_stat(test5$flgDPD, test5$score)



















########################################################
# remove w_useCardNumPost6, pvalue>0.15

M1 <- glm(formula = flgDPD ~ w_RFM_6_var12 + w_age + w_applyTimeSegment + 
                w_avgMonthCall + w_localFriends + w_longTimeShutdown + 
                w_multiBorrowNumP6 + w_zhiceHouse + w_zhimaScore, data=allDataBin, family=binomial())

# M1 <- glm(formula = flgDPD ~ w_age + w_applyTimeSegment + w_avgMonthCall + 
#       w_localFriends + w_longTimeShutdown + w_zhimaScore, family = binomial(), 
#     data = allDataBin)

allDataBin$score <- predict(M1, type='response', allDataBin)

ks_stat(allDataBin$flgDPD, allDataBin$score)

summary(M1)

#####################################################

auc(as.numeric(as.character(allDataBin$flgDPD)), allDataBin$score)

#####################################################

# remove w_useCardNumPost6, pvalue>0.15

M2 <- glm(formula = flgDPD ~ w_applyTimeSegment + 
            w_avgMonthCall + w_longTimeShutdown + 
            w_multiBorrowNumP6 + w_zhimaScore, data=allDataBin, family=binomial())

allDataBin$score <- predict(M2, type='response', allDataBin)

ks_stat(allDataBin$flgDPD, allDataBin$score)

summary(M2)











########################################################
mBinFinal<-M1
# not run
# endproduct:
mBinFinal


