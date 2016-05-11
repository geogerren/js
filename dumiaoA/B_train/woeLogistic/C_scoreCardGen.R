
scoreCard <- scoreCalc(assigningDF, mBinFinal, neutralForMissing = T, p=100, o=0.2, b=0)$scoreDF

write.csv(scoreCard, paste0(boxdata, "scoreCard.csv"))

scoredTrain<-scoreAssignAuto(rawTrain, scoreCard, intercept = 518)
scoredTest <-scoreAssignAuto(rawTest, scoreCard, intercept = 507)

trainScoreBand <- banding(scoredTrain, "s_totalScore", "scoreBand")


scoredTest[, scoreBand:=ifelse(s_totalScore<474, 1, 
                               ifelse(s_totalScore<540, 2, 
                                      ifelse(s_totalScore<584.0651, 3, 
                                             ifelse(s_totalScore<631.4696, 4, 
                                                    ifelse(s_totalScore<667.7191, 5, 
                                                           ifelse(s_totalScore<704.3027, 6, 
                                                                  ifelse(s_totalScore<749.1360, 7, 
                                                                         ifelse(s_totalScore<795.0505, 8, 
                                                                                ifelse(s_totalScore<875.0173, 9, 10)))))))))]
testScoreBand <- scoredTest[, .("TotalCust"=.N, 
                                "DPDCust"=sum(as.numeric(as.character(flgDPD)))), 
                            by="scoreBand"]
testScoreBand[, DPDRate:=DPDCust/TotalCust]
#####################################################
# not run
# endproduct:
# trainScoreBand
scoreCard
