
scoreCard <- scoreCalc(assigningDF, mBinFinal, neutralForMissing = T, p=100, o=0.2, b=500)$scoreDF

write.csv(scoreCard, paste0(boxdata, "scoreCard.csv"))

scoredTrain<-scoreAssignAuto(rawTrain, scoreCard, intercept = 521)
scoredTest <-scoreAssignAuto(rawTest, scoreCard, intercept = 521)

trainScoreBand <- banding(scoredTrain, "s_totalScore", "scoreBand")


scoredTest[, scoreBand:=ifelse(s_totalScore<394, 1, 
                               ifelse(s_totalScore<423, 2, 
                                      ifelse(s_totalScore<438, 3, 
                                             ifelse(s_totalScore<470, 4, 
                                                    ifelse(s_totalScore<505, 5, 
                                                           ifelse(s_totalScore<512, 6, 
                                                                  ifelse(s_totalScore<534, 7, 
                                                                         ifelse(s_totalScore<567, 8, 
                                                                                ifelse(s_totalScore<601, 9, 10)))))))))]
testScoreBand <- scoredTest[, .("TotalCust"=.N, 
                                "DPDCust"=sum(as.numeric(as.character(flgDPD)))), 
                            by="scoreBand"]
testScoreBand[, DPDRate:=DPDCust/TotalCust]
#####################################################
# not run
# endproduct:
# trainScoreBand
scoreCard
