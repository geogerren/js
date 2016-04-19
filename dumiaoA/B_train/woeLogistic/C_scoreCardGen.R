
scoreCard <- scoreCalc(assigningDF, mBinFinal, neutralForMissing = T, p=100, o=0.2, b=0)$scoreDF

write.csv(scoreCard, paste0(boxdata, "scoreCard.csv"))

scoredTrain<-scoreAssignAuto(rawTrain, scoreCard, intercept = 443)
scoredTest <-scoreAssignAuto(rawTest, scoreCard, intercept = 443)

trainScoreBand <- banding(scoredTrain, "s_totalScore", "scoreBand")


scoredTest[, scoreBand:=ifelse(s_totalScore<362, 1, 
                               ifelse(s_totalScore<394, 2, 
                                      ifelse(s_totalScore<423, 3, 
                                             ifelse(s_totalScore<451, 4, 
                                                    ifelse(s_totalScore<458, 5, 
                                                           ifelse(s_totalScore<492, 6, 
                                                                  ifelse(s_totalScore<525, 7, 
                                                                         ifelse(s_totalScore<540, 8, 
                                                                                ifelse(s_totalScore<569, 9, 10)))))))))]
testScoreBand <- scoredTest[, .("TotalCust"=.N, 
                                "DPDCust"=sum(as.numeric(as.character(flgDPD)))), 
                            by="scoreBand"]
testScoreBand[, DPDRate:=DPDCust/TotalCust]
#####################################################
# not run
# endproduct:
# trainScoreBand
scoreCard
