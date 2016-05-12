
scoreCard <- scoreCalc(assigningDF, mBinFinal, neutralForMissing = T, p=100, o=0.2, b=0)$scoreDF

write.csv(scoreCard, paste0(boxdata, "scoreCard.csv"))

rawTrain<-cbind(rawTrain, allDataBin$bucket)
is.data.table(rawTrain)
scoredTrain<-scoreAssignAuto(rawTrain[V2 %in% c(1,2,3,4),], scoreCard, intercept = 508)
scoredTest <-scoreAssignAuto(rawTrain[V2 == 5,], scoreCard, intercept = 508)

trainScoreBand <- banding(scoredTrain, "s_totalScore", "scoreBand")


scoredTest[, scoreBand:=ifelse(s_totalScore<422, 1, 
                               ifelse(s_totalScore<466, 2, 
                                      ifelse(s_totalScore<501, 3, 
                                             ifelse(s_totalScore<523, 4, 
                                                    ifelse(s_totalScore<552, 5, 
                                                           ifelse(s_totalScore<583, 6, 
                                                                  ifelse(s_totalScore<604, 7, 
                                                                         ifelse(s_totalScore<637, 8, 
                                                                                ifelse(s_totalScore<678, 9, 10)))))))))]
testScoreBand <- scoredTest[, .("TotalCust"=.N, 
                                "DPDCust"=sum(as.numeric(as.character(flgDPD)))), 
                            by="scoreBand"]
testScoreBand[, DPDRate:=DPDCust/TotalCust]
#####################################################
# not run
# endproduct:
# trainScoreBand
scoreCard
