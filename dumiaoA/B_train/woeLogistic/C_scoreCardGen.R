
scoreCard <- scoreCalc(assigningDF, mBinFinal, neutralForMissing = T, p=100, o=0.2, b=0)$scoreDF

write.csv(scoreCard, paste0(boxdata, "scoreCard.csv"))

rawTrain<-cbind(rawTrain, allDataBin$bucket)
is.data.table(rawTrain)
scoredTrain<-scoreAssignAuto(rawTrain[V2 %in% c(1,2,3,4),], scoreCard, intercept = 508)
scoredTest <-scoreAssignAuto(rawTrain[V2 == 5,], scoreCard, intercept = 508)

trainScoreBand <- banding(scoredTrain, "s_totalScore", "scoreBand", bands = seq(0,1,0.2))

scoredTest[, scoreBand:=ifelse(s_totalScore<466, 1, 
                               ifelse(s_totalScore<523, 2, 
                                      ifelse(s_totalScore<583, 3, 
                                             ifelse(s_totalScore<637, 4, 5))))]
testScoreBand <- scoredTest[, .("TotalCust"=.N, 
                                "DPDCust"=sum(as.numeric(as.character(flgDPD)))), 
                            by="scoreBand"]
testScoreBand[, DPDRate:=DPDCust/TotalCust]
#####################################################
# not run
# endproduct:
# trainScoreBand
scoreCard
