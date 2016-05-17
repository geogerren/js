total<-rbind(scoredTrain, scoredTest)
compare<-total[, c("s_totalScore","zhimaScore","modelScore","flgDPD"), with=F]


newScore <- banding(compare, "s_totalScore", "scoreBand", bands = seq(0,1,0.2))

zhima <- banding(compare, "zhimaScore", "zhimaBand", bands = seq(0,1,0.2))

modelOld <- banding(compare, "modelScore", "modelBand", bands = seq(0,1,0.2))

compare[, finalScore:=modelScore+1/3*zhimaScore]
finalOld <- banding(compare, "finalScore", "finalBand", bands = seq(0,1,0.2))
