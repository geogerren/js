
scoreDF <- scoreCalc(assigningDF, mBinFinal, p=50, o=0.2, b=500)

scored<-scoreAssignAuto(rawTrain, scoreDF)

max(scored$s_totalScore)
min(scored$s_totalScore)


write.csv(scoreDF, paste0(boxdata, "scoreDF.csv"))


