
scoreCard <- scoreCalc(assigningDF, mBinFinal, neutralForMissing = T, p=100, o=0.2, b=500)$scoreDF

write.csv(scoreCard, paste0(boxdata, "scoreCard.csv"))




#####################################################
# not run
# endproduct:
# trainScoreBand
scoreCard
