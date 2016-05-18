ggImpute(oos, fullImpute = F, removeMassiveMissing = F)
featureAnalysis(oos, exclude = "service_id")


scoreCard<-read.csv(paste0(boxdata, "scoreCard.csv"))
scoreCard<-data.table(scoreCard)
oos<-data.table(oos)

scoreCard[, X:=NULL]

scoredOOS<-scoreAssignAuto(oos, scoreCard, intercept=508)


scoredOOS[, scoreBand:=ifelse(s_totalScore<466, 1, 
                               ifelse(s_totalScore<523, 2, 
                                      ifelse(s_totalScore<583, 3, 
                                             ifelse(s_totalScore<637, 4, 5))))]


View(scoredOOS[, .("cnt"=.N),by=c("scoreBand")])

write.csv(scoredOOS, paste0(boxdata, "scoredOOS.csv"))
