swapSample<-rbind(scoredTest, scoredTrain, fill=T)

swapSample<-scoreAssignAuto(rawTrain, scoreCard, intercept=508)

swapSample<-merge(swapSample, EverDPD90, by.x="financingprojectid", by.y="project_id")
nrow(scoredOOS)

names(swapSample)
min(swapSample$s_totalScore)
max(swapSample$s_totalScore)

cutoffAnalysis<-data.frame()
for(i in 9:79){
  cutoff<-i*10
  sampleIn <- swapSample[s_totalScore >= cutoff,]
  # scoredOOS[, approved:=ifelse(s_totalScore>=cutoff, 1, 0)]
  badRate <- sum(sampleIn$flgDPD)/nrow(sampleIn)
  # approvalRate <- sum(scoredOOS$approved)/nrow(scoredOOS)
  chargeOffRate <- sum(sampleIn$DPD90Sum)/nrow(sampleIn)
  badCount <- sum(sampleIn$flgDPD)
  totalCount <- nrow(sampleIn)
  cutoffAnalysis<-rbind(cutoffAnalysis, c(cutoff, badRate, 
                                          # approvalRate, 
                                          chargeOffRate, badCount, totalCount))
}

names(cutoffAnalysis)<-c("cutoff","badRate","chargeOffRate","badCount","totalCount")
write.csv(cutoffAnalysis, paste0(boxdata, "cutoff.csv"))
