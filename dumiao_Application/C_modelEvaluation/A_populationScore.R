# input
assigningDF
mfull

eval <- copy(featuresWideU)

scoredTrain<-scoreAssignAuto(rawTrain[V2 %in% c(1,2,3,4),], scoreCard, intercept = 508)
scoredTest <-scoreAssignAuto(rawTrain[V2 == 5,], scoreCard, intercept = 508)
