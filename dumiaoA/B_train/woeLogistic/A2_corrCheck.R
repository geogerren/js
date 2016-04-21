################################################################################################################
keepVarRaw <- unique(assigningDF$varName)
corrCheck <- train[, keepVarRaw, with=F]
corrCheck[, c("longTimeShutdown", "marry", "sex", "card_tp", "localFriends", "tongdunMultiLoanNum"):=NULL]
corMatrix1 <- cor(corrCheck, method = "spearman")

write.csv(corMatrix1, paste0(boxdata, "corMatrix1.csv"))


############################################################
corrRemoveList <- c("RFM_12_var2","RFM_6_var15","useCardPM","consumeFreg","useCardSumRank","RFM_1_var14","zhiceCar","useCardAmountAvg")

# 仅保留不在remove里的且出现在assigningDF里的
keepVarRaw <- keepVarRaw[!(keepVarRaw %in% corrRemoveList)]
keepVar <- c(paste0("w_", keepVarRaw), "flgDPD")



rawTrain<-train[, c(keepVarRaw,"flgDPD"), with=F]
binTrain<-train[, keepVar, with=F]


write.csv(rawTrain, paste0(boxdata, "rawTrain.csv"))
write.csv(binTrain, paste0(boxdata, "binTrain.csv"))




#####################################################
# not run
# endproduct:
keepVar
rawTrain
binTrain

