################################################################################################################
keepVarRaw <- unique(assigningDF$varName)
corrCheck <- train[, keepVarRaw, with=F]
corrCheck[, c("longTimeShutdown", "marry", "sex", "card_tp", "localFriends", "tongdunMultiLoanNum"):=NULL]
corMatrix1 <- cor(corrCheck, method = "spearman")

write.csv(corMatrix1, paste0(boxdata, "corMatrix1.csv"))

# variable set 2
# remove 

# FLAG_6_var11 近6个月'月累计消费超过3000元的商户消费金额占月总消费金额比例>= 50%'行为出现月份数,	w_RFM_1_var14 近1个月借贷笔数
# 理由:0.842324613,保留FLAG_6_var12,更强的异常行为区分度更高


# 近6个月交易笔数.RFM_6_var2
# 与RFM_12_var2 (0.890433645)和RFM_1_var1 (0.665265872)均有相关


corrRemoveList <- c("RFM_12_var2","RFM_6_var15","useCardPM")

# 仅保留不在remove里的且出现在assigningDF里的
keepVarRaw <- keepVarRaw[!(keepVarRaw %in% corrRemoveList)]
corrCheck <- train[, keepVarRaw, with=F]
corrCheck[, c("longTimeShutdown", "marry", "sex", "card_tp", "localFriends", "tongdunMultiLoanNum"):=NULL]
corMatrix2 <- cor(corrCheck, method = "spearman")

write.csv(corMatrix2, paste0(boxdata, "corMatrix2.csv"))


corrRemoveList <- c("RFM_1_var14","consumeFreg","zhiceCar")


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

