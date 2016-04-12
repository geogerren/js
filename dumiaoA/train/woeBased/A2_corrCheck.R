################################################################################################################
keepVarRaw <- unique(assigningDF$varName)
corrCheck <- train[, keepVarRaw, with=F]
corrCheck[, c("longTimeShutdown", "marry", "sex", "card_tp", "localFriends"):=NULL]
corMatrix1 <- cor(corrCheck, method = "spearman")

write.csv(corMatrix1, paste0(boxdata, "corMatrix1.csv"))

# variable set 2
# remove 

# FLAG_6_var11 近6个月'月累计消费超过3000元的商户消费金额占月总消费金额比例>= 50%'行为出现月份数,	w_RFM_1_var14 近1个月借贷笔数
# 理由:0.842324613,保留FLAG_6_var12,更强的异常行为区分度更高


# 近6个月交易笔数.RFM_6_var2
# 与RFM_12_var2 (0.890433645)和RFM_1_var1 (0.665265872)均有相关


corrRemoveList <- c("FLAG_6_var11","RFM_6_var2")
