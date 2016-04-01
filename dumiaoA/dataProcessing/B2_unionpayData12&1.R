applyfinancing<-ruleq("select apply_id, financingprojectid from project_detail")
financingccno<-read.csv(paste0(boxdata, "对照表.csv"))
ccdata<-read.csv(paste0(boxdata, "放款人银行卡信息 __数据返还结果.csv"))

zhiceBack<-merge(ccdata, financingccno, by.x="cardNo", by.y="cardNo")
zhiceBack<-merge(zhiceBack, applyfinancing, by.x="applyid", by.y="apply_id")

zhiceBack<-as.data.table(zhiceBack)
zhiceBack[, c("applyid", "cardNo", "stamp"):=NULL]

setnames(zhiceBack, "RFM_12_var56.", "RFM_12_var56")





##################################Dec and Jan Unionpay + rebuilt Unionpay before Nov
unionpayAgg<-rbind(unionPayRebuilt, zhiceBack)
