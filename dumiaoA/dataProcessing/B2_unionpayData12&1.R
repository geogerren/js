applyfinancing<-ruleq("select apply_id, financingprojectid from project_detail")
financingccno<-read.csv(paste0(boxdata, "对照表.csv"))
ccdata<-read.csv(paste0(boxdata, "放款人银行卡信息 __数据返还结果.csv"), stringsAsFactors = F)

zhiceBack<-merge(ccdata, financingccno, by.x="cardNo", by.y="cardNo")
zhiceBack<-merge(zhiceBack, applyfinancing, by.x="applyid", by.y="apply_id")

zhiceBack<-as.data.table(zhiceBack)
zhiceBack[, c("applyid", "cardNo", "stamp"):=NULL]

setnames(zhiceBack, "RFM_12_var56.", "RFM_12_var56")

zhiceBack[, RFM_h_var2:=paste0(substr(RFM_h_var2, 1, 4), "-", substr(RFM_h_var2, 5, 6), "-", substr(RFM_h_var2, 7, 8))]

zhiceBack[RFM_h_var2=="NA-NA-NA", RFM_h_var2:=NA]

zhiceBack[, RFM_h_var2:=as.POSIXct(RFM_h_var2)]
# unionPayRebuilt[, RFM_h_var2:=as.character(RFM_h_var2)]
# zhiceBack[, RFM_h_var2:=as.character(RFM_h_var2)]

##################################Dec and Jan Unionpay + rebuilt Unionpay before Nov

unionpayAgg<-rbind(unionPayRebuilt, zhiceBack)


