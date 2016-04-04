source("E:/Seafiles/Jimu/Code/js/sourceFile.R")

# options(warn=-1)
featuresWideU<-merge(featuresWide, unionpayAgg, by="financingprojectid", all.x=T)

featuresWideU[, FLAG_12_var1:=ifelse(!is.na(FLAG_12_var1), FLAG_12_var1, ifelse(!is.na(zhiceCar), zhiceCar, declareCar))]
featuresWideU[, card_tp:=ifelse(!is.na(card_tp), card_tp, cardType)]
featuresWideU[, RFM_1_var4:=ifelse(!is.na(RFM_1_var4), RFM_1_var4, lastMonthOverdrawNum)]
featuresWideU[, RFM_12_var58:=ifelse(!is.na(RFM_12_var58), RFM_12_var58, post12PMFeeNum)]
featuresWideU[, RFM_12_var59:=ifelse(!is.na(RFM_12_var59), RFM_12_var59, post12PMFeeAmount)]


featuresWideU[, c("zhiceCar", "declareCar", "cardType", "lastMonthOverdrawNum", "post12PMFeeNum", "post12PMFeeAmount"):=NULL]
