# source("E:/Seafiles/Jimu/Code/js/sourceFile.R")

# options(warn=-1)
featuresWideU<-merge(featuresWide, unionpayAgg, by="financingprojectid", all.x=T)

featuresWideU[, FLAG_12_var1:=ifelse(!is.na(FLAG_12_var1), FLAG_12_var1, ifelse(!is.na(zhiceCar), zhiceCar, declareCar))]
featuresWideU[, card_tp:=ifelse(!is.na(card_tp), card_tp, cardType)]
featuresWideU[, RFM_1_var4:=ifelse(!is.na(RFM_1_var4), RFM_1_var4, lastMonthOverdrawNum)]
featuresWideU[, RFM_12_var58:=ifelse(!is.na(RFM_12_var58), RFM_12_var58, post12PMFeeNum)]
featuresWideU[, RFM_12_var59:=ifelse(!is.na(RFM_12_var59), RFM_12_var59, post12PMFeeAmount)]


featuresWideU[, c("zhiceCar", "declareCar", "cardType", "lastMonthOverdrawNum", "post12PMFeeNum", "post12PMFeeAmount"):=NULL]



featuresWideU[, RFM_h_var2_Derived:=difftime(as.POSIXct(createtime), RFM_h_var2, units = "days")] 

featuresWideU[, RFM_h_var2_Derived:=ifelse(RFM_h_var2_Derived<0, 0, RFM_h_var2_Derived)]

featuresWideU[, RFM_h_var2:=NULL]



############################################################################################
# tdMulti<-read.csv(paste0(boxdata, "mytable.csv"))

tdMulti<-ruleq("select p.financingprojectid
               ,var_val as tongdunMultiLoanNum
               from t_variable_dict t
               join project_detail p
               on t.applyid = p.apply_id
               where t.var_code='tongdunMultiLoanNum'
               ")

# 补全同盾缺失
# tongdun<-ruleq("select pd.financingprojectid
#                ,DATE(pd.createtime) as createtime
#                ,td.rule_name
#                ,td.final_score
#                from t_pat_tongdun_blank td
#                join project_detail pd
#                on td.service_id = pd.service_id
#                ")
# tongdun[rule_name=='3个月内身份证在多个平台进行借款', rule_name:='tongdunIdMultiLoanNum']
# tongdun[rule_name=='3个月内手机在多个平台进行借款', rule_name:='tongdunPhoneMultiLoanNum']
# tongdun[rule_name=='3个月内银行卡在多个平台进行借款', rule_name:='tongdunBankCardMultiLoanNum']
# tongdun[rule_name=='借款人身份证命中法院执行法院失信证据库', rule_name:='tongdunIdDiscredit']

featuresWideU[, c("tongdunIdDiscredit","tongdunIdMultiLoanNum","tongdunPhoneDiscredit","tongdunPhoneMultiLoanNum"):=NULL]
# featuresWideU<-merge(featuresWideU, tongdun[, c("financingprojectid","final_score"), with=F], by="financingprojectid", all.x = T)
featuresWideU<-merge(featuresWideU, tdMulti, by="financingprojectid", all.x = T)


# 智策回传的CSV有duplicate(3个)
featuresWideU<-featuresWideU[!duplicated(financingprojectid),]



#####################################################
# not run
# endproduct:
featuresWideU # featuresWide with unionPay and tongdun added, hence featuresWide(U)

