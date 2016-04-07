tdMulti<-read.csv(paste0(boxdata, "mytable.csv"))

# 补全同盾缺失
tongdun<-ruleq("select pd.financingprojectid
               ,DATE(pd.createtime) as createtime
               ,td.rule_name
               ,td.final_score
               from t_pat_tongdun_blank td
               join project_detail pd
               on td.service_id = pd.service_id
               ")
tongdun[rule_name=='3个月内身份证在多个平台进行借款', rule_name:='tongdunIdMultiLoanNum']
tongdun[rule_name=='3个月内手机在多个平台进行借款', rule_name:='tongdunPhoneMultiLoanNum']
tongdun[rule_name=='3个月内银行卡在多个平台进行借款', rule_name:='tongdunBankCardMultiLoanNum']
tongdun[rule_name=='借款人身份证命中法院执行法院失信证据库', rule_name:='tongdunIdDiscredit']

featuresWideU[, c("tongdunIdDiscredit","tongdunIdMultiLoanNum","tongdunPhoneDiscredit","tongdunPhoneMultiLoanNum"):=NULL]
# featuresWideU<-merge(featuresWideU, tongdun[, c("financingprojectid","final_score"), with=F], by="financingprojectid", all.x = T)
featuresWideU<-merge(featuresWideU, tdMulti, by="financingprojectid", all.x = T)


# 智策回传的CSV有duplicate(3个)
featuresWideU<-featuresWideU[!duplicated(financingprojectid),]
