
############################################################################################
# tdMulti<-read.csv(paste0(boxdata, "mytable.csv"))

# tdMulti<-ruleq("select p.financingprojectid
#                ,var_val as tongdunMultiLoanNum
#                from t_variable_dict t
#                join project_detail p
#                on t.applyid = p.apply_id
#                where t.var_code='tongdunMultiLoanNum'
#                ")

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


tdMulti <- ruleq("select 
t.rule_detail, 
t.rule_name,
p.financingprojectid
from t_pat_tongdun_detail t
join project_detail p
on t.service_id = p.service_id
where t.rule_name in ('3个月内手机在多个平台进行借款', '3个月内身份证在多个平台进行借款')
")



tdMulti[, PlatNum:=strParseV(rule_detail, '个数', regexpStopping = '.*\\}\\,', strStart = "=", strEnd = ",")]
tdMulti[, LoanNum:=strParseV(rule_detail, '计算结果', regexpStopping = '.*\\}\\]', strStart = "=", strEnd = "\\}")]

tdMulti[, PhonePlatNum:=ifelse(rule_name=='3个月内手机在多个平台进行借款', 1, NA)*as.numeric(PlatNum)]
tdMulti[, IDPlatNum:=ifelse(rule_name=='3个月内身份证在多个平台进行借款', 1, NA)*as.numeric(PlatNum)]
tdMulti[, PhoneLoanNum:=ifelse(rule_name=='3个月内手机在多个平台进行借款', 1, NA)*as.numeric(LoanNum)]
tdMulti[, IDLoanNum:=ifelse(rule_name=='3个月内身份证在多个平台进行借款', 1, NA)*as.numeric(LoanNum)]


tdMultiWide <- tdMulti[, .("PhonePlatNum"=sum(PhonePlatNum, na.rm = T),
                           "PhoneLoanNum"=sum(PhoneLoanNum, na.rm = T),
                           "IDPlatNum"=sum(IDPlatNum, na.rm = T),
                           "IDLoanNum"=sum(IDLoanNum, na.rm = T)),
                       by="financingprojectid"]



# featuresWideU[, c("tongdunIdDiscredit","tongdunIdMultiLoanNum","tongdunPhoneDiscredit","tongdunPhoneMultiLoanNum"):=NULL]
featuresWideU<-merge(featuresWideU, tdMultiWide, by="financingprojectid", all.x = T)


# 智策回传的CSV有duplicate(3个)
featuresWideU<-featuresWideU[!duplicated(financingprojectid),]

