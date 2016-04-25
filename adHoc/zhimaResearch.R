# 芝麻分和芝麻黑名单是否有关? 是否可以省却芝麻黑名单?

source("E:/Seafiles/Jimu/Code/js/sourceFile.R")
tcust<-ruleq("select
td.service_id
,td.class_1 
,td.class_2
,td.val
from t_cust_ser_data td
where data_source = 'zhima'
                ")
tcust[class_1=='score', class_2:='score']

zhimawide <- dcast.data.table(tcust, service_id ~ class_2, fun.agg=min, value.var = "val")
  
zhimawide <- zhimawide[, c("service_id","ipCredible", "ipRisk", "isUsed", "isSuccess", "isRisk","score"), with=F]
  
table(zhimawide$ipCredible)
table(zhimawide$ipRisk)
table(zhimawide$isUsed)
table(zhimawide$isSuccess)
table(zhimawide$isRisk)

zhimawide[, score:=as.numeric(score)]

a<-ggplot(zhimawide, aes(score, fill=isRisk))
a+geom_histogram(binwidth =10)


