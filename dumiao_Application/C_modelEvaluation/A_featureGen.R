# features<-read.csv(paste0(boxdata, "dumiao.csv"), stringsAsFactors = F)
# features<-data.table(features)

features<-ruleq("select 
t.service_id,
t.mod_id,
t.index_id,
t.rule_id,
t.input_val
from t_mod_score t
where index_id in (1000031.0,1000012,1000018,1000036.2,1000039)
and stamp between '2016-01-20' and '2016-04-15'
                ")

# run in database
cities<-dmq("select applyid, 
# province, city,
createtime,
case
when province in ('北京市','上海市') or city in ('广州市','深圳市') then 1
when province in ('广东省','湖北省','湖南省','河南省','山东省','山西省','贵州省') then 2
when province in ('安徽省','福建省','辽宁省','吉林省','广西省','重庆市') then 3
when province in ('云南省','河北省','江苏省','陕西省','江西省','浙江省','黑龙江省','四川省','天津市','海南省','甘肃省') then 4
 end as provinceTransformedBin
from loan_apply
            where createtime between '2016-01-20' and '2016-04-15'")
cities<-read.csv(paste0(boxdata, "cities.csv"))
cities<-as.data.table(cities)
cities[, applyid:=as.character(applyid)]

applyidMapping <- ruleq("select id as service_id, ser_rec_id as applyid
                        from t_cust_ser")

financingprojectidMapping <- ruleq("select financingprojectid, service_id, apply_id from project_detail")

tvardict<-ruleq("select 
      td.applyid
                ,td.var_code
                ,td.var_val
                from t_variable_dict td
                where var_code in ('modelScore','zhimaScore','avgMonthCall','longTimeShutdown','consumeFreg')
                ")

# append the 2 new features
RFMs<- ruleq("select service_id, class_1, val
from t_cust_ser_data
where class_1 in ('RFM_6_var12','RFM_6_var19')
")

########################################################################################
# t_mod_score
featureGen(features, 10014, '1000031.0', "modelScore")

featureGen(features, 10008, 1000012, "zhimaScore")
featureGen(features, 10010, 1000018, "avgMonthCall")
featureGen(features, 10016, 1000036.2, "longTimeShutdown")
featureGen(features, 10017, 1000039, "consumeFreg")


featuresWide<-dcast.data.table(features, service_id ~ key, fun.agg=min, value.var = "value")
featuresWide<-merge(featuresWide, applyidMapping, by.x="service_id", by.y="service_id")

##################################################################################################
# t_variable_dict
tvardictWide <- dcast.data.table(tvardict, applyid ~ var_code, fun.agg=min, value.var = "var_val")
names(tvardictWide) <- paste0(names(tvardictWide), ".1")



featuresWide <- merge(featuresWide, tvardictWide, by.x="applyid", by.y="applyid.1", all.x=T)

featuresWide[, avgMonthCall:=ifelse(is.na(avgMonthCall), avgMonthCall.1, avgMonthCall)]
featuresWide[, consumeFreg:=ifelse(is.na(consumeFreg), consumeFreg.1, consumeFreg)]
featuresWide[, longTimeShutdown:=ifelse(is.na(longTimeShutdown), longTimeShutdown.1, longTimeShutdown)]
# featuresWide[, modelScore:=ifelse(is.na(modelScore), modelScore.1, modelScore)]
featuresWide[, zhimaScore:=ifelse(is.na(zhimaScore), zhimaScore.1, zhimaScore)]

featuresWide[, names(tvardictWide):=NULL]

##################################################################################

featuresWide<-merge(featuresWide, cities, by.x = "applyid", by.y="applyid")
featuresWide[, createtime:=as.POSIXct(createtime)]
featuresWide[, "NA":=NULL]


# 海哥要再造2个变量
featuresWide[, applyHour:=hour(createtime)]
featuresWide[, applyTimeSegment:=ifelse(applyHour>=1 & applyHour<=6, "3", ifelse(applyHour>=9 & applyHour<=20, "1", "2"))]



##################################################################################


RFMsWide<-dcast.data.table(RFMs, service_id ~ class_1, fun.agg=min, value.var = "val")
RFMsWide<-merge(RFMsWide, applyidMapping, by.x="service_id", by.y="service_id")


featuresWide<-merge(featuresWide, RFMsWide, by.x = "applyid", by.y="applyid", all.x=T)






featuresWideTotalPopulation<-copy(featuresWide)
#####################################################

# library(jsonlite)
# 
# basePath<-paste0(boxdata, "json/")
# fileNames<-list.files(basePath)
# 
# unpJson<-data.frame()
# i<-1
# 
# 
# ok<-fromJSON(paste0(basePath, fileNames[2]))$data$data[, c("RFM_6_var12","RFM_6_var19")]
# ok$applyid<-268136
# unpJson<-ok
# 
# 
# 
# for(file in fileNames){
#   print(i)
#   newRecord<-fromJSON(paste0(basePath, file))$data$data[, c("RFM_6_var12","RFM_6_var19")]
#   if(is.null(newRecord))
#     next
#   newRecord$applyid <- substr(file, 1,6)
#   unpJson<-rbind(unpJson, newRecord)
#   i<-i+1
# }
# 
# ##################################################################
# featuresWideTotalPopulation<-merge(featuresWideTotalPopulation, unpJson, by="applyid", all.x=T)


oos<-copy(featuresWideTotalPopulation)
oos[, c("service_id.x","service_id.y"):=NULL]


