unionTrxn<-ruleq("select pd.financingprojectid
                  ,DATE(pd.createtime) as createtime
                  ,tx.transtime
                 ,tx.transcurrcode
                 ,tx.acptresponsecode
                 ,tx.transreceipts
                 ,tx.transexpenses
,tx.merchantname
                  ,tx.mcc 
                 ,tx.transType
                 from t_cust_ser_trans_detail tx
                 join project_detail pd
                 on tx.service_id = pd.service_id
                 ")
unionTrxn[, transtime:=as.POSIXct(transtime)]
unionTrxn[, createtime:=as.POSIXct(createtime)]
##################################################################################
# 通过cust_ser_trans table重建银联变量
unionTrxn[, consume:=
            ifelse(acptresponsecode %in% c('00','10','11','16','A2','A4','A5','A6','Y1','Y3','成功') 
                   & transType %in% c('S22','消费'), 1, 0)]
unionTrxn[, withdraw:=
            ifelse(acptresponsecode %in% c('00','10','11','16','A2','A4','A5','A6','Y1','Y3','成功') 
                   & transType %in% c('S24','取现'), 1, 0)]

unionTrxn[, overdraw:=
            ifelse(acptresponsecode %in% c('资金不足','金额超限') 
                   & transType %in% c('S24','取现'), 1, 0)]


unionTrxn[, food:=ifelse(mcc %in% c('5812','5813','5814','快餐店','食品零售','就餐场所'), 1, 0)]
unionTrxn[, pm:=ifelse(mcc %in% c('6513','物业管理'), 1, 0)]
unionTrxn[, entertain:=ifelse(mcc %in% c('7932','7933','7993','7994','7995','宾馆餐饮娱乐优惠类','饮酒场所','电影院'
                                         ,'洗浴按摩','歌舞厅KTV','戏剧演出','其他娱乐服务','乐队文艺表演'), 1, 0)]

# 高危：带旗舰店专营店信息技术字样，无奇漾京东当当网字样，mcc=5411
unionTrxn$highRisk<-ifelse(
  (!is.na(str_match(unionTrxn$merchantname, "旗舰店"))
   |!is.na(str_match(unionTrxn$merchantname, "专营店"))
   |!is.na(str_match(unionTrxn$merchantname, "信息技术"))
   |!is.na(str_match(unionTrxn$merchantname, "数据")))
  & (is.na(str_match(unionTrxn$merchantname, "奇漾"))
     &is.na(str_match(unionTrxn$merchantname, "京东"))
     &is.na(str_match(unionTrxn$merchantname, "当当网")))
  & unionTrxn$mcc=='5411', 1, 0)

unionTrxn[, multiPlat:=ifelse(merchantname %in% c("身份验证默认商户", "掌众科技", "钱袋宝", "钱袋网", "夸氪金融"), 1, 0)]

unionTrxn[, night:=ifelse(hour(transtime) %in% c(1,2,3,4,5), 1, 0)]

# unionTrxn[, fTrans:=ifelse(acptresponsecode %in% c('资金不足','金额超限'), 1, 0)]

unionTrxn[, daysFromApply:=difftime(createtime,transtime,unit="day")]

################## aggregation



unionPayRebuilt<-unionTrxn[daysFromApply>0, 
                    .("RFM_1_var1"=sum(ifelse(daysFromApply<=30, 1, 0), na.rm=T),
                      "RFM_1_var2"=sum(ifelse(daysFromApply<=30, transexpenses, 0), na.rm=T),
                      "RFM_1_var3"=sum(ifelse(daysFromApply<=30, withdraw*transexpenses, 0), na.rm=T),
                      "RFM_1_var4"=sum(ifelse(daysFromApply<=30, withdraw, 0), na.rm=T),
                      "RFM_1_var5"=NA,
                      "RFM_1_var6"=NA,
                      "RFM_1_var7"=NA,
                      "RFM_1_var8"=NA,
                      "RFM_1_var9"=NA,
                      "RFM_1_var10"=NA,
                      "RFM_1_var11"=sum(ifelse(daysFromApply<=30, highRisk, 0), na.rm=T),
                      "RFM_1_var12"=sum(ifelse(daysFromApply<=30, highRisk*transexpenses, 0), na.rm=T),
                      "RFM_1_var13"=sum(ifelse(daysFromApply<=30, multiPlat*transexpenses, 0), na.rm=T),
                      "RFM_1_var14"=sum(ifelse(daysFromApply<=30, multiPlat, 0), na.rm=T),
                      
                      "RFM_3_var6"=sum(ifelse(daysFromApply<=90, night*entertain, 0), na.rm=T),
                      "RFM_3_var7"=sum(ifelse(daysFromApply<=90, night*entertain*transexpenses, 0), na.rm=T),
                      
                      "RFM_6_var1"=sum(ifelse(daysFromApply<=180, transexpenses, 0), na.rm=T),
                      "RFM_6_var2"=sum(ifelse(daysFromApply<=180, 1, 0), na.rm=T),
                      "RFM_6_var12"=sum(ifelse(daysFromApply<=180, food, 0), na.rm=T),
                      "RFM_6_var13"=sum(ifelse(daysFromApply<=180, food*transexpenses, 0), na.rm=T),
                      "RFM_6_var14"=sum(ifelse(transexpenses>=3000 & daysFromApply<=180,1,0)*transexpenses, na.rm=T),
                      "RFM_6_var15"=sum(ifelse(transexpenses>=3000 & daysFromApply<=180,1,0), na.rm=T),
                      "RFM_6_var17"=sum(ifelse(daysFromApply<=180, highRisk, 0), na.rm=T),
                      "RFM_6_var18"=sum(ifelse(daysFromApply<=180, highRisk*transexpenses, 0), na.rm=T),
                      "RFM_6_var19"=sum(ifelse(daysFromApply<=180, multiPlat*transexpenses, 0), na.rm=T),
                      "RFM_6_var20"=sum(ifelse(daysFromApply<=180, multiPlat, 0), na.rm=T),
                      "RFM_6_var21"=sum(ifelse(daysFromApply<=180, entertain, 0), na.rm=T), 
                      # "FLAG_6_var11"=NA,
                      # "FLAG_6_var12"=NA,
                      "LOC_6_var12"=NA,
                      "LOC_6_var13"=NA,
                      "LOC_6_var14"=NA,
                      
                      "MON_6_var1_1"=sum(ifelse(daysFromApply<=30, 1, 0), na.rm=T),
                      "MON_6_var1_2"=sum(ifelse(daysFromApply<=60 & daysFromApply>30, 1, 0), na.rm=T),
                      "MON_6_var1_3"=sum(ifelse(daysFromApply<=90 & daysFromApply>60, 1, 0), na.rm=T),
                      "MON_6_var1_4"=sum(ifelse(daysFromApply<=120 & daysFromApply>90, 1, 0), na.rm=T),
                      "MON_6_var1_5"=sum(ifelse(daysFromApply<=150 & daysFromApply>120, 1, 0), na.rm=T),
                      "MON_6_var1_6"=sum(ifelse(daysFromApply<=180 & daysFromApply>150, 1, 0), na.rm=T),

                      "FLAG_12_var1"=NA,
                      "RFM_12_var1"=sum(transexpenses, na.rm=T),
                      "RFM_12_var2"=.N,
                      "RFM_12_var29"=sum(withdraw, na.rm=T),
                      "RFM_12_var30"=sum(withdraw*transexpenses, na.rm=T),
                      "RFM_12_var55"=sum(night, na.rm=T),
                      "RFM_12_var56"=sum(night*transexpenses, na.rm=T),
                      "RFM_12_var58"=sum(pm, na.rm=T),
                      "RFM_12_var59"=sum(pm*transexpenses, na.rm=T),
                      "RFM_h_var2"=max(as.POSIXct(transtime), na.rm=T)                      
                      
                    ), by=c("financingprojectid","createtime")]

unionPayRebuilt[, MON_6_var1:=
                  ifelse(MON_6_var1_1>0,1,0)+
                  ifelse(MON_6_var1_2>0,1,0)+
                  ifelse(MON_6_var1_3>0,1,0)+
                  ifelse(MON_6_var1_4>0,1,0)+
                  ifelse(MON_6_var1_5>0,1,0)+
                  ifelse(MON_6_var1_6>0,1,0)]
unionPayRebuilt[, c("MON_6_var1_1","MON_6_var1_2","MON_6_var1_3","MON_6_var1_4","MON_6_var1_5","MON_6_var1_6"):=NULL]

unionTrxnSupp<-copy(unionTrxn)
unionTrxnSupp[, monthOfTrans:=year(transtime)*100+month(transtime)]
unionTrxnSupp[, over3000:=ifelse(transexpenses>=3000, 1, 0)]
card6mthsSupp<-unionTrxnSupp[daysFromApply>0, 
                             .("over3000Total"=sum(ifelse(daysFromApply<=180, over3000, 0), na.rm = T),
                               "total"=sum(ifelse(daysFromApply<=180, 1, 0), na.rm=T)), 
                             by=c("financingprojectid", "monthOfTrans")]
card6mthsSupp[, over50:=ifelse(over3000Total/total>=0.5, 1, 0)]
card6mthsSupp[, over95:=ifelse(over3000Total/total>=0.95, 1, 0)]
card6mthsSupp_pivot<-card6mthsSupp[,
                                   .("FLAG_6_var11"=sum(over50, na.rm = T),
                                     "FLAG_6_var12"=sum(over95, na.rm = T)),
                                   by="financingprojectid"]


unionPayRebuilt<-merge(unionPayRebuilt, card6mthsSupp_pivot, by="financingprojectid", all.x=T)




unionPayRebuilt[, c("card_tp",
                "cnp_score",
                "cot_cluster",
                "cot_score" ,
                "dc_flag",
                "flag_h_var1",
                "RFM_56_var1",
                "rsk_cluster",
                "rsk_score" ,
                "wlp_score"):=NA
                ]
unionPayRebuilt[, createtime:=NULL]
# unionPayRebuilt[, unionPayDerived:=1]
# names(unionPayRebuilt)<-paste0(names(unionPayRebuilt), ".1")
# 

#######################################################################################################
remove(card12mths)
remove(card1mth)
remove(card3mths)
remove(card6mths)
remove(unionTrxn)
