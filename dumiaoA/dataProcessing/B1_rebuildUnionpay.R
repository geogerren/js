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

unionTrxn[, highRisk:=ifelse(
  (!is.na(str_locate(merchantname, "旗舰店")[1])
   |!is.na(str_locate(merchantname, "专营店")[1])
   |!is.na(str_locate(merchantname, "信息技术")[1])
   |!is.na(str_locate(merchantname, "数据")[1]))
  & (is.na(str_locate(merchantname, "奇漾")[1])
     &is.na(str_locate(merchantname, "京东")[1])
     &is.na(str_locate(merchantname, "当当网")[1]))
  & mcc=='5411', 1, 0)]

unionTrxn[, multiPlat:=ifelse(!is.na(str_locate(merchantname, "身份验证默认商户")[1])
                              |!is.na(str_locate(merchantname, "掌众科技")[1])
                              |!is.na(str_locate(merchantname, "钱袋宝")[1])
                              |!is.na(str_locate(merchantname, "钱袋网")[1])
                              |!is.na(str_locate(merchantname, "夸氪金融")[1]), 1, 0)]

unionTrxn[, night:=ifelse(hour(transtime) %in% c(1,2,3,4,5), 1, 0)]

# unionTrxn[, fTrans:=ifelse(acptresponsecode %in% c('资金不足','金额超限'), 1, 0)]

unionTrxn[, daysFromApply:=difftime(createtime,transtime,unit="day")]

################## aggregation
card1mth<-unionTrxn[daysFromApply<=30 & daysFromApply>0, 
                    .("RFM_1_var1"=.N,
                      "RFM_1_var2"=sum(transexpenses),
                      "RFM_1_var3"=sum(withdraw*transexpenses),
                      "RFM_1_var4"=sum(withdraw),
                      "RFM_1_var5"=NA,
                      "RFM_1_var6"=NA,
                      "RFM_1_var7"=NA,
                      "RFM_1_var8"=NA,
                      "RFM_1_var9"=NA,
                      "RFM_1_var10"=NA,
                      "RFM_1_var11"=sum(highRisk),
                      "RFM_1_var12"=sum(highRisk*transexpenses),
                      "RFM_1_var13"=sum(multiPlat*transexpenses),
                      "RFM_1_var14"=sum(multiPlat)
                    ), by=c("financingprojectid","createtime")]

card3mths<-unionTrxn[daysFromApply<=90 & daysFromApply>0,
                     .("RFM_3_var6"=sum(night*entertain),
                       "RFM_3_var7"=sum(night*entertain*transexpenses)
                       ), by=c("financingprojectid","createtime")]

card6mths<-unionTrxn[daysFromApply<=180 & daysFromApply>0, 
                     .("RFM_6_var1"=sum(transexpenses),
                       "RFM_6_var2"=.N,
                       "RFM_6_var12"=sum(food),
                       "RFM_6_var13"=sum(food*transexpenses),
                       "RFM_6_var14"=sum(ifelse(transexpenses>=3000,1,0)*transexpenses),
                       "RFM_6_var15"=sum(ifelse(transexpenses>=3000,1,0)),
                       "RFM_6_var17"=sum(highRisk),
                       "RFM_6_var18"=sum(highRisk*transexpenses),
                       "RFM_6_var19"=sum(multiPlat*transexpenses),
                       "RFM_6_var20"=sum(multiPlat),
                       "RFM_6_var21"=sum(entertain), 
                       # "FLAG_6_var11"=NA,
                       # "FLAG_6_var12"=NA,
                       "LOC_6_var12"=NA,
                       "LOC_6_var13"=NA,
                       "LOC_6_var14"=NA,
                       "MON_6_var1"=length(unique(month(transtime)))
                     ), by=c("financingprojectid","createtime")]


unionTrxnSupp<-copy(unionTrxn)
unionTrxnSupp[, monthOfTrans:=year(transtime)*100+month(transtime)]
unionTrxnSupp[, over3000:=ifelse(transexpenses>=3000, 1, 0)]
card6mthsSupp<-unionTrxnSupp[daysFromApply<=180 & daysFromApply>0, 
                               .("over3000Total"=sum(over3000),
                                 "total"=.N), 
                               by=c("financingprojectid", "monthOfTrans")]
card6mthsSupp[, over50:=ifelse(over3000Total/total>=0.5, 1, 0)]
card6mthsSupp[, over95:=ifelse(over3000Total/total>=0.95, 1, 0)]
card6mthsSupp_pivot<-card6mthsSupp[,
                                   .("FLAG_6_var11"=sum(over50),
                                     "FLAG_6_var12"=sum(over95)),
                                   by="financingprojectid"]


card6mths<-merge(card6mths, card6mthsSupp_pivot, by="financingprojectid", all.x=T)



card12mths<-unionTrxn[daysFromApply<=360 & daysFromApply>0, 
                    .("FLAG_12_var1"=NA,
                      "RFM_12_var1"=sum(transexpenses),
                      "RFM_12_var2"=.N,
                      "RFM_12_var29"=sum(withdraw),
                      "RFM_12_var30"=sum(withdraw*transexpenses),
                      "RFM_12_var55"=sum(night),
                      "RFM_12_var56"=sum(night*transexpenses),
                      "RFM_12_var58"=sum(pm),
                      "RFM_12_var59"=sum(pm*transexpenses),
                      "RFM_h_var2"=max(transtime)
                      ), by=c("financingprojectid","createtime")]


unionPayRebuilt<-merge(card12mths, card6mths, by=c("financingprojectid","createtime"), all.x=T)
unionPayRebuilt<-merge(unionPayRebuilt, card3mths, by=c("financingprojectid","createtime"), all.x=T)
unionPayRebuilt<-merge(unionPayRebuilt, card1mth, by=c("financingprojectid","createtime"), all.x=T)

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
