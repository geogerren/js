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

unionTrxn[, fTrans:=ifelse(acptresponsecode %in% c('资金不足','金额超限'), 1, 0)]

unionTrxn[, daysFromApply:=difftime(createtime,transtime,unit="day")]

################## aggregation
# cardterm<-unionTrxn[, .("cardTerm"=max(daysFromApply)), by=c("financingprojectid","createtime")]

card3mths<-unionTrxn[daysFromApply<=90 & daysFromApply>0, 
                     .("creditWD3Months"=sum(withdraw)
                       ), by=c("financingprojectid","createtime")]

card6mths<-unionTrxn[daysFromApply<=180 & daysFromApply>0, 
                     .("hightRiskTransNum6"=sum(highRisk),
                       "hightRiskTransAvg6Total"=sum(highRisk*transexpenses),
                       "post6MonthOverdrawNum"=sum(overdraw),
                       "transFalsePast6"=sum(fTrans),
                       "useCardNumPMTotal"=sum(consume)
                       ), by=c("financingprojectid","createtime")]

card12mths<-unionTrxn[daysFromApply<=360 & daysFromApply>0, 
                    .("post12PMFeeAmount"=sum(pm*transexpenses),
                      "post12PMFeeNum"=sum(pm)
                      ), by=c("financingprojectid","createtime")]

card1mth<-unionTrxn[daysFromApply<=30 & daysFromApply>0, 
                    .("highRiskTransAvg1Total"=sum(highRisk*transexpenses),
                      "multiBorrowNumP1"=sum(multiPlat),
                      "lastMonthOverdrawNum"=sum(overdraw),
                      "transFalsePastMonth"=sum(fTrans)
                    ), by=c("financingprojectid","createtime")]


