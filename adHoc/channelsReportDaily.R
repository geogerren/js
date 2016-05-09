##########################################################################
# 各渠道的前端转化


upfront<-dmq("select
             date(a.CreateTime) as applyDate,
             case 
             when b.channel='dumiaowechat' then 'WechatDumiaoPublic'              
             when b.channel='58' and creditBalance <=5000 then '58Tongcheng1'
             when b.channel='58' and creditBalance <=10000 and creditBalance >5000 then '58Tongcheng2'
             when b.channel='58' and creditBalance >10000 then '58Tongcheng3'
             when b.channel in ('groupmessage','singlemessage','timeline','wechatshare') then 'Wechat'
             when b.channel in ('baidu','jmhz01','jmhz10','jmhz15','jmhz16') then 'Baidu'
             when b.channel in ('jmhz3','jmhz17') then '360SEM'
             when b.channel='jmhz31' then 'FenqiGuanjia'
             when b.channel in ('jmhz32','jmhz33','jmhz34','jmhz35','jmhz36') then 'JunheWenhua'
             when b.channel in ('jmhz49','jmhz50') then 'Yinteli'
             when b.channel='jmhz53' then 'RenrenXin'
             when b.channel='jmhz61' then 'JinriToutiao'
             when b.channel in ('jmhz62','jielema') then 'Jielema'
             
             when b.channel='jmhz18' then 'SoudaiWang'
             when b.channel='jmhz54' then 'Linshijie'
             when b.channel='jmhz67' then 'BenniaoFenqi'
             when b.channel in ('jmhz88','jmhz89','jmhz90','jmhz91') then 'SudaiZhijia'
             when b.channel like 'woaika%' then 'Woaika'
             when b.channel='xinhe' then 'Xinhe'
             when b.channel='qunar' then 'Qunar'
             when b.channel='haodai' then 'HaodaiWang'
             when b.channel='jiedianqian' then 'Jiedianqian'
             when b.channel like '90%' or b.channel='ds9890' then 'DS'
             when b.channel like 'gengmeidai%' then 'Gengmeidai'
             else b.channel end
             as channel_1,
             case 
             when status =1 then '1_LoanInfo'
             when status =2 then '2_PersonalInfo'
             when status =3 then '3_FamilyInfo'
             when status =4 then '4_JuxinliAuth'
             when status =5 then '5_UnionpayAuth'
             when status =6 then '6_ZhimaAuth'
             when status =10 then '9_PROCESSFINISHED'
             end as status, 
             count(1) as cnt
             from loan_apply a
             join dumiao_tracking b
             where a.applyid=b.applyid
             group by 1,2,3
             ")


upfront[, channel_2:=ifelse(channel_1 %in% c("58Tongcheng1","58Tongcheng2","58Tongcheng3","chanrong","FenqiGuanjia","HaodaiWang","Jiedianqian",
                                             "Jielema","JunheWenhua","Qunar","SudaiZhijia","Woaika","Xinhe"), '渠道', 
                            ifelse(channel_1 %in% c("Baidu","360SEM","JinriToutiao"), '品专', '自有'))]

write.csv(upfront, paste0(boxdata, "upfront.csv"))







#########################################################################################################
# 所有渠道的申请到放款数据

channels<-dmq("select
date(a.CreateTime) as applyDate,
              case 
              when b.channel='dumiaowechat' then 'WechatDumiaoPublic'              
              when b.channel='58' and creditBalance <=5000 then '58Tongcheng1'
              when b.channel='58' and creditBalance <=10000 and creditBalance >5000 then '58Tongcheng2'
              when b.channel='58' and creditBalance >10000 then '58Tongcheng3'
              when b.channel in ('groupmessage','singlemessage','timeline','wechatshare') then 'Wechat'
              when b.channel in ('baidu','jmhz01','jmhz10','jmhz15','jmhz16') then 'Baidu'
              when b.channel in ('jmhz3','jmhz17') then '360SEM'
              when b.channel='jmhz31' then 'FenqiGuanjia'
              when b.channel in ('jmhz32','jmhz33','jmhz34','jmhz35','jmhz36') then 'JunheWenhua'
              when b.channel in ('jmhz49','jmhz50') then 'Yinteli'
              when b.channel='jmhz53' then 'RenrenXin'
              when b.channel='jmhz61' then 'JinriToutiao'
              when b.channel in ('jmhz62','jielema') then 'Jielema'
              
              when b.channel='jmhz18' then 'SoudaiWang'
              when b.channel='jmhz54' then 'Linshijie'
              when b.channel='jmhz67' then 'BenniaoFenqi'
              when b.channel in ('jmhz88','jmhz89','jmhz90','jmhz91') then 'SudaiZhijia'
              when b.channel like 'woaika%' then 'Woaika'
              when b.channel='xinhe' then 'Xinhe'
              when b.channel='qunar' then 'Qunar'
              when b.channel='haodai' then 'HaodaiWang'
              when b.channel='jiedianqian' then 'Jiedianqian'
              when b.channel like '90%' or b.channel='ds9890' then 'DS'
              when b.channel like 'gengmeidai%' then 'Gengmeidai'
              else b.channel end
              as channel_1,
              count(1) as applied,
              sum(if(a.applyStatus>0,1,0)) as processFinished,
              sum(if(a.applyStatus=1 or a.applyStatus=3 or a.applyStatus=8 or a.applyStatus=10,1,0)) as passed,
              sum(if(a.projectId>0,1,0)) as disbursement,
              FORMAT(sum(if(a.projectId>0,creditBalance,0)),2) as disburseamount
              from loan_apply a
              join dumiao_tracking b
              where a.applyid=b.applyid
and a.createtime > date_add(date(NOW()), interval -30 day)
              group by 1,2")

channels[, channel_2:=ifelse(channel_1 %in% c("58Tongcheng1","58Tongcheng2","58Tongcheng3","chanrong","Fenqiguanjia","HaodaiWang","Jiedianqian",
                                              "Jielema","JunheWenhua","Qunar","SudaiZhijia","Woaika","Xinhe"), '渠道', 
                             ifelse(channel_1 %in% c("Baidu","360SEM","JinriToutiao"), '品专', '自有'))]


write.csv(channels, paste0(boxdata, "channelsRaw.csv"))



