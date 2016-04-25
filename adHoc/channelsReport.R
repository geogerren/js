channels<-dmq("select
year(a.CreateTime)*100+month(a.CreateTime) as applyMonth,
b.channel as channel,
count(1) as applied,
sum(if(a.applyStatus>0,1,0)) as processFinished,
sum(if(a.applyStatus=1 or a.applyStatus=3 or a.applyStatus=8 or a.applyStatus=10,1,0)) as passed,
sum(if(a.projectId>0,1,0)) as disbursement,
FORMAT(sum(if(a.projectId>0,creditBalance,0)),2) as disburseamount
from loan_apply a
join dumiao_tracking b
where a.applyid=b.applyid
and b.channel not like '90%'
group by 1,2
              ")

write.csv(channels, paste0(boxdata, "channelsRaw.csv"))
