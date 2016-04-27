
source("E:/Seafiles/Jimu/Code/js/sourceFile.R")

qunar<-dmq("select
a.*
from loan_apply a
join dumiao_tracking b
where a.applyid=b.applyid
and b.channel='qunar'
")  

qunar[, applyDate:=as.Date(CreateTime)]

qunarAnalysis<-qunar[, .(cnt=.N), by=c("applyDate", "Status", "ApplyStatus")]


#########################################################################################
wuba<-dmq("select
a.*
           from loan_apply a
           join dumiao_tracking b
           where a.applyid=b.applyid
           and b.channel='58'
           ")  

wuba[, applyDate:=as.Date(CreateTime)]

wubaAnalysis<-wuba[, .(cnt=.N), by=c("applyDate", "Status", "ApplyStatus")]


write.csv(wubaAnalysis, paste0(boxdata, "wuba.csv"))
