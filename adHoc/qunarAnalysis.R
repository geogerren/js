
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
