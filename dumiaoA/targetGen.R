ods<-read.csv("E:/BaiduDropbox/jimu/Data/ods_drawdown_loans.csv", stringsAsFactors = F)
ods<-data.table(target)
ods[, Loan_Date:=as.Date(Loan_Date)]
ods[, statc_dt:=as.Date(statc_dt)]
ods90days<-ods[statc_dt-Loan_Date<=90, ]

ods90days_pivot<-ods90days[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods90days_pivot[, daysFromDD:=maxx-loanDate]
ods90days_pivot<-ods90days_pivot[daysFromDD==90,]

ods90days<-ods90days[project_id %in% ods90days_pivot$project_id, ]
ods90days[, od15:=ifelse(Cur_Overdue_Days==15, 1, 0)]
target<-ods90days[, .("ever15"=sum(od15)), by="project_id"]

target[, ODFlag:=ifelse(ever15>0,1,0)]
