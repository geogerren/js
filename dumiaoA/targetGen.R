ods<-read.csv("E:/Seafiles/Jimu/Data/ods_drawdown_loans.csv", stringsAsFactors = F)
ods<-data.table(ods)
ods[, Loan_Date:=as.Date(Loan_Date)]
ods[, statc_dt:=as.Date(statc_dt)]

ods90days<-ods[statc_dt-Loan_Date<=90&tenor>=3, ]
ods90days_pivot<-ods90days[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods90days_pivot[, maxDaysFromDD:=maxx-loanDate]
ods90days_pivot<-ods90days_pivot[maxDaysFromDD==90,]
ods90days<-ods90days[project_id %in% ods90days_pivot$project_id, ]
ods90days[, od15:=ifelse(Cur_Overdue_Days==15, 1, 0)]
target3<-ods90days[, .("ever15"=sum(od15)), by=c("project_id", "Loan_Date")]
target3[, ODFlag:=ifelse(ever15>0,1,0)]


ods60days<-ods[statc_dt-Loan_Date<=60&tenor==2, ]
ods60days_pivot<-ods60days[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods60days_pivot[, maxDaysFromDD:=maxx-loanDate]
ods60days_pivot<-ods60days_pivot[maxDaysFromDD==60,]
ods60days<-ods60days[project_id %in% ods60days_pivot$project_id, ]
ods60days[, od15:=ifelse(Cur_Overdue_Days==15, 1, 0)]
target2<-ods60days[, .("ever15"=sum(od15)), by=c("project_id", "Loan_Date")]
target2[, ODFlag:=ifelse(ever15>0,1,0)]

ods30days<-ods[statc_dt-Loan_Date<=30&tenor==1, ]
ods30days_pivot<-ods30days[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods30days_pivot[, maxDaysFromDD:=maxx-loanDate]
ods30days_pivot<-ods30days_pivot[maxDaysFromDD==30,]
ods30days<-ods30days[project_id %in% ods30days_pivot$project_id, ]
ods30days[, od15:=ifelse(Cur_Overdue_Days==15, 1, 0)]
target1<-ods30days[, .("ever15"=sum(od15)), by=c("project_id", "Loan_Date")]
target1[, ODFlag:=ifelse(ever15>0,1,0)]

target<-rbind(target1, target2, target3)
