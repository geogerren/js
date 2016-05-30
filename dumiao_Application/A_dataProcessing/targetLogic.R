ods90DaysPerf<-ods[tenor>=3, ]
ods90DaysPerf[, daysFromDD:=statc_dt-Loan_Date]
ods90DaysPerf[, DPD8in90:=ifelse(Cur_Overdue_Days==8 & daysFromDD<=90, 1, 0)]
ods90DaysPerf[, DPD8out90:=ifelse(Cur_Overdue_Days==8 & daysFromDD>90, 1, 0)]

ods90DaysPerf[, DPD15in90:=ifelse(Cur_Overdue_Days==15 & daysFromDD<=90, 1, 0)]
ods90DaysPerf[, DPD15out90:=ifelse(Cur_Overdue_Days==15 & daysFromDD>90, 1, 0)]

ods90DaysPerf[, DPD1in90:=ifelse(Cur_Overdue_Days==1 & daysFromDD<=90, 1, 0)]
ods90DaysPerf[, DPD1out90:=ifelse(Cur_Overdue_Days==1 & daysFromDD>90, 1, 0)]
# ods90DaysPerf<-ods[statc_dt-Loan_Date<=90&tenor>=3, ]
ods90days_pivot<-ods90DaysPerf[, .("maxx"=max(statc_dt), 
                                   "loanDate"=min(Loan_Date),
                                   "DPD8in90"=sum(DPD8in90),
                                   "DPD8out90"=sum(DPD8out90),
                                   "DPD15in90"=sum(DPD15in90),
                                   "DPD15out90"=sum(DPD15out90),
                                   "DPD1in90"=sum(DPD1in90),
                                   "DPD1out90"=sum(DPD1out90)), by="project_id"]
ods90days_pivot[, maxDaysFromDD:=maxx-loanDate]


> nrow(ods90days_pivot[maxDaysFromDD<90 & DPD1in90>0 &DPD8in90 ==0,])
[1] 125
> nrow(ods90days_pivot[maxDaysFromDD<90 & DPD1in90>0 & DPD8in90 >0 & DPD15in90==0,])
[1] 23
> nrow(ods90days_pivot[maxDaysFromDD<90 & DPD1in90>0 & DPD8in90 >0 & DPD15in90>0,])
[1] 40
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD1in90>0 &DPD8in90 ==0,])
[1] 223
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD1in90>0 & DPD8in90 >0 & DPD15in90==0,])
[1] 28
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD1in90>0 & DPD8in90 >0 & DPD15in90>0,])
[1] 101
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD1out90>0 & DPD8out90==0,])
[1] 165
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD8out90==0,])
[1] 824
> nrow(ods90days_pivot[maxDaysFromDD<90 & DPD8in90 ==0,])
[1] 1494
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD8in90 ==0,])
[1] 764
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD1in90 ==0 & DPD1out90 ==0,])
[1] 453
> nrow(ods90days_pivot[maxDaysFromDD>=90 & DPD8in90 ==0 & DPD8out90 ==0,])
[1] 715
> nrow(ods90days_pivot[maxDaysFromDD>=90 & ((DPD15in90 ==0 & DPD8in90>0) | (DPD15out90 ==0 & DPD8out90>0)),])
[1] 57