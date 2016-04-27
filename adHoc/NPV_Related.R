npv_disburse<-dmq("select
year(a.CreateTime)*100+month(a.CreateTime) as applyMonth,
              a.ApplyDays as tenor,
              sum(if(a.projectId>0,1,0)) as disbursement,
              FORMAT(sum(if(a.projectId>0,creditBalance,0)),2) as disburseamount
              from loan_apply a
              join dumiao_tracking b
              on a.applyid=b.applyid
              group by 1,2
              ")

write.csv(npv_disburse, paste0(boxdata, "npv_disburse.csv"))

#####################################################################################################
avg_rate_raw<-dmq("select ApplyDays, FeeRate, sum(ApplyBalance) as ApplyBalance
                from loan_apply 
                where ProjectID>0
and feerate>0
                group by 1,2  
                ")
avg_rate_raw[, totalDollor:=ApplyBalance*(1+FeeRate*ApplyDays)]

avg_rate<-avg_rate_raw[, .("tenorTotal"=sum(totalDollor),
                           "tenorBalance"=sum(ApplyBalance)),
                       by="ApplyDays"]
avg_rate[, avg_month_rate:=(tenorTotal/tenorBalance-1)/ApplyDays]


#######################################################################################################


odsMaxDD<-ods[, .("maxDD"=max(daysFromDD)), by="project_id"]
ods[, months:=floor(daysFromDD/30)]


# tenor=1且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==1 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==1 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Overdue_Principal)*100/169000

# tenor=2且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==2 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==2 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),
    .("totalDQ90"=sum(Overdue_Principal)*100/49000),
    by="months"]


# tenor=3且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==3 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==3 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),
    .("totalDQ90"=sum(Overdue_Principal)*100/221000),
    by="months"]


# tenor=6且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==6 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==6 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),
    .("totalDQ90"=sum(Overdue_Principal)*100/2510000),
    by="months"]


# tenor=12且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==12 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==12 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),
    .("totalDQ90"=sum(Overdue_Principal)*100/1087000),
    by="months"]


# tenor=18且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==18 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==18 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),
    .("totalDQ90"=sum(Overdue_Principal)/1087000),
    by="months"]


# tenor=24且有120天performance的总数
sum(ods[project_id %in% odsMaxDD[maxDD>120]$project_id & daysFromDD==0 & tenor==24 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),]$Loan_Amount)
# tenor=1且有120天performance且有过90天逾期的总数
ods[project_id %in% odsMaxDD[maxDD>120]$project_id & Cur_Overdue_Days==91 & tenor==24 & (Loan_Date<'2015-07-01' | Loan_Date>'2015-07-31'),
    .("totalDQ90"=sum(Overdue_Principal)/1087000),
    by="months"]













