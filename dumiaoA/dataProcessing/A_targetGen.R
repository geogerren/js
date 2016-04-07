source("E:/Seafiles/Jimu/Code/js/sourceFile.R")
ods<-read.csv("E:/Seafiles/Jimu/Data/ods_drawdown_loans.csv", stringsAsFactors = F)
ods<-data.table(ods)
ods[, Loan_Date:=as.Date(Loan_Date)]
ods[, statc_dt:=as.Date(statc_dt)]
ods<-ods[!(Loan_Date>='2015-11-03'&Loan_Date<='2015-11-30'), ]
ods<-ods[order(ods$Loan_Date)]


# 15 days DQ in 90 days 
# tenor>=3要求至少90天performance
ods90DaysPerf<-ods[statc_dt-Loan_Date<=90&tenor>=3, ]
ods90days_pivot<-ods90DaysPerf[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods90days_pivot[, maxDaysFromDD:=maxx-loanDate]
ods90days_pivot<-ods90days_pivot[maxDaysFromDD==90,]

ods90DaysPerf<-ods[project_id %in% ods90days_pivot$project_id, ]
ods90DaysPerf[, daysFromDD:=statc_dt-Loan_Date]
ods90DaysPerf[, DPD8to14in90:=ifelse(daysFromDD<=90&Cur_Overdue_Days>=8&Cur_Overdue_Days<=14, 1, 0)]
ods90DaysPerf[, DPD15out90:=ifelse(daysFromDD>90&Cur_Overdue_Days==15, 1, 0)]
ods90DaysPerf[, DPD15in90:=ifelse(daysFromDD<=90&Cur_Overdue_Days==15, 1, 0)]
target3<-ods90DaysPerf[, .("DPD8to14in90"=sum(DPD8to14in90), "DPD15out90"=sum(DPD15out90), "DPD15in90"=sum(DPD15in90)), 
                       by=c("project_id","Loan_Date","tenor")]
target3[, DPD8to14in:=ifelse(DPD8to14in90>0,1,0)]
target3[, DPD15out:=ifelse(DPD15out90>0,1,0)]
target3[, DPD15in:=ifelse(DPD15in90>0,1,0)]
target3[, flgDPD:=ifelse(DPD15in==1, 1, ifelse(DPD8to14in==1|DPD15out==1, -1, 0))]

target3[, c("DPD8to14in90","DPD15out90","DPD15in90"):=NULL]
target3Final<-target3[flgDPD!=-1,]

# k3<-floor(0.80*nrow(target3Final))
# target3Final[1:k3, flgTest:=0]
# target3Final[(k3+1):nrow(target3Final), flgTest:=1]


# table(target3$flgDPD)
# 不包含11.3-11.30
# -1   0   1 
# 52 738 103 

# 包含11.3-11.30
# -1    0    1 
# 75 1002  140 

# tenor=2要求至少60天performance
ods60DaysPerf<-ods[statc_dt-Loan_Date<=60&tenor==2, ]
ods60days_pivot<-ods60DaysPerf[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods60days_pivot[, maxDaysFromDD:=maxx-loanDate]
ods60days_pivot<-ods60days_pivot[maxDaysFromDD==60,]

ods60DaysPerf<-ods[project_id %in% ods60days_pivot$project_id, ]
ods60DaysPerf[, daysFromDD:=statc_dt-Loan_Date]
ods60DaysPerf[, DPD8to14in60:=ifelse(daysFromDD<=60&Cur_Overdue_Days>=8&Cur_Overdue_Days<=14, 1, 0)]
ods60DaysPerf[, DPD15out60:=ifelse(daysFromDD>60&Cur_Overdue_Days==15, 1, 0)]
ods60DaysPerf[, DPD15in60:=ifelse(daysFromDD<=60&Cur_Overdue_Days==15, 1, 0)]
target2<-ods60DaysPerf[, .("DPD8to14in60"=sum(DPD8to14in60), "DPD15out60"=sum(DPD15out60), "DPD15in60"=sum(DPD15in60)), 
                       by=c("project_id","Loan_Date","tenor")]
target2[, DPD8to14in:=ifelse(DPD8to14in60>0,1,0)]
target2[, DPD15out:=ifelse(DPD15out60>0,1,0)]
target2[, DPD15in:=ifelse(DPD15in60>0,1,0)]
target2[, flgDPD:=ifelse(DPD15in==1, 1, ifelse(DPD8to14in==1|DPD15out==1, -1, 0))]

target2[, c("DPD8to14in60","DPD15out60","DPD15in60"):=NULL]
target2Final<-target2[flgDPD!=-1,]

# k2<-floor(0.8*nrow(target2Final))
# target2Final[1:k2, flgTest:=0]
# target2Final[(k2+1):nrow(target2Final), flgTest:=1]



# table(target2$flgDPD)
# 不包含11.3-11.30
# 0  1 
# 30  4 

# 包含11.3-11.30
# -1  0  1 
# 2 36  5 

# tenor=1要求至少30天performance
ods30DaysPerf<-ods[statc_dt-Loan_Date<=30&tenor==1, ]
ods30days_pivot<-ods30DaysPerf[, .("maxx"=max(statc_dt), "loanDate"=min(Loan_Date)), by="project_id"]
ods30days_pivot[, maxDaysFromDD:=maxx-loanDate]
ods30days_pivot<-ods30days_pivot[maxDaysFromDD==30,]

ods30DaysPerf<-ods[project_id %in% ods30days_pivot$project_id, ]
ods30DaysPerf[, daysFromDD:=statc_dt-Loan_Date]
ods30DaysPerf[, DPD8to14in30:=ifelse(daysFromDD<=30&Cur_Overdue_Days>=8&Cur_Overdue_Days<=14, 1, 0)]
ods30DaysPerf[, DPD15out30:=ifelse(daysFromDD>30&Cur_Overdue_Days==15, 1, 0)]
ods30DaysPerf[, DPD15in30:=ifelse(daysFromDD<=30&Cur_Overdue_Days==15, 1, 0)]
target1<-ods30DaysPerf[, .("DPD8to14in30"=sum(DPD8to14in30), "DPD15out30"=sum(DPD15out30), "DPD15in30"=sum(DPD15in30)), 
                       by=c("project_id","Loan_Date","tenor")]
target1[, DPD8to14in:=ifelse(DPD8to14in30>0,1,0)]
target1[, DPD15out:=ifelse(DPD15out30>0,1,0)]
target1[, DPD15in:=ifelse(DPD15in30>0,1,0)]
target1[, flgDPD:=ifelse(DPD15in==1, 1, ifelse(DPD8to14in==1|DPD15out==1, 1, 0))]

target1[, c("DPD8to14in30","DPD15out30","DPD15in30"):=NULL]
target1Final<-target1[flgDPD!=-1,]

# k1<-floor(0.8*nrow(target1Final))
# target1Final[1:k1, flgTest:=0]
# target1Final[(k1+1):nrow(target1Final), flgTest:=1]


# table(target1$flgDPD)
# 不包含11.3-11.30
# -1   0 
# 17 111 

# 包含11.3-11.30
# -1   0 
# 23 137 

target<-rbind(target1Final, target2Final, target3Final)

# targetTotal<-rbind(target1, target2, target3)

################################################################################
# 去除失联人员
lostContact <- read.csv(paste0(boxdata, "lostContact0322.csv"))
lostContact<-data.table(lostContact)
mapping <- read.csv(paste0(boxdata, "financings.csv"),stringsAsFactors = F)
mapping<-data.table(mapping)
mapping[, borroweruserid:=as.integer(borroweruserid)]
mapping[, financingprojectid:=as.integer(financingprojectid)]
lostContact <- merge(lostContact, mapping, by.x="委案编号", by.y="borroweruserid", all.x=T)

target<-target[!(project_id %in% lostContact$financingprojectid & flgDPD==0), ]

# target[, flgDPD:=as.numeric(flgDPD)]



################################################################################
remove(lostContact)
remove(mapping)
remove(ods)
remove(ods30DaysPerf)
remove(ods30days_pivot)
remove(ods60DaysPerf)
remove(ods60days_pivot)
remove(ods90DaysPerf)
remove(ods90days_pivot)
remove(target1)
remove(target2)
remove(target3)
remove(k1)
remove(k2)
remove(k3)

target[project_id %in% sample(target$project_id, floor(nrow(target)*0.2)), flgValidation:=1]


