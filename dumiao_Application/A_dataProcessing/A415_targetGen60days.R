source("E:/Seafiles/Jimu/Code/js/sourceFile.R")
ods<-read.csv("E:/Seafiles/Jimu/Data/ods_drawdown_loans_413.csv", stringsAsFactors = F)
ods<-data.table(ods)
ods[, Loan_Date:=as.Date(Loan_Date)]
ods[, statc_dt:=as.Date(statc_dt)]
ods<-ods[!(Loan_Date>='2015-11-03'&Loan_Date<='2015-11-30'), ]
ods<-ods[order(ods$Loan_Date)]


ods[, daysFromDD:=statc_dt-Loan_Date]
ods[, DPD15:=ifelse(Cur_Overdue_Days==15, 1, 0)]


ods60DaysPerf<-ods[daysFromDD<=60, ]
ods60days_pivot<-ods60DaysPerf[, .("maxx"=max(statc_dt), 
                                   "loanDate"=min(Loan_Date),
                                   "DPD15"=sum(DPD15)), by="project_id"]
ods60days_pivot[, maxDaysFromDD:=maxx-loanDate]

# Indeterminant
nrow(ods60days_pivot[maxDaysFromDD<60 & DPD15==0, ])

# Bad
nrow(ods60days_pivot[DPD15>0, ])


ods60days_pivot<-ods60days_pivot[maxDaysFromDD==60,]


target2[, Good:=ifelse(DPD8==0 & maxFromDD>=60,1,0)]
target2[, Bad:=ifelse(DPD15>0,1,0)]
target2[, flgDPD:=ifelse(Good==1, 0, ifelse(Bad==1, 1, -1))]

# ods60DaysPerf<-ods[project_id %in% ods60days_pivot$project_id, ]
# ods60DaysPerf[, daysFromDD:=statc_dt-Loan_Date]
# ods60DaysPerf[, DPD8to14in60:=ifelse(daysFromDD<=60&Cur_Overdue_Days>=8&Cur_Overdue_Days<=14, 1, 0)]
# ods60DaysPerf[, DPD15out60:=ifelse(daysFromDD>60&Cur_Overdue_Days==15, 1, 0)]
# ods60DaysPerf[, DPD15in60:=ifelse(daysFromDD<=60&Cur_Overdue_Days==15, 1, 0)]
# target2<-ods60DaysPerf[, .("DPD8to14in60"=sum(DPD8to14in60), "DPD15out60"=sum(DPD15out60), "DPD15in60"=sum(DPD15in60)), 
#                        by=c("project_id","Loan_Date","tenor")]
# target2[, DPD8to14in:=ifelse(DPD8to14in60>0,1,0)]
# target2[, DPD15out:=ifelse(DPD15out60>0,1,0)]
# target2[, DPD15in:=ifelse(DPD15in60>0,1,0)]
# target2[, flgDPD:=ifelse(DPD15in==1, 1, ifelse(DPD8to14in==1|DPD15out==1, -1, 0))]

# target2[, c("DPD8to14in60","DPD15out60","DPD15in60"):=NULL]
target2Final<-target2[flgDPD!=-1,]

# k2<-floor(0.8*nrow(target2Final))
# target2Final[1:k2, flgTest:=0]
# target2Final[(k2+1):nrow(target2Final), flgTest:=1]


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
# remove(lostContact)
# remove(mapping)
# remove(ods)
# remove(ods30DaysPerf)
# remove(ods30days_pivot)
# remove(ods60DaysPerf)
# remove(ods60days_pivot)
# remove(ods90DaysPerf)
# remove(ods90days_pivot)
# remove(target1)
# remove(target2)
# remove(target3)
# remove(k1)
# remove(k2)
# remove(k3)

set.seed(2016)
target[project_id %in% sample(target$project_id, floor(nrow(target)*0.2)), flgValidation:=1]




#####################################################
# not run
# endproduct:
target


