# 氪信

creditXAnalysis<-read.csv(paste0(boxdata, "kexinAnalysis.csv"))
creditXAnalysis<-data.table(creditXAnalysis)
creditXAnalysis<-creditXAnalysis[, c("applytime","applyid","Pass","riskLevel","riskDesc"), with=F]
creditXAnalysis[, applytime:=as.POSIXct(applytime)]

table(creditXAnalysis$applytime)

ods<-read.csv("E:/Seafiles/Jimu/Data/ods_drawdown_loans_413.csv", stringsAsFactors = F)

projectid2applyid <- ruleq("select apply_id as applyid, financingprojectid as project_id
                           from project_detail")

ods<-data.table(ods)
ods[, Loan_Date:=as.Date(Loan_Date)]
ods[, statc_dt:=as.Date(statc_dt)]

target_creditX<-rbind(target1[, c("project_id", "Loan_Date", "tenor", "flgDPD"), with=F], 
              target2[, c("project_id", "Loan_Date", "tenor", "flgDPD"), with=F], 
              target3[, c("project_id", "Loan_Date", "tenor", "flgDPD"), with=F])


target_creditX<-merge(target_creditX, projectid2applyid, by.x = "project_id", by.y = "project_id")

creditXAnalysis[, applyid:=as.character(applyid)]
creditXAnalysis<-merge(creditXAnalysis, target_creditX, by.x="applyid", by.y="applyid", all.x=T)

creditXAnalysis[is.na(flgDPD), flgDPD:=-1]

creditXAnalysis<-creditXAnalysis[!is.na(Pass),]

creditXAnalysis[flgDPD==-1, flgDPD:=0]

creditX<-creditXAnalysis[, .("num"=.N),
                         by=c("riskDesc","Pass","flgDPD")]


write.csv(creditX, paste0(boxdata, "creditX.csv"))
