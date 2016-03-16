target<-read.csv("E:/BaiduDropbox/jimu/Data/ods.csv")
target<-data.table(target)
target[Fst_Term_Overdue_Date!='\\N',]
sample<-target[project_id==98318,]
sample2<-target[project_id==98554,]
