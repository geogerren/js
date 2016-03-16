jmsd2<-read.csv("E:/BaiduDropbox/jimu/Business/jmsd_perf_vintage_201602.csv", stringsAsFactors = F)
jmsd2<-data.table(jmsd2)
jmsd2<-jmsd2[SubChannel=='读秒',]
jmsd2[, ddstamp:=as.Date(ddstamp)]
jmsd2[ever_delq_flag!=0,]


jmsdAnalysis3<-jmsd2[monthDiff(ddmonth,reportmth)<=3,]
jmsdAnalysis1<-jmsd2[monthDiff(ddmonth,reportmth)<=1,]

jmsdAnalysis3Pivot<-jmsdAnalysis3[, .("everDQInFirst3Mth"=ifelse(sum(ever_delq_flag)>0,1,0),
                                      "everDQ30InFirst3Mth"=ifelse(sum(ever30_delq_flag)>0,1,0)),
                                  by=c("ddmonth","Name")]

jmsdAnalysis1Pivot<-jmsdAnalysis1[, .("everDQInFirst1Mth"=ifelse(sum(ever_delq_flag)>0,1,0),
                                      "everDQ30InFirst1Mth"=ifelse(sum(ever30_delq_flag)>0,1,0)),
                                  by=c("ddmonth","Name")]

pivot3<-jmsdAnalysis3Pivot[, .("cnt"=.N), by=c("ddmonth","everDQInFirst3Mth","everDQ30InFirst3Mth")]
pivot1<-jmsdAnalysis1Pivot[, .("cnt"=.N), by=c("ddmonth","everDQInFirst1Mth","everDQ30InFirst1Mth")]


write.csv(pivot3, paste0(box, "pivot3.csv"))
write.csv(pivot1, paste0(box, "pivot1.csv"))
