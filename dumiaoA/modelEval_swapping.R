scores<-ruleq("select 
p.financingprojectid, 
                t.mod_id,
                t.index_id,
                t.rule_id,
                t.input_val
                from t_mod_score t
                join project_detail p
                on t.service_id = p.service_id
                ")

featureGen(scores, 10014, "1000031.0", "modelScore")
featureGen(scores, 10014, "1000031.1", "zhimaScore")


scoresWide<-dcast.data.table(scores, financingprojectid ~ key, fun.agg=max, value.var = "value")
scoresWide<-merge(scoresWide, target, by.x="financingprojectid", by.y="project_id")

scoresWide[, modelScore:=as.numeric(modelScore)]
banding(scoresWide, "modelScore", "modelScoreBand")

scoresWide[, zhimaScore:=as.numeric(zhimaScore)]
banding(scoresWide, "zhimaScore", "zhimaScoreBand")

modelScoreAnalysis<-scoresWide[, .("badCnt"=sum(ODFlag), "totalCnt"=.N), by="modelScoreBand"]
zhimaScoreAnalysis<-scoresWide[, .("badCnt"=sum(ODFlag), "totalCnt"=.N), by="zhimaScoreBand"]

quantile(scoresWide$zhimaScore, probs=bands, na.rm=T)
