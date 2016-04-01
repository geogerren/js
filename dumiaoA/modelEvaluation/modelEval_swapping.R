# Pull population features








scores<-ruleq("select 
p.financingprojectid, 
                t.mod_id,
                t.index_id,
                t.rule_id,
                t.input_val
                from t_mod_score t
                ")

featureGen(scores, 10000, "1000031.2", "finalScore")


scoresWide<-dcast.data.table(scores, financingprojectid ~ key, fun.agg=max, value.var = "value")
scoresWide<-merge(scoresWide, target, by.x="financingprojectid", by.y="project_id")

scoresWide[, modelScore:=as.numeric(modelScore)]
banding(scoresWide, "modelScore", "modelScoreBand")

scoresWide[, zhimaScore:=as.numeric(zhimaScore)]
banding(scoresWide, "zhimaScore", "zhimaScoreBand")

modelScoreAnalysis<-scoresWide[, .("badCnt"=sum(ODFlag), "totalCnt"=.N), by="modelScoreBand"]
zhimaScoreAnalysis<-scoresWide[, .("badCnt"=sum(ODFlag), "totalCnt"=.N), by="zhimaScoreBand"]

quantile(scoresWide$zhimaScore, probs=bands, na.rm=T)

