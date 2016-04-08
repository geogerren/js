featuresWideU[, applyHour:=hour(createtime)]
featuresWideU[, applyTimeSegment:=ifelse(applyHour>=1 & applyHour<=6, "UnexpectedTime", ifelse(applyHour>=9 & applyHour<=20, "WorkHour", "NonWorkHour"))]



