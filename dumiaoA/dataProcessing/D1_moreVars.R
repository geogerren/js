featuresWideU[, applyHour:=hour(createtime)]
featuresWideU[, applyTimeSegment:=ifelse(applyHour>=1 & applyHour<=6, "3", ifelse(applyHour>=9 & applyHour<=20, "1", "2"))]



