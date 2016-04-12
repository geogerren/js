keepVarRaw <- unique(assigningDF$varName)
keepVarRaw <- keepVarRaw[!(keepVarRaw %in% corrRemoveList)]
keepVar <- c(paste0("w_", keepVarRaw), "flgDPD")

trainWoE <- train[, keepVar, with=F]

###############################################################################

logitWoE<-glm(flgDPD~., data=trainWoE, family=binomial())

testBin$score<-predict(logitWoE, type='response', testWoE)

WoEcurve<-roc(testWoE$flgDPD, testWoE$score)
plot(WoEcurve)
# auc(WoEcurve)
# 0.6162

plot(fitted(logitWoE), residuals(logitWoE),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(logitWoE), residuals(logitWoE)))

HMeasure(testBin$flgDPD, testBin$score, threshold = 0.90)$metrics


