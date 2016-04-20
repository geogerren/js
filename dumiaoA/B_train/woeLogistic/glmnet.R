glmnetX <- copy(binTrain)
glmnetX[, flgDPD:=NULL]
glmnetX<-as.matrix(glmnetX)

glmnetY <- binTrain$flgDPD

glmnetCV <- cv.glmnet(glmnetX, glmnetY, type.measure = "auc", family="binomial")

glmnetCV$glmnet.fit$beta[, which(glmnetCV$lambda == glmnetCV$lambda.1se)]


glmnetTestX <- copy(binTest)
glmnetTestX[, flgDPD:=NULL]
glmnetTestX<-as.matrix(glmnetTestX)

glmnetTestY <- binTest$flgDPD

glmnetTestScore <- predict(glmnetCV, newx = glmnetTestX, s=glmnetCV$lambda.1se)

ks_stat(as.numeric(as.character(glmnetTestY)), glmnetTestScore)
