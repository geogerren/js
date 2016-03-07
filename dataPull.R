applications <- jmq("select * from dumiao_analysis.dwh_application limit 100")

library(randomForest)
library(pmml)
iris.rf <- randomForest(Species ~ ., data=iris, ntree=20)
# Convert to pmml
pmml(iris.rf)
rm(iris.rf)


plot.roc(aSAH$outcome, aSAH$s100b)

