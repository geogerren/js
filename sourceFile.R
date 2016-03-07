# source("~/jimu/sourceFile.R")
box <- "E:/BaiduDropbox/jimu/"
library(RMySQL)
library(data.table)
library(reshape2) # for melt and cast
library(randomForest)
library(pmml)

# library(pROC)
# library(ROCR)
library(PRROC)
# Jimu data queries
drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="dumiao_analysis", password="analysis4321",
              dbname="dumiao_analysis", host="172.19.1.221", port=9900, encoding = getOption("utf8"))
jmq <- function(query) {
  dbGetQuery(con, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(con, query)
  data.table(resultDF)
}

