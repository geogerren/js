MISSING_DEFAULT <- -99999
source("./generalFunction.R")
box <- "E:/Seafiles/Jimu/Data/adhoc/"
boxdata <- "E:/Seafiles/Jimu/Data/"

# general
library(RMySQL)
library(data.table)
# library(reshape2) # for melt and cast
library(pmml)
library(stringr)
library(pROC)
library(ROCR)
library(PRROC)
library(caret)
options(sqldf.driver="SQLite")
# rf
library(randomForest)
library(varSelRF)

# logistic & elastic net
library(glmnet)
library(Information)
library(smbinning)

# Model validation
library(hmeasure)
library(InformationValue)

# connect with SAS
# library(sas7bdat)
library(foreign)
# library(SASxport)



all_cons <- dbListConnections(MySQL())
for(con in all_cons)
  dbDisconnect(con)

# Jimu data queries
drv <- dbDriver("MySQL")
aecon <- dbConnect(drv, user="dumiao_analysis", password="analysis4321",
              dbname="dumiao_analysis", host="172.19.1.221", port=9900, encoding = getOption("utf8"))
rulecon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                   dbname="rule_engineer", host="172.16.2.28", port=3311, encoding = getOption("utf8"))
dmcon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                     dbname="dumiao", host="172.16.2.28", port=3311, encoding = getOption("utf8"))

aeq <- function(query) {
  dbGetQuery(aecon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(aecon, query)
  data.table(resultDF)
}

ruleq <- function(query) {
  dbGetQuery(rulecon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(rulecon, query)
  data.table(resultDF)
}

dmq <- function(query) {
  dbGetQuery(rulecon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(dmcon, query)
  data.table(resultDF)
}

