test <- copy(validateData)
test[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
          "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
          "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
      ]

test2 <- woeAssignAuto(test, assigningDF)

rawTest<-test2[, c(keepVarRaw,"flgDPD"), with=F]
binTest<-test2[, keepVar, with=F]

write.csv(rawTest, paste0(boxdata, "rawTest.csv"))
write.csv(binTest, paste0(boxdata, "binTest.csv"))

#####################################################
# not run
# endproduct:
testWoE
rawTest
binTest