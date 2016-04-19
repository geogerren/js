train <- copy(trainData)
# train[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
#           "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
#           "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
#       ]


autoBin<-woeAutoBin(train, "flgDPD", exclude = c("financingprojectid", "createtime"))
binningDF<-autoBin$woeTable
write.csv(binningDF, paste0(boxdata, "autoBin.csv"))


#####################################################################################################################
assigningDF <- data.frame()

RFM_12_var2_list<-woeCalc(train, "RFM_12_var2","flgDPD", binning=c(-Inf, 25, 75, Inf))
train <- RFM_12_var2_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var2_list$woeVar)

RFM_12_var55_list<-woeCalc(train, "RFM_12_var55","flgDPD", binning=c(-Inf, -999, 3, Inf), naZeroWoE=T)
train <- RFM_12_var55_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var55_list$woeVar)

RFM_1_var14_list<-woeCalc(train, "RFM_1_var14","flgDPD", binning=c(-Inf, -999, 0, Inf), naZeroWoE=T)
train <- RFM_1_var14_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var14_list$woeVar)


RFM_6_var12_list<-woeCalc(train, "RFM_6_var12","flgDPD", binning=c(-Inf, -999, 1, Inf), naZeroWoE=T)
train <- RFM_6_var12_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var12_list$woeVar)


RFM_6_var15_list<-woeCalc(train, "RFM_6_var15","flgDPD", binning=c(-Inf, -999, 1, Inf), naZeroWoE=T)
train <- RFM_6_var15_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var15_list$woeVar)


age_Derived_list<-woeCalc(train, "age","flgDPD", binning=c(-Inf, 24, 29, Inf))
train <- age_Derived_list$resultDT
assigningDF <- rbind(assigningDF, age_Derived_list$woeVar)

amuseConsumeFreq_Derived_list<-woeCalc(train, "amuseConsumeFreq","flgDPD", binning=c(-Inf, -999, 0, Inf), naZeroWoE=T)
train <- amuseConsumeFreq_Derived_list$resultDT
assigningDF <- rbind(assigningDF, amuseConsumeFreq_Derived_list$woeVar)

applyTimeSegment_list<-woeCalc(train, "applyTimeSegment","flgDPD")
train <- applyTimeSegment_list$resultDT
assigningDF <- rbind(assigningDF, applyTimeSegment_list$woeVar)

avgMonthCall_list<-woeCalc(train, "avgMonthCall","flgDPD", binning = c(-Inf, 150, 320, Inf))
train <- avgMonthCall_list$resultDT
assigningDF <- rbind(assigningDF, avgMonthCall_list$woeVar)

cardType_binningDF <- data.table(c("-99999","2","1","2","2"), c(-99999, 0, 1, 3, 4))
cardType_list<-woeCalc(train, "cardType","flgDPD", binning=cardType_binningDF, naZeroWoE=T)
train <- cardType_list$resultDT
assigningDF <- rbind(assigningDF, cardType_list$woeVar)

childrenNum_list<-woeCalc(train, "childrenNum","flgDPD")
train <- childrenNum_list$resultDT
assigningDF <- rbind(assigningDF, childrenNum_list$woeVar)

consumeFreg_list<-woeCalc(train, "consumeFreg","flgDPD", binning = c(-Inf, 2, 4, 10, Inf))
train <- consumeFreg_list$resultDT
assigningDF <- rbind(assigningDF, consumeFreg_list$woeVar)

consumeLineRate_list<-woeCalc(train, "consumeLineRate","flgDPD", binning = c(-Inf, -0.04, 0, Inf))
train <- consumeLineRate_list$resultDT
assigningDF <- rbind(assigningDF, consumeLineRate_list$woeVar)

creditCashFreq_list<-woeCalc(train, "creditCashFreq","flgDPD", binning = c(-Inf, -999, 2, Inf), naZeroWoE=T)
train <- creditCashFreq_list$resultDT
assigningDF <- rbind(assigningDF, creditCashFreq_list$woeVar)

creditWD3Months_list<-woeCalc(train, "creditWD3Months","flgDPD", binning = c(-Inf, -999, 2, Inf), naZeroWoE=T)
train <- creditWD3Months_list$resultDT
assigningDF <- rbind(assigningDF, creditWD3Months_list$woeVar)

currentJobyear_list<-woeCalc(train, "currentJobyear","flgDPD", binning = c(-Inf, 0, 1, 2, Inf))
train <- currentJobyear_list$resultDT
assigningDF <- rbind(assigningDF, currentJobyear_list$woeVar)

# -1是程序异常
naBlankInfer(train, "goOut120", -1, -99999)
goOut120_list<-woeCalc(train, "goOut120","flgDPD", binning = c(-Inf, -999, 0, Inf), naZeroWoE=T)
train <- goOut120_list$resultDT
assigningDF <- rbind(assigningDF, goOut120_list$woeVar)

lastMonthOverdrawNum_list<-woeCalc(train, "lastMonthOverdrawNum","flgDPD", binning = c(-Inf, -999, 0, Inf), naZeroWoE=T)
train <- lastMonthOverdrawNum_list$resultDT
assigningDF <- rbind(assigningDF, lastMonthOverdrawNum_list$woeVar)

loansCalls3_list<-woeCalc(train, "loansCalls3","flgDPD", binning = c(-Inf, -999, 0, 6, Inf), naZeroWoE=T)
train <- loansCalls3_list$resultDT
assigningDF <- rbind(assigningDF, loansCalls3_list$woeVar)

# -1是程序异常
naBlankInfer(train, "localFriends", -1, -99999)
localFriends_list<-woeCalc(train, "localFriends","flgDPD", naZeroWoE=T)
train <- localFriends_list$resultDT
assigningDF <- rbind(assigningDF, localFriends_list$woeVar)

longTimeShutdown_list<-woeCalc(train, "longTimeShutdown","flgDPD", naZeroWoE=T)
train <- longTimeShutdown_list$resultDT
assigningDF <- rbind(assigningDF, longTimeShutdown_list$woeVar)

multiBorrowNumP6_list<-woeCalc(train, "multiBorrowNumP6","flgDPD", binning = c(-Inf, -999, 0, 2, Inf), naZeroWoE=T)
train <- multiBorrowNumP6_list$resultDT
assigningDF <- rbind(assigningDF, multiBorrowNumP6_list$woeVar)

postMonthConsumeFreg_list<-woeCalc(train, "postMonthConsumeFreg","flgDPD", binning = c(-Inf, 4, Inf), naZeroWoE=T)
train <- postMonthConsumeFreg_list$resultDT
assigningDF <- rbind(assigningDF, postMonthConsumeFreg_list$woeVar)

useCardAmountAvg_list<-woeCalc(train, "useCardAmountAvg","flgDPD", binning = c(-Inf, 2000, Inf), naZeroWoE=T)
train <- useCardAmountAvg_list$resultDT
assigningDF <- rbind(assigningDF, useCardAmountAvg_list$woeVar)

useCardLastTime_list<-woeCalc(train, "useCardLastTime","flgDPD", binning = c(-Inf, -999, 2, Inf), naZeroWoE=T)
train <- useCardLastTime_list$resultDT
assigningDF <- rbind(assigningDF, useCardLastTime_list$woeVar)

useCardNumPost6_list<-woeCalc(train, "useCardNumPost6","flgDPD", binning = c(-Inf, -999, 4, Inf), naZeroWoE=T)
train <- useCardNumPost6_list$resultDT
assigningDF <- rbind(assigningDF, useCardNumPost6_list$woeVar)

useCardPM_list<-woeCalc(train, "useCardPM","flgDPD", binning = c(-Inf, -999, 4, 6, Inf), naZeroWoE=T)
train <- useCardPM_list$resultDT
assigningDF <- rbind(assigningDF, useCardPM_list$woeVar)

useCardSumRank_list<-woeCalc(train, "useCardSumRank","flgDPD", binning = c(-Inf, -999, 50, Inf), naZeroWoE=T)
train <- useCardSumRank_list$resultDT
assigningDF <- rbind(assigningDF, useCardSumRank_list$woeVar)

zhiceCar_list<-woeCalc(train, "zhiceCar","flgDPD", binning = c(-Inf, -999, 0, Inf), naZeroWoE=T)
train <- zhiceCar_list$resultDT
assigningDF <- rbind(assigningDF, zhiceCar_list$woeVar)

zhiceHouse_list<-woeCalc(train, "zhiceHouse","flgDPD", binning = c(-Inf, -999, 0, Inf), naZeroWoE=T)
train <- zhiceHouse_list$resultDT
assigningDF <- rbind(assigningDF, zhiceHouse_list$woeVar)

zhimaScore_list<-woeCalc(train, "zhimaScore","flgDPD", binning = c(-Inf, 660, 700, Inf), naZeroWoE=T)
train <- zhimaScore_list$resultDT
assigningDF <- rbind(assigningDF, zhimaScore_list$woeVar)








write.csv(assigningDF, paste0(boxdata, "assigning.csv"))
###############################################################################################
# not run
# endproduct:
View(assigningDF)
train



