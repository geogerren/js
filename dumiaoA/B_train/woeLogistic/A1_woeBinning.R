train <- copy(trainData)
train[, c("applicantContact", "addrUsedNum",           "browserUsedNum",        "called5",               "cellphoneAuth",         "ecpNum",
          "ecp_eachother",         "highZhimaScore",        "inBlanklist",           "inZhimaBlank",          "ipUsedNum",
          "mateNum",               "noNeedMobileAuthCheck",  "trustAddr",             "trustIP",               "unexpectedApplyTime" ):=NULL
      ]


autoBin<-woeAutoBin(train, "flgDPD", exclude = c("financingprojectid", "createtime"))
binningDF<-autoBin$woeTable
write.csv(binningDF, paste0(boxdata, "autoBin.csv"))


#####################################################################################################################
assigningDF <- data.frame()

FLAG_6_var11_list<-woeCalc(train, "FLAG_6_var11","flgDPD", binning=c(-Inf, -999, 2, Inf))
train <- FLAG_6_var11_list$resultDT
assigningDF <- rbind(assigningDF, FLAG_6_var11_list$woeVar)


FLAG_6_var12_list<-woeCalc(train, "FLAG_6_var12","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- FLAG_6_var12_list$resultDT
assigningDF <- rbind(assigningDF, FLAG_6_var12_list$woeVar)


MON_6_var1_list<-woeCalc(train, "MON_6_var1","flgDPD", binning=c(-Inf, 4, Inf))
train <- MON_6_var1_list$resultDT
assigningDF <- rbind(assigningDF, MON_6_var1_list$woeVar)


RFM_12_var2_list<-woeCalc(train, "RFM_12_var2","flgDPD", binning=c(-Inf, 20, 50, Inf))
train <- RFM_12_var2_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var2_list$woeVar)


RFM_12_var29_list<-woeCalc(train, "RFM_12_var29","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_12_var29_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var29_list$woeVar)


RFM_12_var55_list<-woeCalc(train, "RFM_12_var55","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_12_var55_list$resultDT
assigningDF <- rbind(assigningDF, RFM_12_var55_list$woeVar)


RFM_1_var1_list<-woeCalc(train, "RFM_1_var1","flgDPD", binning=c(-Inf, -999, 3, Inf))
train <- RFM_1_var1_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var1_list$woeVar)


RFM_1_var14_list<-woeCalc(train, "RFM_1_var14","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_1_var14_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var14_list$woeVar)


RFM_1_var4_list<-woeCalc(train, "RFM_1_var4","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_1_var4_list$resultDT
assigningDF <- rbind(assigningDF, RFM_1_var4_list$woeVar)


RFM_6_var12_list<-woeCalc(train, "RFM_6_var12","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_6_var12_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var12_list$woeVar)


RFM_6_var2_list<-woeCalc(train, "RFM_6_var2","flgDPD", binning=c(-Inf, 10, Inf))
train <- RFM_6_var2_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var2_list$woeVar)


RFM_6_var20_list<-woeCalc(train, "RFM_6_var20","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_6_var20_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var20_list$woeVar)


RFM_6_var21_list<-woeCalc(train, "RFM_6_var21","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- RFM_6_var21_list$resultDT
assigningDF <- rbind(assigningDF, RFM_6_var21_list$woeVar)


RFM_h_var2_Derived_list<-woeCalc(train, "RFM_h_var2_Derived","flgDPD", binning=c(-Inf, -999, 1, 10, Inf))
train <- RFM_h_var2_Derived_list$resultDT
assigningDF <- rbind(assigningDF, RFM_h_var2_Derived_list$woeVar)


age_Derived_list<-woeCalc(train, "age","flgDPD", binning=c(-Inf, 27, Inf))
train <- age_Derived_list$resultDT
assigningDF <- rbind(assigningDF, age_Derived_list$woeVar)


applyTimeSegment_binningDF <- data.table(c("1","1","2"), c(1,2,3))
applyTimeSegment_list<-woeCalc(train, "applyTimeSegment","flgDPD", binning=applyTimeSegment_binningDF)
train <- applyTimeSegment_list$resultDT
assigningDF <- rbind(assigningDF, applyTimeSegment_list$woeVar)


longTimeShutdown_list<-woeCalc(train, "longTimeShutdown","flgDPD")
train <- longTimeShutdown_list$resultDT
assigningDF <- rbind(assigningDF, longTimeShutdown_list$woeVar)


marry_list<-woeCalc(train, "marry","flgDPD")
train <- marry_list$resultDT
assigningDF <- rbind(assigningDF, marry_list$woeVar)


rsk_score_list<-woeCalc(train, "rsk_score","flgDPD", binning=c(-Inf, -999, 620, Inf))
train <- rsk_score_list$resultDT
assigningDF <- rbind(assigningDF, rsk_score_list$woeVar)


sex_list<-woeCalc(train, "sex","flgDPD")
train <- sex_list$resultDT
assigningDF <- rbind(assigningDF, sex_list$woeVar)


zhimaScore_list<-woeCalc(train, "zhimaScore","flgDPD", binning=c(-Inf, 660, 680, Inf))
train <- zhimaScore_list$resultDT
assigningDF <- rbind(assigningDF, zhimaScore_list$woeVar)


card_tp_binningDF <- data.table(c("-99999","1","1","2","3"), c(-99999, 0, 1, 3, 4))
card_tp_list<-woeCalc(train, "card_tp","flgDPD", binning=card_tp_binningDF)
train <- card_tp_list$resultDT
assigningDF <- rbind(assigningDF, card_tp_list$woeVar)


currentJobyear_list<-woeCalc(train, "currentJobyear","flgDPD", binning=c(-Inf, 0, 2, Inf))
train <- currentJobyear_list$resultDT
assigningDF <- rbind(assigningDF, currentJobyear_list$woeVar)


loansCalls1_list<-woeCalc(train, "loansCalls1","flgDPD", binning=c(-Inf, -999, 0, Inf))
train <- loansCalls1_list$resultDT
assigningDF <- rbind(assigningDF, loansCalls1_list$woeVar)


localFriends_list_binningDF <- data.table(c("-99999","-99999","0","1"), c(-99999, -1, 0, 1))
localFriends_list<-woeCalc(train, "localFriends","flgDPD", binning=localFriends_list_binningDF)
train <- localFriends_list$resultDT
assigningDF <- rbind(assigningDF, localFriends_list$woeVar)

tongdunMultiLoanNum_list<-woeCalc(train, "tongdunMultiLoanNum","flgDPD")
train <- tongdunMultiLoanNum_list$resultDT
assigningDF <- rbind(assigningDF, tongdunMultiLoanNum_list$woeVar)


write.csv(assigningDF, paste0(boxdata, "assigning.csv"))
###############################################################################################
# not run
# endproduct:
View(assigningDF)
train



