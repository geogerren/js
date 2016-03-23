emailProjectidMapping <- aeq("select 
  pd.financingprojectid,
  pd.createtime,
  em.val as email
  FROM
  project_detail pd
  join
  (
  select sd.val,service_id 
  from t_cust_ser_data sd where sd.data_source='jisudai' 
  and sd.class_1='CCEmailAddress' and is_delete= 0
  ) em
  on pd.service_id = em.service_id
                   ")



##################################################################################
# 通过application table, projectdetails table连上邮箱，解析信用卡资料
library(jsonlite)

basePath<-paste0(boxdata, "ccData/")
folderNames<-list.files(basePath)

billBasix<-data.frame()
billTrxns<-data.frame()

for (folder in folderNames){

  billFiles<-list.files(paste0(basePath, folder))

  EmailID<-folder
  print(EmailID)
  
  for (bill in billFiles){
    # Match ID
    BillAcctID<-substr(bill, 1, str_locate(bill, ".json")-1)
    print(BillAcctID)
    
    ############################################################################    
    # Bill Details
    detailbill<-jsonlite::fromJSON(paste0(basePath, folder, "/", bill)) 
    
    ###########################################
    ##################### 账单汇总及额度等信息，每行是一个邮箱的一个账单
    print("汇总信息")
    # 卡片及额度信息
    # Name                <-detailbill$Name
    # Card_Num            <-detailbill$Card_Num
    Cash_Advance_Limit  <-ifelse(is.null(detailbill$Cash_Advance_Limit), NA, detailbill$Cash_Advance_Limit)
    Credit_Limit        <-ifelse(is.null(detailbill$Credit_Limit), NA, detailbill$Credit_Limit)
    # Payment_Date        <-detailbill$Payment_Date
    Credit_Line         <-ifelse(is.null(detailbill$Credit_Line), NA, detailbill$Credit_Line)
    Statement_Date      <-ifelse(is.null(detailbill$Statement_Date), NA, detailbill$Statement_Date)
    
    ######### 账单信息
    # Bill_Date           <-detailbill$Current_Bill$Bill_Date
    
    # 人民币
    Current_Bill_RMB    <-detailbill$Current_Bill$RMB_Count
    # Minimum_Payment_rmb <-Current_Bill_RMB[1]$Minimum_Payment
    Current_Balance_RMB <-ifelse(is.null(Current_Bill_RMB$Current_Balance), NA, Current_Bill_RMB$Current_Balance)
    New_Balance_RMB     <-ifelse(is.null(Current_Bill_RMB$New_Balance), NA, Current_Bill_RMB$New_Balance)
    Previous_Balance_RMB<-ifelse(is.null(Current_Bill_RMB$Previous_Balance), NA, Current_Bill_RMB$Previous_Balance)
    Last_Payment_RMB    <-ifelse(is.null(Current_Bill_RMB$Last_Payment), NA, Current_Bill_RMB$Last_Payment)

    # 美元
    Current_Bill_USD    <-detailbill$Current_Bill$USD_Count
    # Minimum_Payment_USD <-Current_Bill_USD[1]$Minimum_Payment
    Current_Balance_USD <-ifelse(is.null(Current_Bill_USD$Current_Balance), NA, Current_Bill_USD$Current_Balance)
    New_Balance_USD     <-ifelse(is.null(Current_Bill_USD$New_Balance), NA, Current_Bill_USD$New_Balance)
    Previous_Balance_USD<-ifelse(is.null(Current_Bill_USD$Previous_Balance), NA, Current_Bill_USD$Previous_Balance)
    Last_Payment_USD    <-ifelse(is.null(Current_Bill_USD$Last_Payment), NA, Current_Bill_USD$Last_Payment)

    ######### 积分信息
    Points_Previous     <-ifelse(is.null(detailbill$Points_Info$Previous_Points), NA, detailbill$Points_Info$Previous_Points)
    Points_Current      <-ifelse(is.null(detailbill$Points_Info$Ending_Points), NA, detailbill$Points_Info$Ending_Points)
    
    newBill<-data.frame(EmailID
      ,BillAcctID
      ,Name
      ,Cash_Advance_Limit
      ,Credit_Limit
      ,Credit_Line
      ,Statement_Date
      ,Current_Balance_RMB
      ,New_Balance_RMB
      ,Previous_Balance_RMB
      ,Last_Payment_RMB
      ,Current_Balance_USD
      ,New_Balance_USD
      ,Previous_Balance_USD
      ,Last_Payment_USD
      ,Points_Previous
      ,Points_Current
    )
    billBasix<-rbind(billBasix,newBill)  #set together

    ###########################################
    ##################### 详细交易信息，每行是一个邮箱一个账单的一笔交易，与上面的表用EmailID和BillAcctID去match
    print("交易详情")
    trxnList <- data.table(detailbill$Bill_Detail)
    if(nrow(trxnList)>0){
      trxnList <- trxnList[, c("Trans_Date", "Trans_Amt", "Description", "Account_Type"), with=F]
      trxnList[, EmailID:=EmailID] 
      trxnList[, BillAcctID:=BillAcctID] 
      billTrxns<- rbind(billTrxns, trxnList)
    }

  }

   # dmbilldata<-rbind(dmbilldata,billdata)
}



print(dmbilldata)






dmbilldata2<-within(dmbilldata,{
  CreditLimit<-gsub(pattern="CNY|??",replacement="",dmbilldata$Credit_Limit)  #?滻?쳣?ַ?
  CashAdvanceLimit<-gsub(pattern="CNY|??",replacement="",dmbilldata$Cash_Advance_Limit)
  MinimumPayment_rmb<-gsub(pattern="RMB:",replacement="",dmbilldata$Minimum_Payment_rmb)
  Name2<-gsub(pattern="<U+00A0><U+00A0>",replacement="",dmbilldata$Name)
  
}
)

# CreditLimit<-gsub(pattern="??|CNY",replacement="",dmbilldata$Credit_Limit)
# CreditAdvanceLimit<-gsub(pattern="??|CNY",replacement="",dmbilldata$Cash_Advance_Limit)
# Name2<-gsub(pattern="<U+00A0><U+00A0>",replacement="",dmbilldata$Name)
# 
# dmbilldata3<-data.frame(dmbilldata2,CreditLimit,CreditAdvanceLimit,Name2)



write.csv(dmbilldata2,file="D:/1WorkDoc/3Project/P011_CreditBills/dumiao_ccbills_20160112.csv",row.names=T)


#write.table(dmbilldata,file="D:/1WorkDoc/3Project/P011_CreditBills/parse_results/dumiao_ccbills.xls",quote=T,row.names=T)
