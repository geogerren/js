unionTrxn<-ruleq("select pd.financingprojectid
                  ,tx.settledate
                 ,tx.transcurrcode
                 ,tx.acptresponsecode
                 ,tx.transreceipts
                 ,tx.transexpenses
                 ,tx.transType
                 from t_cust_ser_trans_detail tx
                 join project_detail pd
                 on tx.service_id = pd.service_id
                 ")

##################################################################################
# 通过cust_ser_trans table重建银联变量
