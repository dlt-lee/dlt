
data.for.xgb.clu<-for.clu.xgb()
data.for.xgb.row<-for.row.xgb()
data.xgb.clu.clu<-dlt.xgb.clu.clu(data.for.xgb.clu)
data.xgb.row.clu<-dlt.xgb.row.clu(data.for.xgb.row)
data.xgb.clu.row<-dlt.xgb.clu.row(data.for.xgb.clu)
data.xgb.row.row<-dlt.xgb.row.row(data.for.xgb.row)
data.xgb.clu<-dlt.xgboost.clu(dlt)
data.xgb.row<-dlt.xgboost.row(dlt)



#clu:
sort(table(data.xgb.clu$a1.clu))
sort(table(data.xgb.clu$a2.clu))
sort(table(data.xgb.clu$a3.clu))
sort(table(data.xgb.clu$a4.clu))
sort(table(data.xgb.clu$a5.clu))
sort(table(data.xgb.clu$b1.clu))
sort(table(data.xgb.clu$b2.clu))
#row:
sort(table(data.xgb.row$a1.row))
sort(table(data.xgb.row$a2.row))
sort(table(data.xgb.row$a3.row))
sort(table(data.xgb.row$a4.row))
sort(table(data.xgb.row$a5.row))
sort(table(data.xgb.row$b1.row))
sort(table(data.xgb.row$b2.row))


data.xgb.clu.clu
data.xgb.row.clu
data.xgb.clu.row
data.xgb.row.row





