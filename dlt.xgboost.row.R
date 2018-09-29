data<-dlt
count<-dim(dlt)[1]
n<-300

ab.1.row <-dlt.xgboost.I(data,count,n)
ab.2.row <-dlt.xgboost.II(data,count,n)
ab.3.row <-dlt.xgboost.III(data,count,n)
ab.6.row <-dlt.xgboost.VI(data,count,n)
ab.9.row <-dlt.xgboost.IX(data,count,n)
ab.12.row<-dlt.xgboost.XII(data,count,n)
ab.15.row <-dlt.xgboost.XV(data,count,n)
ab.18.row<-dlt.xgboost.XVIII(data,count,n)
ab.21.row<-dlt.xgboost.XXI(data,count,n)
ab.24.row<-dlt.xgboost.XXIV(data,count,n)
ab.27.row<-dlt.xgboost.XXVII(data,count,n)
ab.30.row<-dlt.xgboost.XXX(data,count,n)
ab.33.row<-dlt.xgboost.XXXIII(data,count,n)
ab.36.row<-dlt.xgboost.XXXVI(data,count,n)
ab.39.row<-dlt.xgboost.XXXIX(data,count,n)
ab.42.row<-dlt.xgboost.XXXXII(data,count,n)


ab.result.a1.row<-c(tail(ab.1.row$a1,1),tail(ab.2.row$a1,1),tail(ab.3.row$a1,1),tail(ab.6.row$a1,1),
                tail(ab.9.row$a1,1),tail(ab.12.row$a1,1),tail(ab.15.row$a1,1),tail(ab.18.row$a1,1),
                tail(ab.21.row$a1,1),tail(ab.24.row$a1,1),tail(ab.27.row$a1,1),tail(ab.30.row$a1,1),
                tail(ab.33.row$a1,1),tail(ab.36.row$a1,1),tail(ab.39.row$a1,1),tail(ab.42.row$a1,1))

ab.result.a2.row<-c(tail(ab.1.row$a2,1),tail(ab.2.row$a2,1),tail(ab.3.row$a2,1),tail(ab.6.row$a2,1),
                tail(ab.9.row$a2,1),tail(ab.12.row$a2,1),tail(ab.15.row$a2,1),tail(ab.18.row$a2,1),
                tail(ab.21.row$a2,1),tail(ab.24.row$a2,1),tail(ab.27.row$a2,1),tail(ab.30.row$a2,1),
                tail(ab.33.row$a2,1),tail(ab.36.row$a2,1),tail(ab.39.row$a2,1),tail(ab.42.row$a2,1))

ab.result.a3.row<-c(tail(ab.1.row$a3,1),tail(ab.2.row$a3,1),tail(ab.3.row$a3,1),tail(ab.6.row$a3,1),
                tail(ab.9.row$a3,1),tail(ab.12.row$a3,1),tail(ab.15.row$a3,1),tail(ab.18.row$a3,1),
                tail(ab.21.row$a3,1),tail(ab.24.row$a3,1),tail(ab.27.row$a3,1),tail(ab.30.row$a3,1),
                tail(ab.33.row$a3,1),tail(ab.36.row$a3,1),tail(ab.39.row$a3,1),tail(ab.42.row$a3,1))

ab.result.a4.row<-c(tail(ab.1.row$a4,1),tail(ab.2.row$a4,1),tail(ab.3.row$a4,1),tail(ab.6.row$a4,1),
                tail(ab.9.row$a4,1),tail(ab.12.row$a4,1),tail(ab.15.row$a4,1),tail(ab.18.row$a4,1),
                tail(ab.21.row$a4,1),tail(ab.24.row$a4,1),tail(ab.27.row$a4,1),tail(ab.30.row$a4,1),
                tail(ab.33.row$a4,1),tail(ab.36.row$a4,1),tail(ab.39.row$a4,1),tail(ab.42.row$a4,1))

ab.result.a5.row<-c(tail(ab.1.row$a5,1),tail(ab.2.row$a5,1),tail(ab.3.row$a5,1),tail(ab.6.row$a5,1),
                tail(ab.9.row$a5,1),tail(ab.12.row$a5,1),tail(ab.15.row$a5,1),tail(ab.18.row$a5,1),
                tail(ab.21.row$a5,1),tail(ab.24.row$a5,1),tail(ab.27.row$a5,1),tail(ab.30.row$a5,1),
                tail(ab.33.row$a5,1),tail(ab.36.row$a5,1),tail(ab.39.row$a5,1),tail(ab.42.row$a5,1))

ab.result.b1.row<-c(tail(ab.1.row$b1,1),tail(ab.2.row$b1,1),tail(ab.3.row$b1,1),tail(ab.6.row$b1,1),
                tail(ab.9.row$b1,1),tail(ab.12.row$b1,1),tail(ab.15.row$b1,1),tail(ab.18.row$b1,1),
                tail(ab.21.row$b1,1),tail(ab.24.row$b1,1),tail(ab.27.row$b1,1),tail(ab.30.row$b1,1),
                tail(ab.33.row$b1,1),tail(ab.36.row$b1,1),tail(ab.39.row$b1,1),tail(ab.42.row$b1,1))

ab.result.b2.row<-c(tail(ab.1.row$b2,1),tail(ab.2.row$b2,1),tail(ab.3.row$b2,1),tail(ab.6.row$b2,1),
                tail(ab.9.row$b2,1),tail(ab.12.row$b2,1),tail(ab.15.row$b2,1),tail(ab.18.row$b2,1),
                tail(ab.21.row$b2,1),tail(ab.24.row$b2,1),tail(ab.27.row$b2,1),tail(ab.30.row$b2,1),
                tail(ab.33.row$b2,1),tail(ab.36.row$b2,1),tail(ab.39.row$b2,1),tail(ab.42.row$b2,1))



ab.result.row<-data.frame(ab.result.a1.row,ab.result.a2.row,ab.result.a3.row,ab.result.a4.row,
                      ab.result.a5.row,ab.result.b1.row,ab.result.b2.row)


sort(table(ab.result.a1.row[1:9]))
sort(table(ab.result.a2.row[1:9]))
sort(table(ab.result.a3.row[1:9]))
sort(table(ab.result.a4.row[1:9]))
sort(table(ab.result.a5.row[1:9]))
sort(table(ab.result.b1.row[1:9]))
sort(table(ab.result.b2.row[1:9]))

sort(table(ab.result.a1.row[10:16]))
sort(table(ab.result.a2.row[10:16]))
sort(table(ab.result.a3.row[10:16]))
sort(table(ab.result.a4.row[10:16]))
sort(table(ab.result.a5.row[10:16]))
sort(table(ab.result.b1.row[10:16]))
sort(table(ab.result.b2.row[10:16]))



ab.result



