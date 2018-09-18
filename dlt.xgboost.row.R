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

i<-1
ab.result.a1<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])
i<-2
ab.result.a2<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])
i<-3
ab.result.a3<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])
i<-4
ab.result.a4<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])
i<-5
ab.result.a5<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])
i<-6
ab.result.b1<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])
i<-7
ab.result.b2<-c(tail(ab.1.row,1)[i],tail(ab.2.row,1)[i],tail(ab.3.row,1)[i],tail(ab.6.row,1)[i],
                tail(ab.9.row,1)[i],tail(ab.12.row,1)[i],tail(ab.15.row,1)[i],tail(ab.18.row,1)[i],
                tail(ab.21.row,1)[i],tail(ab.24.row,1)[i],tail(ab.27.row,1)[i],tail(ab.30.row,1)[i],
                tail(ab.33.row,1)[i],tail(ab.36.row,1)[i],tail(ab.39.row,1)[i],tail(ab.42.row,1)[i])



ab.result<-data.frame(ab.result.a1,ab.result.a2,ab.result.a3,ab.result.a4,ab.result.a5,ab.result.b1,ab.result.b2)

table(ab.result$a1)
table(ab.result.a2)
table(ab.result.a3)
table(ab.result.a4)
table(ab.result.a5)
table(ab.result.b1)
table(ab.result.b2)



