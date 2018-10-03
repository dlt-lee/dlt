data<-dlt
count<-dim(dlt)[1]
n<-300

ab.3.clu <-dlt.clu.xgb.III(data,count,n)
ab.6.clu <-dlt.clu.xgb.VI(data,count,n)
ab.9.clu <-dlt.clu.xgb.IX(data,count,n)
ab.12.clu <-dlt.clu.xgb.XII(data,count,n)
ab.15.clu <-dlt.clu.xgb.XV(data,count,n)
ab.18.clu <-dlt.clu.xgb.XVIII(data,count,n)
ab.21.clu <-dlt.clu.xgb.XXI(data,count,n)
ab.24.clu <-dlt.clu.xgb.XXIV(data,count,n)
ab.27.clu <-dlt.clu.xgb.XXVII(data,count,n)
ab.30.clu<-dlt.clu.xgb.XXX(data,count,n)
ab.33.clu<-dlt.clu.xgb.XXXIII(data,count,n)
ab.36.clu<-dlt.clu.xgb.XXXVI(data,count,n)
ab.39.clu<-dlt.clu.xgb.XXXIX(data,count,n)
ab.42.clu<-dlt.clu.xgb.XXXXII(data,count,n)


a1.clu<-c(tail(ab.3.clu$a1,1),tail(ab.6.clu$a1,1),tail(ab.9.clu$a1,1),
                    tail(ab.12.clu$a1,1),tail(ab.15.clu$a1,1),tail(ab.18.clu$a1,1),
                    tail(ab.21.clu$a1,1),tail(ab.24.clu$a1,1),tail(ab.27.clu$a1,1),
                    tail(ab.30.clu$a1,1),tail(ab.33.clu$a1,1),tail(ab.36.clu$a1,1),
                    tail(ab.39.clu$a1,1),tail(ab.42.clu$a1,1))
a2.clu<-c(tail(ab.3.clu$a2,1),tail(ab.6.clu$a2,1),tail(ab.9.clu$a2,1),
                    tail(ab.12.clu$a2,1),tail(ab.15.clu$a2,1),tail(ab.18.clu$a2,1),
                    tail(ab.21.clu$a2,1),tail(ab.24.clu$a2,1),tail(ab.27.clu$a2,1),
                    tail(ab.30.clu$a2,1),tail(ab.33.clu$a2,1),tail(ab.36.clu$a2,1),
                    tail(ab.39.clu$a2,1),tail(ab.42.clu$a2,1))
a3.clu<-c(tail(ab.3.clu$a3,1),tail(ab.6.clu$a3,1),tail(ab.9.clu$a3,1),
                    tail(ab.12.clu$a3,1),tail(ab.15.clu$a3,1),tail(ab.18.clu$a3,1),
                    tail(ab.21.clu$a3,1),tail(ab.24.clu$a3,1),tail(ab.27.clu$a3,1),
                    tail(ab.30.clu$a3,1),tail(ab.33.clu$a3,1),tail(ab.36.clu$a3,1),
                    tail(ab.39.clu$a3,1),tail(ab.42.clu$a3,1))
a4.clu<-c(tail(ab.3.clu$a4,1),tail(ab.6.clu$a4,1),tail(ab.9.clu$a4,1),
                    tail(ab.12.clu$a4,1),tail(ab.15.clu$a4,1),tail(ab.18.clu$a4,1),
                    tail(ab.21.clu$a4,1),tail(ab.24.clu$a4,1),tail(ab.27.clu$a4,1),
                    tail(ab.30.clu$a4,1),tail(ab.33.clu$a4,1),tail(ab.36.clu$a4,1),
                    tail(ab.39.clu$a4,1),tail(ab.42.clu$a4,1))
a5.clu<-c(tail(ab.3.clu$a5,1),tail(ab.6.clu$a5,1),tail(ab.9.clu$a5,1),
                    tail(ab.12.clu$a5,1),tail(ab.15.clu$a5,1),tail(ab.18.clu$a5,1),
                    tail(ab.21.clu$a5,1),tail(ab.24.clu$a5,1),tail(ab.27.clu$a5,1),
                    tail(ab.30.clu$a5,1),tail(ab.33.clu$a5,1),tail(ab.36.clu$a5,1),
                    tail(ab.39.clu$a5,1),tail(ab.42.clu$a5,1))
b1.clu<-c(tail(ab.3.clu$b1,1),tail(ab.6.clu$b1,1),tail(ab.9.clu$b1,1),
                    tail(ab.12.clu$b1,1),tail(ab.15.clu$b1,1),tail(ab.18.clu$b1,1),
                    tail(ab.21.clu$b1,1),tail(ab.24.clu$b1,1),tail(ab.27.clu$b1,1),
                    tail(ab.30.clu$b1,1),tail(ab.33.clu$b1,1),tail(ab.36.clu$b1,1),
                    tail(ab.39.clu$b1,1),tail(ab.42.clu$b1,1))
b2.clu<-c(tail(ab.3.clu$b2,1),tail(ab.6.clu$b2,1),tail(ab.9.clu$b2,1),
                    tail(ab.12.clu$b2,1),tail(ab.15.clu$b2,1),tail(ab.18.clu$b2,1),
                    tail(ab.21.clu$b2,1),tail(ab.24.clu$b2,1),tail(ab.27.clu$b2,1),
                    tail(ab.30.clu$b2,1),tail(ab.33.clu$b2,1),tail(ab.36.clu$b2,1),
                    tail(ab.39.clu$b2,1),tail(ab.42.clu$b2,1))


ab.result.clu<-data.frame(a1.clu,a2.clu,a3.clu,a4.clu,a5.clu,b1.clu,b2.clu)

sort(table(ab.result.clu$a1.clu))
sort(table(ab.result.clu$a2.clu))
sort(table(ab.result.clu$a3.clu))
sort(table(ab.result.clu$a4.clu))
sort(table(ab.result.clu$a5.clu))
sort(table(ab.result.clu$b1.clu))
sort(table(ab.result.clu$b2.clu))



ab.result.clu




