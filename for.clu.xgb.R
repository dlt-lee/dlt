data<-dlt
count<-dim(dlt)[1]
n<-300

r1<-dlt.clu.xgb.III(data,count,n)
r2<-dlt.clu.xgb.VI(data,count,n)
r3<-dlt.clu.xgb.IX(data,count,n)
r4<-dlt.clu.xgb.XII(data,count,n)
r5<-dlt.clu.xgb.XV(data,count,n)
r6<-dlt.clu.xgb.XVIII(data,count,n)
r7<-dlt.clu.xgb.XXI(data,count,n)


r1
r2
r3
r4
r5
r6
r7
