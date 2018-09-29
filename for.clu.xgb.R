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
rab.42.clu14<-dlt.clu.xgb.XXXXII(data,count,n)


ab.result.a1.clu<-c(tail(ab.3.clu$a1,1))

sort(table(c(r8[1],r9[1],r10[1],r11[1],r12[1],r13[1],r14[1])))
sort(table(c(r8[2],r9[2],r10[2],r11[2],r12[2],r13[2],r14[2])))
sort(table(c(r8[3],r9[3],r10[3],r11[3],r12[3],r13[3],r14[3])))
sort(table(c(r8[4],r9[4],r10[4],r11[4],r12[4],r13[4],r14[4])))
sort(table(c(r8[5],r9[5],r10[5],r11[5],r12[5],r13[5],r14[5])))
sort(table(c(r8[6],r9[6],r10[6],r11[6],r12[6],r13[6],r14[6])))
sort(table(c(r8[7],r9[7],r10[7],r11[7],r12[7],r13[7],r14[7])))


r1
r2
r3
r4
r5
r6
r7
r8
r9
r10
r11
r12
r13
r14




