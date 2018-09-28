data<-dlt
count<-dim(dlt)[1]
n<-300

r1 <-dlt.clu.xgb.III(data,count,n)
r2 <-dlt.clu.xgb.VI(data,count,n)
r3 <-dlt.clu.xgb.IX(data,count,n)
r4 <-dlt.clu.xgb.XII(data,count,n)
r5 <-dlt.clu.xgb.XV(data,count,n)
r6 <-dlt.clu.xgb.XVIII(data,count,n)
r7 <-dlt.clu.xgb.XXI(data,count,n)
r8 <-dlt.clu.xgb.XXIV(data,count,n)
r9 <-dlt.clu.xgb.XXVII(data,count,n)
r10<-dlt.clu.xgb.XXX(data,count,n)
r11<-dlt.clu.xgb.XXXIII(data,count,n)
r12<-dlt.clu.xgb.XXXVI(data,count,n)
r13<-dlt.clu.xgb.XXXIX(data,count,n)
r14<-dlt.clu.xgb.XXXXII(data,count,n)

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




