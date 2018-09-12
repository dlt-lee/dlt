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
r8<-dlt.clu.xgb.XXIV(data,count,n)

table(c(r1[1],r2[1],r3[1],r4[1],r5[1],r6[1],r7[1],r8[1]))
table(c(r1[2],r2[2],r3[2],r4[2],r5[2],r6[2],r7[2],r8[1]))
table(c(r1[3],r2[3],r3[3],r4[3],r5[3],r6[3],r7[3],r8[1]))
table(c(r1[4],r2[4],r3[4],r4[4],r5[4],r6[4],r7[4],r8[1]))
table(c(r1[5],r2[5],r3[5],r4[5],r5[5],r6[5],r7[5],r8[1]))
table(c(r1[6],r2[6],r3[6],r4[6],r5[6],r6[6],r7[6],r8[1]))
table(c(r1[7],r2[7],r3[7],r4[7],r5[7],r6[7],r7[7],r8[1]))


r1
r2
r3
r4
r5
r6
r7
r8




