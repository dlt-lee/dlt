
rows<-dim(dlt)[1]
p.data<-matrix(0,nrow = rows-200,ncol = 15)
j<-0
for (i in rows:201) {
  dlt.data<-tail(dlt,i)[1:201,]
  p<-dlt.f.mclust(dlt.data)
  
  j<-j+1
  
  n.a1<-p$nm.a1
  p.a1<-p$p.a1
  p.a1<-data.frame(n.a1,p.a1)
  
  n.a2<-p$nm.a2
  p.a2<-p$p.a2
  p.a2<-data.frame(n.a2,p.a2)
  
  n.a3<-p$nm.a3
  p.a3<-p$p.a3
  p.a3<-data.frame(n.a3,p.a3)
  
  n.a4<-p$nm.a4
  p.a4<-p$p.a4
  p.a4<-data.frame(n.a4,p.a4)
  
  n.a5<-p$nm.a5
  p.a5<-p$p.a5
  p.a5<-data.frame(n.a5,p.a5)
  
  n.b1<-p$nm.b1
  p.b1<-p$p.b1
  p.b1<-data.frame(n.b1,p.b1)
  
  n.b2<-p$nm.b2
  p.b2<-p$p.b2
  p.b2<-data.frame(n.b2,p.b2)
  
  dlt.n<-tail(dlt.data,1)$n
  dlt.a1<-tail(dlt.data,1)$a1
  dlt.a2<-tail(dlt.data,1)$a2
  dlt.a3<-tail(dlt.data,1)$a3
  dlt.a4<-tail(dlt.data,1)$a4
  dlt.a5<-tail(dlt.data,1)$a5
  dlt.b1<-tail(dlt.data,1)$b1
  dlt.b2<-tail(dlt.data,1)$b2
  
  p.data[j, 1]<-dlt.n
  
  p.data[j, 2]<-p.a1[n.a1==dlt.a1,]$p.a1
  p.data[j, 4]<-p.a2[n.a2==dlt.a2,]$p.a2
  p.data[j, 6]<-p.a3[n.a3==dlt.a3,]$p.a3
  p.data[j, 8]<-p.a4[n.a4==dlt.a4,]$p.a4
  p.data[j,10]<-p.a5[n.a5==dlt.a5,]$p.a5
  p.data[j,12]<-p.b1[n.b1==dlt.b1,]$p.b1
  p.data[j,14]<-p.b2[n.b2==dlt.b2,]$p.b2
  
  p.data[j, 3]<-p.a1[n.a1==dlt.a1,]$p.a1-max(p.a1$p.a1)
  p.data[j, 5]<-p.a2[n.a2==dlt.a2,]$p.a2-max(p.a2$p.a2)
  p.data[j, 7]<-p.a3[n.a3==dlt.a3,]$p.a3-max(p.a3$p.a3)
  p.data[j, 9]<-p.a4[n.a4==dlt.a4,]$p.a4-max(p.a4$p.a4)
  p.data[j,11]<-p.a5[n.a5==dlt.a5,]$p.a5-max(p.a5$p.a5)
  p.data[j,13]<-p.b1[n.b1==dlt.b1,]$p.b1-max(p.b1$p.b1)
  p.data[j,15]<-p.b2[n.b2==dlt.b2,]$p.b2-max(p.b2$p.b2)
  
}

n<-p.data[,1]
p.a1<-p.data[, 2]
d.a1<-p.data[, 3]
p.a2<-p.data[, 4]
d.a2<-p.data[, 5]
p.a3<-p.data[, 6]
d.a3<-p.data[, 7]
p.a4<-p.data[, 8]
d.a4<-p.data[, 9]
p.a5<-p.data[,10]
d.a5<-p.data[,11]
p.b1<-p.data[,12]
d.b1<-p.data[,13]
p.b2<-p.data[,14]
d.b2<-p.data[,15]

pd.data<-data.frame(n,p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2,d.a1,d.a2,d.a3,d.a4,d.a5,d.b1,d.b2)





