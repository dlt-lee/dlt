row.ab<-dim(dlt)[1]
clu.ab<-dim(dlt)[2]
ab.p<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1)
s.n<-floor(row.ab/2)
for(i in s.n:row.ab-1) {
  print(c(i,row.ab))
  data<-head(dlt,i)
  result<-dlt[i+1,]
  count<-s.n
  ab.temp<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
             1,1,1,1,1,1,1,1,1,1,1,1,
             1,1,1)
  p.temp<-dlt.f.hmm(data,count)
  ab.temp[result$a1]    <-p.temp$p.a1[result$a1]
  ab.temp[result$a2]    <-p.temp$p.a2[result$a2]
  ab.temp[result$a3]    <-p.temp$p.a3[result$a3]
  ab.temp[result$a4]    <-p.temp$p.a4[result$a4]
  ab.temp[result$a5]    <-p.temp$p.a5[result$a5]
  ab.temp[result$b1+35] <-p.temp$p.b1[result$b1]
  ab.temp[result$b2+35] <-p.temp$p.b2[result$b2]
  ab.temp[48]<-p.temp$p.a1[result$a1]*p.temp$p.a2[result$a2]*p.temp$p.a3[result$a3]*p.temp$p.a4[result$a4]*p.temp$p.a5[result$a5]
  ab.temp[49]<-p.temp$p.b1[result$b1]*p.temp$p.b2[result$b2]
  ab.temp[50]<-ab.temp[48]*ab.temp[49]
  ab.p<-c(ab.p,ab.temp)
  
}
ab.p.m<-matrix(ab.p,ncol = 50,byrow = TRUE)[-1,]


