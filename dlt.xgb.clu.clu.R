rows<-dim(dlt)[1]-273
pre.a1<-0
pre.a2<-0
pre.a3<-0
pre.a4<-0
pre.a5<-0
pre.b1<-0
pre.b2<-0

exp.a1<-0
exp.a2<-0
exp.a3<-0
exp.a4<-0
exp.a5<-0
exp.b1<-0
exp.b2<-0

for(i in 1:90) {
  data<-head(dlt,rows)
  data<-tail(data,300)
  temp.data<-for.clu.xgb(data)
  pre.a1<-c(pre.a1,temp.data$a1)
  
  rows<-rows+3
  
}


