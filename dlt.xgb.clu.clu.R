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
  pre.a2<-c(pre.a2,temp.data$a2)
  pre.a3<-c(pre.a3,temp.data$a3)
  pre.a4<-c(pre.a4,temp.data$a4)
  pre.a5<-c(pre.a5,temp.data$a5)
  pre.b1<-c(pre.b1,temp.data$b1)
  pre.b2<-c(pre.b2,temp.data$b2)
  
  exp.a1<-c(exp.a1,dlt[rows+1,]$a1)
  exp.a2<-c(exp.a2,dlt[rows+1,]$a2)
  exp.a3<-c(exp.a3,dlt[rows+1,]$a3)
  exp.a4<-c(exp.a4,dlt[rows+1,]$a4)
  exp.a5<-c(exp.a5,dlt[rows+1,]$a5)
  exp.b1<-c(exp.b1,dlt[rows+1,]$b1)
  exp.b2<-c(exp.b2,dlt[rows+1,]$b2)
  
  
  rows<-rows+3
  
}

pre.a1<-pre.a1[-1,]
pre.a2<-pre.a2[-1,]
pre.a3<-pre.a3[-1,]
pre.a4<-pre.a4[-1,]
pre.a5<-pre.a5[-1,]
pre.b1<-pre.b1[-1,]
pre.b2<-pre.b2[-1,]


















