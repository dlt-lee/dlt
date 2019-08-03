library(xgboost)

rows<-dim(dlt)[1]
line<-rows-9  #back to one month
data<-head(dlt,line)  #get data
line<-line-line%%3  # split to group by 3

n<-line/3

data<-head(dlt,rows-2)
line<-line+1
data<-tail(data,line)

a1.1<-0;a2.1<-0;a3.1<-0;a4.1<-0;a5.1<-0;b1.1<-0;b2.1<-0
a1.2<-0;a2.2<-0;a3.2<-0;a4.11<-0;a5.11<-0;b1.11<-0;b2.11<-0
a1.3<-0;a2.3<-0;a3.3<-0;a4.21<-0;a5.21<-0;b1.21<-0;b2.21<-0
a1.4<-0;a2.4<-0;a3.4<-0;a4.31<-0;a5.31<-0;b1.31<-0;b2.31<-0
a1.5<-0;a2.5<-0;a3.5<-0;a4.41<-0;a5.41<-0;b1.41<-0;b2.41<-0
a1.6<-0;a2.6<-0;a3.6<-0;a4.51<-0;a5.51<-0;b1.51<-0;b2.51<-0
a1.7<-0;a2.7-0;a3.7<-0;a4.61<-0;a5.61<-0;b1.61<-0;b2.61<-0
a1.8<-0;a2.8<-0;a3.8<-0;a4.71<-0;a5.71<-0;b1.71<-0;b2.71<-0
a1.9<-0;a2.9<-0;a3.9<-0;a4.81<-0;a5.81<-0;b1.81<-0;b2.81<-0
res.a1<-0
res.a2<-0
res.a3<-0
res.a4<-0
res.a5<-0
res.b1<-0
res.b2<-0

j<-1
for (j in 1:n) {
  a1.01<-c(a1.01,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.11<-c(a1.11,data[j,]$a1);a2.11<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.21<-c(a1.21,data[j,]$a1);a2.21<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.31<-c(a1.31,data[j,]$a1);a2.231<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.41<-c(a1.41,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.51<-c(a1.51,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.61<-c(a1.61,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.71<-c(a1.71,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.81<-c(a1.81,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  a1.91<-c(a1.91,data[j,]$a1);a2.01<-c(a2.01,data[j,]$a2);a3.01<-c(a3.01,data[j,]$a3);a4.01<-c(a4.01,data[j,]$a4);a5.01<-c(a5.01,data[j,]$a5);b1.01<-c(b10.1,data[j,]$b1);b2.01<-c(b2.01,data[j,]$b2)
  
}