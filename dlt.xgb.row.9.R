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
a1.2<-0;a2.2<-0;a3.2<-0;a4.2<-0;a5.2<-0;b1.2<-0;b2.2<-0
a1.3<-0;a2.3<-0;a3.3<-0;a4.3<-0;a5.3<-0;b1.3<-0;b2.3<-0
a1.4<-0;a2.4<-0;a3.4<-0;a4.4<-0;a5.4<-0;b1.4<-0;b2.4<-0
a1.5<-0;a2.5<-0;a3.5<-0;a4.5<-0;a5.5<-0;b1.5<-0;b2.5<-0
a1.6<-0;a2.6<-0;a3.6<-0;a4.6<-0;a5.6<-0;b1.6<-0;b2.6<-0
a1.7<-0;a2.7<-0;a3.7<-0;a4.7<-0;a5.7<-0;b1.7<-0;b2.7<-0
a1.8<-0;a2.8<-0;a3.8<-0;a4.8<-0;a5.8<-0;b1.8<-0;b2.8<-0
a1.9<-0;a2.9<-0;a3.9<-0;a4.9<-0;a5.9<-0;b1.9<-0;b2.9<-0
res.a1<-0
res.a2<-0
res.a3<-0
res.a4<-0
res.a5<-0
res.b1<-0
res.b2<-0

j<-1
for (j in 1:n) {
  a1.1<-c(a1.1,data[j+0,]$a1);a2.1<-c(a2.1,data[j+0,]$a2);a3.1<-c(a3.1,data[j+0,]$a3);a4.1<-c(a4.1,data[j+0,]$a4);a5.1<-c(a5.1,data[j+0,]$a5);b1.1<-c(b1.1,data[j+0,]$b1);b2.1<-c(b2.1,data[j+0,]$b2)
  a1.2<-c(a1.2,data[j+1,]$a1);a2.2<-c(a2.2,data[j+1,]$a2);a3.2<-c(a3.2,data[j+1,]$a3);a4.2<-c(a4.2,data[j+1,]$a4);a5.2<-c(a5.2,data[j+1,]$a5);b1.2<-c(b1.2,data[j+1,]$b1);b2.2<-c(b2.2,data[j+1,]$b2)
  a1.3<-c(a1.3,data[j+2,]$a1);a2.3<-c(a2.3,data[j+2,]$a2);a3.3<-c(a3.3,data[j+2,]$a3);a4.3<-c(a4.3,data[j+2,]$a4);a5.3<-c(a5.3,data[j+2,]$a5);b1.3<-c(b1.3,data[j+2,]$b1);b2.3<-c(b2.3,data[j+2,]$b2)
  a1.4<-c(a1.4,data[j+3,]$a1);a2.4<-c(a2.4,data[j+3,]$a2);a3.4<-c(a3.4,data[j+3,]$a3);a4.4<-c(a4.4,data[j+3,]$a4);a5.4<-c(a5.4,data[j+3,]$a5);b1.4<-c(b1.4,data[j+3,]$b1);b2.4<-c(b2.4,data[j+3,]$b2)
  a1.5<-c(a1.5,data[j+4,]$a1);a2.5<-c(a2.5,data[j+4,]$a2);a3.5<-c(a3.5,data[j+4,]$a3);a4.5<-c(a4.5,data[j+4,]$a4);a5.5<-c(a5.5,data[j+4,]$a5);b1.5<-c(b1.5,data[j+4,]$b1);b2.5<-c(b2.5,data[j+4,]$b2)
  a1.6<-c(a1.6,data[j+5,]$a1);a2.6<-c(a2.6,data[j+5,]$a2);a3.6<-c(a3.6,data[j+5,]$a3);a4.6<-c(a4.6,data[j+5,]$a4);a5.6<-c(a5.6,data[j+5,]$a5);b1.6<-c(b1.6,data[j+5,]$b1);b2.6<-c(b2.6,data[j+5,]$b2)
  a1.7<-c(a1.7,data[j+6,]$a1);a2.7<-c(a2.7,data[j+6,]$a2);a3.7<-c(a3.7,data[j+6,]$a3);a4.7<-c(a4.7,data[j+6,]$a4);a5.7<-c(a5.7,data[j+6,]$a5);b1.7<-c(b1.7,data[j+6,]$b1);b2.7<-c(b2.7,data[j+6,]$b2)
  a1.8<-c(a1.8,data[j+7,]$a1);a2.8<-c(a2.8,data[j+7,]$a2);a3.8<-c(a3.8,data[j+7,]$a3);a4.8<-c(a4.8,data[j+7,]$a4);a5.8<-c(a5.8,data[j+7,]$a5);b1.8<-c(b1.8,data[j+7,]$b1);b2.8<-c(b2.8,data[j+7,]$b2)
  a1.9<-c(a1.9,data[j+8,]$a1);a2.9<-c(a2.9,data[j+8,]$a2);a3.9<-c(a3.9,data[j+8,]$a3);a4.9<-c(a4.9,data[j+8,]$a4);a5.9<-c(a5.9,data[j+8,]$a5);b1.9<-c(b1.9,data[j+8,]$b1);b2.9<-c(b2.9,data[j+8,]$b2)
  res.a1<-c(res.a1,data[j+9,]$a1)
  res.a2<-c(res.a2,data[j+9,]$a2)
  res.a3<-c(res.a3,data[j+9,]$a3)
  res.a4<-c(res.a4,data[j+9,]$a4)
  res.a5<-c(res.a5,data[j+9,]$a5)
  res.b1<-c(res.b1,data[j+9,]$b1)
  res.b2<-c(res.b2,data[j+9,]$b2)
  j=j+3
}
a1.1<-as.matrix(a1.1)[-1];a2.1<-as.matrix(a2.1)[-1];a3.1<-as.matrix(a3.1)[-1];a4.1<-as.matrix(a4.1)[-1];a5.1<-as.matrix(a5.1)[-1];b1.1<-as.matrix(b1.1)[-1];b2.1<-as.matrix(b2.1)[-1]
a1.2<-as.matrix(a1.2)[-1];a2.2<-as.matrix(a2.2)[-1];a3.2<-as.matrix(a3.2)[-1];a4.2<-as.matrix(a4.2)[-1];a5.2<-as.matrix(a5.2)[-1];b1.2<-as.matrix(b1.2)[-1];b2.2<-as.matrix(b2.2)[-1]
a1.3<-as.matrix(a1.3)[-1];a2.3<-as.matrix(a2.3)[-1];a3.3<-as.matrix(a3.3)[-1];a4.3<-as.matrix(a4.3)[-1];a5.3<-as.matrix(a5.3)[-1];b1.3<-as.matrix(b1.3)[-1];b2.3<-as.matrix(b2.3)[-1]
a1.4<-as.matrix(a1.4)[-1];a2.4<-as.matrix(a2.4)[-1];a3.4<-as.matrix(a3.4)[-1];a4.4<-as.matrix(a4.4)[-1];a5.4<-as.matrix(a5.4)[-1];b1.4<-as.matrix(b1.4)[-1];b2.4<-as.matrix(b2.4)[-1]
a1.5<-as.matrix(a1.5)[-1];a2.5<-as.matrix(a2.5)[-1];a3.5<-as.matrix(a3.5)[-1];a4.5<-as.matrix(a4.5)[-1];a5.5<-as.matrix(a5.5)[-1];b1.5<-as.matrix(b1.5)[-1];b2.5<-as.matrix(b2.5)[-1]
a1.6<-as.matrix(a1.6)[-1];a2.6<-as.matrix(a2.6)[-1];a3.6<-as.matrix(a3.6)[-1];a4.6<-as.matrix(a4.6)[-1];a5.6<-as.matrix(a5.6)[-1];b1.6<-as.matrix(b1.6)[-1];b2.6<-as.matrix(b2.6)[-1]
a1.7<-as.matrix(a1.7)[-1];a2.7<-as.matrix(a2.7)[-1];a3.7<-as.matrix(a3.7)[-1];a4.7<-as.matrix(a4.7)[-1];a5.7<-as.matrix(a5.7)[-1];b1.7<-as.matrix(b1.7)[-1];b2.7<-as.matrix(b2.7)[-1]
a1.8<-as.matrix(a1.8)[-1];a2.8<-as.matrix(a2.8)[-1];a3.8<-as.matrix(a3.8)[-1];a4.8<-as.matrix(a4.8)[-1];a5.8<-as.matrix(a5.8)[-1];b1.8<-as.matrix(b1.8)[-1];b2.8<-as.matrix(b2.8)[-1]
a1.9<-as.matrix(a1.9)[-1];a2.9<-as.matrix(a2.9)[-1];a3.9<-as.matrix(a3.9)[-1];a4.9<-as.matrix(a4.9)[-1];a5.9<-as.matrix(a5.9)[-1];b1.9<-as.matrix(b1.9)[-1];b2.9<-as.matrix(b2.9)[-1]
res.a1<-as.matrix(res.a1)[-1]
res.a2<-as.matrix(res.a2)[-1]
res.a3<-as.matrix(res.a3)[-1]
res.a4<-as.matrix(res.a4)[-1]
res.a5<-as.matrix(res.a5)[-1]
res.b1<-as.matrix(res.b1)[-1]
res.b2<-as.matrix(res.b2)[-1]

trains.a1<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.a1)
trains.a2<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.a2)
trains.a3<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.a3)
trains.a4<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.a4)
trains.a5<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.a5)
trains.b1<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.b1)
trains.b2<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                      a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                      a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                      a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                      a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                      a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                      a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                      res.b2)
trains.T.a1<-Matrix(as.matrix(trains.a1[,1:63]),sparse=T)
trains.T.a2<-Matrix(as.matrix(trains.a2[,1:63]),sparse=T)
trains.T.a3<-Matrix(as.matrix(trains.a3[,1:63]),sparse=T)
trains.T.a4<-Matrix(as.matrix(trains.a4[,1:63]),sparse=T)
trains.T.a5<-Matrix(as.matrix(trains.a5[,1:63]),sparse=T)
trains.T.b1<-Matrix(as.matrix(trains.b1[,1:63]),sparse=T)
trains.T.b2<-Matrix(as.matrix(trains.b2[,1:63]),sparse=T)
bst.a1<-xgboost(data = trains.T.a1,label = trains.a1$res.a1,nrounds = 300,print_every_n = 300L)
bst.a2<-xgboost(data = trains.T.a2,label = trains.a2$res.a2,nrounds = 300,print_every_n = 300L)
bst.a3<-xgboost(data = trains.T.a3,label = trains.a3$res.a3,nrounds = 300,print_every_n = 300L)
bst.a4<-xgboost(data = trains.T.a4,label = trains.a4$res.a4,nrounds = 300,print_every_n = 300L)
bst.a5<-xgboost(data = trains.T.a5,label = trains.a5$res.a5,nrounds = 300,print_every_n = 300L)
bst.b1<-xgboost(data = trains.T.b1,label = trains.b1$res.b1,nrounds = 300,print_every_n = 300L)
bst.b2<-xgboost(data = trains.T.b2,label = trains.b2$res.b2,nrounds = 300,print_every_n = 300L)


#predoct
a1.1<-dlt$a1[1:(dim(dlt)[1]-8)];a2.1<-dlt$a2[1:(dim(dlt)[1]-8)];a3.1<-dlt$a3[1:(dim(dlt)[1]-8)];a4.1<-dlt$a4[1:(dim(dlt)[1]-8)];a5.1<-dlt$a5[1:(dim(dlt)[1]-8)];b1.1<-dlt$b1[1:(dim(dlt)[1]-8)];b2.1<-dlt$b2[1:(dim(dlt)[1]-8)]
a1.2<-dlt$a1[2:(dim(dlt)[1]-7)];a2.2<-dlt$a2[2:(dim(dlt)[1]-7)];a3.2<-dlt$a3[2:(dim(dlt)[1]-7)];a4.2<-dlt$a4[2:(dim(dlt)[1]-7)];a5.2<-dlt$a5[2:(dim(dlt)[1]-7)];b1.2<-dlt$b1[2:(dim(dlt)[1]-7)];b2.2<-dlt$b2[2:(dim(dlt)[1]-7)]
a1.3<-dlt$a1[3:(dim(dlt)[1]-6)];a2.3<-dlt$a2[3:(dim(dlt)[1]-6)];a3.3<-dlt$a3[3:(dim(dlt)[1]-6)];a4.3<-dlt$a4[3:(dim(dlt)[1]-6)];a5.3<-dlt$a5[3:(dim(dlt)[1]-6)];b1.3<-dlt$b1[3:(dim(dlt)[1]-6)];b2.3<-dlt$b2[3:(dim(dlt)[1]-6)]
a1.4<-dlt$a1[4:(dim(dlt)[1]-5)];a2.4<-dlt$a2[4:(dim(dlt)[1]-5)];a3.4<-dlt$a3[4:(dim(dlt)[1]-5)];a4.4<-dlt$a4[4:(dim(dlt)[1]-5)];a5.4<-dlt$a5[4:(dim(dlt)[1]-5)];b1.4<-dlt$b1[4:(dim(dlt)[1]-5)];b2.4<-dlt$b2[4:(dim(dlt)[1]-5)]
a1.5<-dlt$a1[5:(dim(dlt)[1]-4)];a2.5<-dlt$a2[5:(dim(dlt)[1]-4)];a3.5<-dlt$a3[5:(dim(dlt)[1]-4)];a4.5<-dlt$a4[5:(dim(dlt)[1]-4)];a5.5<-dlt$a5[5:(dim(dlt)[1]-4)];b1.5<-dlt$b1[5:(dim(dlt)[1]-4)];b2.5<-dlt$b2[5:(dim(dlt)[1]-4)]
a1.6<-dlt$a1[6:(dim(dlt)[1]-3)];a2.6<-dlt$a2[6:(dim(dlt)[1]-3)];a3.6<-dlt$a3[6:(dim(dlt)[1]-3)];a4.6<-dlt$a4[6:(dim(dlt)[1]-3)];a5.6<-dlt$a5[6:(dim(dlt)[1]-3)];b1.6<-dlt$b1[6:(dim(dlt)[1]-3)];b2.6<-dlt$b2[6:(dim(dlt)[1]-3)]
a1.7<-dlt$a1[7:(dim(dlt)[1]-2)];a2.7<-dlt$a2[7:(dim(dlt)[1]-2)];a3.7<-dlt$a3[7:(dim(dlt)[1]-2)];a4.7<-dlt$a4[7:(dim(dlt)[1]-2)];a5.7<-dlt$a5[7:(dim(dlt)[1]-2)];b1.7<-dlt$b1[7:(dim(dlt)[1]-2)];b2.7<-dlt$b2[7:(dim(dlt)[1]-2)]
a1.8<-dlt$a1[8:(dim(dlt)[1]-1)];a2.8<-dlt$a2[8:(dim(dlt)[1]-1)];a3.8<-dlt$a3[8:(dim(dlt)[1]-1)];a4.8<-dlt$a4[8:(dim(dlt)[1]-1)];a5.8<-dlt$a5[8:(dim(dlt)[1]-1)];b1.8<-dlt$b1[8:(dim(dlt)[1]-1)];b2.8<-dlt$b2[8:(dim(dlt)[1]-1)]
a1.9<-dlt$a1[9:(dim(dlt)[1]-0)];a2.9<-dlt$a2[9:(dim(dlt)[1]-0)];a3.9<-dlt$a3[9:(dim(dlt)[1]-0)];a4.9<-dlt$a4[9:(dim(dlt)[1]-0)];a5.9<-dlt$a5[9:(dim(dlt)[1]-0)];b1.9<-dlt$b1[9:(dim(dlt)[1]-0)];b2.9<-dlt$b2[9:(dim(dlt)[1]-0)]

tests<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                  a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                  a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                  a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                  a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                  a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                  a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                  a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                  a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9)
tests.T<-Matrix(as.matrix(tests),sparse=T)
testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T)
testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T)
testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T)
testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T)
testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T)
testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T)
testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T)


#result
c(round(tail(testPredictions.a1,1)),
  round(tail(testPredictions.a2,1)),
  round(tail(testPredictions.a3,1)),
  round(tail(testPredictions.a4,1)),
  round(tail(testPredictions.a5,1)),
  round(tail(testPredictions.b1,1)),
  round(tail(testPredictions.b2,1))
)





