library(xgboost)

rows<-dim(dlt)[1]
line<-rows-3  #back to one week
data<-head(dlt,line)  #get data
line<-line-line%%3  # split to group by 3

n<-line/3

data<-head(dlt,rows-2)
line<-line+1
data<-tail(data,line)

a1.1<-0
a2.1<-0
a3.1<-0
a4.1<-0
a5.1<-0
b1.1<-0
b2.1<-0
a1.2<-0
a2.2<-0
a3.2<-0
a4.2<-0
a5.2<-0
b1.2<-0
b2.2<-0
a1.3<-0
a2.3<-0
a3.3<-0
a4.3<-0
a5.3<-0
b1.3<-0
b2.3<-0
res.a1<-0
res.a2<-0
res.a3<-0
res.a4<-0
res.a5<-0
res.b1<-0
res.b2<-0

j<-1

for (i in 1:n) {
  a1.1<-c(a1.1,data[j,]$a1)
  a2.1<-c(a2.1,data[j,]$a2)
  a3.1<-c(a3.1,data[j,]$a3)
  a4.1<-c(a4.1,data[j,]$a4)
  a5.1<-c(a5.1,data[j,]$a5)
  b1.1<-c(b1.1,data[j,]$b1)
  b2.1<-c(b2.1,data[j,]$b2)
  
  a1.2<-c(a1.2,data[j+1,]$a1)
  a2.2<-c(a2.2,data[j+1,]$a2)
  a3.2<-c(a3.2,data[j+1,]$a3)
  a4.2<-c(a4.2,data[j+1,]$a4)
  a5.2<-c(a5.2,data[j+1,]$a5)
  b1.2<-c(b1.2,data[j+1,]$b1)
  b2.2<-c(b2.2,data[j+1,]$b2)
  
  a1.3<-c(a1.3,data[j+2,]$a1)
  a2.3<-c(a2.3,data[j+2,]$a2)
  a3.3<-c(a3.3,data[j+2,]$a3)
  a4.3<-c(a4.3,data[j+2,]$a4)
  a5.3<-c(a5.3,data[j+2,]$a5)
  b1.3<-c(b1.3,data[j+2,]$b1)
  b2.3<-c(b2.3,data[j+2,]$b2)
  
  res.a1<-c(res.a1,data[j+3,]$a1)
  res.a2<-c(res.a2,data[j+3,]$a2)
  res.a3<-c(res.a3,data[j+3,]$a3)
  res.a4<-c(res.a4,data[j+3,]$a4)
  res.a5<-c(res.a5,data[j+3,]$a5)
  res.b1<-c(res.b1,data[j+3,]$b1)
  res.b2<-c(res.b2,data[j+3,]$b2)
  
  j<-j+3
}
a1.1<-tail(a1.1,length(a1.1)-1)
a1.2<-tail(a1.2,length(a1.2)-1)
a1.3<-tail(a1.3,length(a1.3)-1)
a2.1<-tail(a2.1,length(a2.1)-1)
a2.2<-tail(a2.2,length(a2.2)-1)
a2.3<-tail(a2.3,length(a2.3)-1)
a3.1<-tail(a3.1,length(a3.1)-1)
a3.2<-tail(a3.2,length(a3.2)-1)
a3.3<-tail(a3.3,length(a3.3)-1)
a4.1<-tail(a4.1,length(a4.1)-1)
a4.2<-tail(a4.2,length(a4.2)-1)
a4.3<-tail(a4.3,length(a4.3)-1)
a5.1<-tail(a5.1,length(a5.1)-1)
a5.2<-tail(a5.2,length(a5.2)-1)
a5.3<-tail(a5.3,length(a5.3)-1)
b1.1<-tail(b1.1,length(b1.1)-1)
b1.2<-tail(b1.2,length(b1.2)-1)
b1.3<-tail(b1.3,length(b1.3)-1)
b2.1<-tail(b2.1,length(b2.1)-1)
b2.2<-tail(b2.2,length(b2.2)-1)
b2.3<-tail(b2.3,length(b2.3)-1)
res.a1<-tail(res.a1,length(res.a1)-1)
res.a2<-tail(res.a2,length(res.a2)-1)
res.a3<-tail(res.a3,length(res.a3)-1)
res.a4<-tail(res.a4,length(res.a4)-1)
res.a5<-tail(res.a5,length(res.a5)-1)
res.b1<-tail(res.b1,length(res.b1)-1)
res.b2<-tail(res.b2,length(res.b2)-1)



trains.a1<-data.frame(a1.1,a1.2,a1.3,res.a1)
trains.a2<-data.frame(a2.1,a2.2,a2.3,res.a2)
trains.a3<-data.frame(a3.1,a3.2,a3.3,res.a3)
trains.a4<-data.frame(a4.1,a4.2,a4.3,res.a4)
trains.a5<-data.frame(a5.1,a5.2,a5.3,res.a5)
trains.b1<-data.frame(b1.1,b1.2,b1.3,res.b1)
trains.b2<-data.frame(b2.1,b2.2,b2.3,res.b2)


trains.T.a1<-Matrix(as.matrix(trains.a1[,1:3]),sparse=T)
trains.T.a2<-Matrix(as.matrix(trains.a2[,1:3]),sparse=T)
trains.T.a3<-Matrix(as.matrix(trains.a3[,1:3]),sparse=T)
trains.T.a4<-Matrix(as.matrix(trains.a4[,1:3]),sparse=T)
trains.T.a5<-Matrix(as.matrix(trains.a5[,1:3]),sparse=T)
trains.T.b1<-Matrix(as.matrix(trains.b1[,1:3]),sparse=T)
trains.T.b2<-Matrix(as.matrix(trains.b2[,1:3]),sparse=T)
bst.a1<-xgboost(data = trains.T.a1,label = trains.a1$res.a1,nrounds = 300,print_every_n = 300L)
bst.a2<-xgboost(data = trains.T.a2,label = trains.a2$res.a2,nrounds = 300,print_every_n = 300L)
bst.a3<-xgboost(data = trains.T.a3,label = trains.a3$res.a3,nrounds = 300,print_every_n = 300L)
bst.a4<-xgboost(data = trains.T.a4,label = trains.a4$res.a4,nrounds = 300,print_every_n = 300L)
bst.a5<-xgboost(data = trains.T.a5,label = trains.a5$res.a5,nrounds = 300,print_every_n = 300L)
bst.b1<-xgboost(data = trains.T.b1,label = trains.b1$res.b1,nrounds = 300,print_every_n = 300L)
bst.b2<-xgboost(data = trains.T.b2,label = trains.b2$res.b2,nrounds = 300,print_every_n = 300L)


#predoct
a1.1<-dlt$a1[2:(dim(dlt)[1]-2)]
a1.2<-dlt$a1[3:(dim(dlt)[1]-1)]
a1.3<-dlt$a1[4:dim(dlt)[1]]
a2.1<-dlt$a2[2:(dim(dlt)[1]-2)]
a2.2<-dlt$a2[3:(dim(dlt)[1]-1)]
a2.3<-dlt$a2[4:dim(dlt)[1]]
a3.1<-dlt$a3[2:(dim(dlt)[1]-2)]
a3.2<-dlt$a3[3:(dim(dlt)[1]-1)]
a3.3<-dlt$a3[4:dim(dlt)[1]]
a4.1<-dlt$a4[2:(dim(dlt)[1]-2)]
a4.2<-dlt$a4[3:(dim(dlt)[1]-1)]
a4.3<-dlt$a4[4:dim(dlt)[1]]
a5.1<-dlt$a5[2:(dim(dlt)[1]-2)]
a5.2<-dlt$a5[3:(dim(dlt)[1]-1)]
a5.3<-dlt$a5[4:dim(dlt)[1]]
b1.1<-dlt$b1[2:(dim(dlt)[1]-2)]
b1.2<-dlt$b1[3:(dim(dlt)[1]-1)]
b1.3<-dlt$b1[4:dim(dlt)[1]]
b2.1<-dlt$b2[2:(dim(dlt)[1]-2)]
b2.2<-dlt$b2[3:(dim(dlt)[1]-1)]
b2.3<-dlt$b2[4:dim(dlt)[1]]
tests.a1<-data.frame(a1.1,a1.2,a1.3)
tests.a2<-data.frame(a2.1,a2.2,a2.3)
tests.a3<-data.frame(a3.1,a3.2,a3.3)
tests.a4<-data.frame(a4.1,a4.2,a4.3)
tests.a5<-data.frame(a5.1,a5.2,a5.3)
tests.b1<-data.frame(b1.1,b1.2,b1.3)
tests.b2<-data.frame(b2.1,b2.2,b2.3)
tests.T.a1<-Matrix(as.matrix(tests.a1),sparse=T)
tests.T.a2<-Matrix(as.matrix(tests.a2),sparse=T)
tests.T.a3<-Matrix(as.matrix(tests.a3),sparse=T)
tests.T.a4<-Matrix(as.matrix(tests.a4),sparse=T)
tests.T.a5<-Matrix(as.matrix(tests.a5),sparse=T)
tests.T.b1<-Matrix(as.matrix(tests.b1),sparse=T)
tests.T.b2<-Matrix(as.matrix(tests.b2),sparse=T)
testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.a1)
testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.a2)
testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.a3)
testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.a4)
testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.a5)
testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.b1)
testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.b2)


#result
c(round(tail(testPredictions.a1,1)),
  round(tail(testPredictions.a2,1)),
  round(tail(testPredictions.a3,1)),
  round(tail(testPredictions.a4,1)),
  round(tail(testPredictions.a5,1)),
  round(tail(testPredictions.b1,1)),
  round(tail(testPredictions.b2,1))
)














