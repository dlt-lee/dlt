library(xgboost)

#train
a1.1<-dlt$a1[1:(dim(dlt)[1]-3)]
a1.2<-dlt$a1[2:(dim(dlt)[1]-2)]
a1.3<-dlt$a1[3:(dim(dlt)[1]-1)]
a2.1<-dlt$a2[1:(dim(dlt)[1]-3)]
a2.2<-dlt$a2[2:(dim(dlt)[1]-2)]
a2.3<-dlt$a2[3:(dim(dlt)[1]-1)]
a3.1<-dlt$a3[1:(dim(dlt)[1]-3)]
a3.2<-dlt$a3[2:(dim(dlt)[1]-2)]
a3.3<-dlt$a3[3:(dim(dlt)[1]-1)]
a4.1<-dlt$a4[1:(dim(dlt)[1]-3)]
a4.2<-dlt$a4[2:(dim(dlt)[1]-2)]
a4.3<-dlt$a4[3:(dim(dlt)[1]-1)]
a5.1<-dlt$a5[1:(dim(dlt)[1]-3)]
a5.2<-dlt$a5[2:(dim(dlt)[1]-2)]
a5.3<-dlt$a5[3:(dim(dlt)[1]-1)]
b1.1<-dlt$b1[1:(dim(dlt)[1]-3)]
b1.2<-dlt$b1[2:(dim(dlt)[1]-2)]
b1.3<-dlt$b1[3:(dim(dlt)[1]-1)]
b2.1<-dlt$b2[1:(dim(dlt)[1]-3)]
b2.2<-dlt$b2[2:(dim(dlt)[1]-2)]
b2.3<-dlt$b2[3:(dim(dlt)[1]-1)]
result.a1<-dlt$a1[4:dim(dlt)[1]]
result.a2<-dlt$a2[4:dim(dlt)[1]]
result.a3<-dlt$a3[4:dim(dlt)[1]]
result.a4<-dlt$a4[4:dim(dlt)[1]]
result.a5<-dlt$a5[4:dim(dlt)[1]]
result.b1<-dlt$b1[4:dim(dlt)[1]]
result.b2<-dlt$b2[4:dim(dlt)[1]]
trains.a1<-data.frame(a1.1,a1.2,a1.3,result.a1)
trains.a2<-data.frame(a2.1,a2.2,a2.3,result.a2)
trains.a3<-data.frame(a3.1,a3.2,a3.3,result.a3)
trains.a4<-data.frame(a4.1,a4.2,a4.3,result.a4)
trains.a5<-data.frame(a5.1,a5.2,a5.3,result.a5)
trains.b1<-data.frame(b1.1,b1.2,b1.3,result.b1)
trains.b2<-data.frame(b2.1,b2.2,b2.3,result.b2)
trains.T.a1<-Matrix(as.matrix(trains.a1[,1:3]),sparse=T)
trains.T.a2<-Matrix(as.matrix(trains.a2[,1:3]),sparse=T)
trains.T.a3<-Matrix(as.matrix(trains.a3[,1:3]),sparse=T)
trains.T.a4<-Matrix(as.matrix(trains.a4[,1:3]),sparse=T)
trains.T.a5<-Matrix(as.matrix(trains.a5[,1:3]),sparse=T)
trains.T.b1<-Matrix(as.matrix(trains.b1[,1:3]),sparse=T)
trains.T.b2<-Matrix(as.matrix(trains.b2[,1:3]),sparse=T)
bst.a1<-xgboost(data = trains.T.a1,label = trains.a1$result.a1,nrounds = 300,print_every_n = 300L)
bst.a2<-xgboost(data = trains.T.a2,label = trains.a2$result.a2,nrounds = 300,print_every_n = 300L)
bst.a3<-xgboost(data = trains.T.a3,label = trains.a3$result.a3,nrounds = 300,print_every_n = 300L)
bst.a4<-xgboost(data = trains.T.a4,label = trains.a4$result.a4,nrounds = 300,print_every_n = 300L)
bst.a5<-xgboost(data = trains.T.a5,label = trains.a5$result.a5,nrounds = 300,print_every_n = 300L)
bst.b1<-xgboost(data = trains.T.b1,label = trains.b1$result.b1,nrounds = 300,print_every_n = 300L)
bst.b2<-xgboost(data = trains.T.b2,label = trains.b2$result.b2,nrounds = 300,print_every_n = 300L)

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

