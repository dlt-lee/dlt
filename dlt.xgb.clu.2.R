library(xgboost)

trains<-dlt.data.reset(dlt)
trains.T<-Matrix(as.matrix(trains[,1:63]),sparse=T)
bst.a1<-xgboost(data = trains.T[,c(50,57)],label = trains$res.a1,nrounds = 300,print_every_n = 300L)
bst.a2<-xgboost(data = trains.T[,c(51,58)],label = trains$res.a2,nrounds = 300,print_every_n = 300L)
bst.a3<-xgboost(data = trains.T[,c(52,59)],label = trains$res.a3,nrounds = 300,print_every_n = 300L)
bst.a4<-xgboost(data = trains.T[,c(53,60)],label = trains$res.a4,nrounds = 300,print_every_n = 300L)
bst.a5<-xgboost(data = trains.T[,c(54,61)],label = trains$res.a5,nrounds = 300,print_every_n = 300L)
bst.b1<-xgboost(data = trains.T[,c(55,62)],label = trains$res.b1,nrounds = 300,print_every_n = 300L)
bst.b2<-xgboost(data = trains.T[,c(56,63)],label = trains$res.b2,nrounds = 300,print_every_n = 300L)

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
testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T[,c(50,57)])
testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T[,c(51,58)])
testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T[,c(52,59)])
testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T[,c(53,60)])
testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T[,c(54,61)])
testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T[,c(55,62)])
testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T[,c(56,63)])


#result
c(round(tail(testPredictions.a1,1)),
  round(tail(testPredictions.a2,1)),
  round(tail(testPredictions.a3,1)),
  round(tail(testPredictions.a4,1)),
  round(tail(testPredictions.a5,1)),
  round(tail(testPredictions.b1,1)),
  round(tail(testPredictions.b2,1))
)



