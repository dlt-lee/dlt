library(xgboost)
#BUild data:
trains_1 <-tail(dlt,203)[1:200,]
trains_2 <-tail(dlt,203)[2:201,]
trains_3 <-tail(dlt,203)[3:202,]
trn1<-trains_1$n
trn2<-trains_2$n
tra1.1<-trains_1$a1
tra2.1<-trains_1$a2
tra3.1<-trains_1$a3
tra4.1<-trains_1$a4
tra5.1<-trains_1$a5
tra1.2<-trains_2$a1
tra2.2<-trains_2$a2
tra3.2<-trains_2$a3
tra4.2<-trains_2$a4
tra5.2<-trains_2$a5
tra1.3<-trains_3$a1
tra2.3<-trains_3$a2
tra3.3<-trains_3$a3
tra4.3<-trains_3$a4
tra5.3<-trains_3$a5
trains.a1<-data.frame(trn1,trn2,tra1.3,tra1.2,tra1.1)
trains.a2<-data.frame(trn1,trn2,tra2.3,tra2.2,tra2.1)
trains.a3<-data.frame(trn1,trn2,tra3.3,tra3.2,tra3.1)
trains.a4<-data.frame(trn1,trn2,tra4.3,tra4.2,tra4.1)
trains.a5<-data.frame(trn1,trn2,tra5.3,tra5.2,tra5.1)

results<-tail(dlt,200)
tests_1<-tail(dlt,3)[1,]
tests_2<-tail(dlt,3)[2,]
tests_3<-tail(dlt,3)[3,]
tsn1<-tests_1$n
tsn2<-tests_2$n
tsa1.1<-tests_1$a1
tsa2.1<-tests_1$a2
tsa3.1<-tests_1$a3
tsa4.1<-tests_1$a4
tsa5.1<-tests_1$a5
tsa1.2<-tests_2$a1
tsa2.2<-tests_2$a2
tsa3.2<-tests_2$a3
tsa4.2<-tests_2$a4
tsa5.2<-tests_2$a5
tsa1.3<-tests_3$a1
tsa2.3<-tests_3$a2
tsa3.3<-tests_3$a3
tsa4.3<-tests_3$a4
tsa5.3<-tests_3$a5
tests.a1<-data.frame(tsn1,tsn2,tsa1.3,tsa1.2,tsa1.1)
tests.a2<-data.frame(tsn1,tsn2,tsa2.3,tsa2.2,tsa2.1)
tests.a3<-data.frame(tsn1,tsn2,tsa3.3,tsa3.2,tsa3.1)
tests.a4<-data.frame(tsn1,tsn2,tsa4.3,tsa4.2,tsa4.1)
tests.a5<-data.frame(tsn1,tsn2,tsa5.3,tsa5.2,tsa5.1)
#Convert
m<-19
#dlt_trains.a1<-dlt.convert(trains.a1$trn2,trains.a1[,3:4],1)[,2:36]
#dlt_tests.a1<-dlt.convert(tests.a1$tsn2,tests.a1[,3:4],1)[,2:36]
#dlt_trains.a2<-dlt.convert(trains.a2$trn2,trains.a2[,3:4],1)[,2:36]
#dlt_tests.a2<-dlt.convert(tests.a2$tsn2,tests.a2[,3:4],1)[,2:36]
#dlt_trains.a3<-dlt.convert(trains.a3$trn2,trains.a3[,3:4],1)[,2:36]
#dlt_tests.a3<-dlt.convert(tests.a3$tsn2,tests.a3[,3:4],1)[,2:36]
#dlt_trains.a4<-dlt.convert(trains.a4$trn2,trains.a4[,3:4],1)[,2:36]
#dlt_tests.a4<-dlt.convert(tests.a4$tsn2,tests.a4[,3:4],1)[,2:36]
#dlt_trains.a5<-dlt.convert(trains.a5$trn2,trains.a5[,3:4],1)[,2:36]
#dlt_tests.a5<-dlt.convert(tests.a5$tsn2,tests.a5[,3:4],1)[,2:36]
dlt_trains.a1<-dlt.convert(trains.a1$trn2,trains.a1[,3:5],3)[,2:m]
dlt_tests.a1<-dlt.convert(tests.a1$tsn2,tests.a1[,3:5],3)[,2:m]
dlt_trains.a2<-dlt.convert(trains.a2$trn2,trains.a2[,3:5],3)[,2:m]
dlt_tests.a2<-dlt.convert(tests.a2$tsn2,tests.a2[,3:5],3)[,2:m]
dlt_trains.a3<-dlt.convert(trains.a3$trn2,trains.a3[,3:5],3)[,2:m]
dlt_tests.a3<-dlt.convert(tests.a3$tsn2,tests.a3[,3:5],3)[,2:m]
dlt_trains.a4<-dlt.convert(trains.a4$trn2,trains.a4[,3:5],3)[,2:m]
dlt_tests.a4<-dlt.convert(tests.a4$tsn2,tests.a4[,3:5],3)[,2:m]
dlt_trains.a5<-dlt.convert(trains.a5$trn2,trains.a5[,3:5],3)[,2:m]
dlt_tests.a5<-dlt.convert(tests.a5$tsn2,tests.a5[,3:5],3)[,2:m]
n=200
#A1:
bst.a1 <- xgboost(data = dlt_trains.a1,label = results$a1,nrounds = n)
p.a1 <- predict(object = bst.a1,newdata = t(dlt_tests.a1))
#A2:
bst.a2 <- xgboost(data = dlt_trains.a2,label = results$a2,nrounds = n)
p.a2 <- predict(object = bst.a2,newdata = t(dlt_tests.a2))
#A3:
bst.a3 <- xgboost(data = dlt_trains.a3,label = results$a3,nrounds = n)
p.a3 <- predict(object = bst.a3,newdata = t(dlt_tests.a3))
#A4:
bst.a4 <- xgboost(data = dlt_trains.a4,label = results$a4,nrounds = n)
p.a4 <- predict(object = bst.a4,newdata = t(dlt_tests.a4))
#A5:
bst.a5 <- xgboost(data = dlt_trains.a5,label = results$a5,nrounds = n)
p.a5 <- predict(object = bst.a5,newdata = t(dlt_tests.a5))



#B:
trb1.1<-trains_1$b1
trb2.1<-trains_1$b2
trb1.2<-trains_2$b1
trb2.2<-trains_2$b2
trb1.3<-trains_3$b1
trb2.3<-trains_3$b2
trains.b1<-data.frame(trn1,trn2,trb1.3,trb1.2,trb1.1)
trains.b2<-data.frame(trn1,trn2,trb2.3,trb2.2,trb2.1)
tsb1.1<-tests_1$b1
tsb2.1<-tests_1$a2
tsb1.2<-tests_2$b1
tsb2.2<-tests_2$b2
tsb1.3<-tests_3$b1
tsb2.3<-tests_3$b2
tests.b1<-data.frame(tsn1,tsn2,tsb1.3,tsb1.2,tsb1.1)
tests.b2<-data.frame(tsn1,tsn2,tsb2.3,tsb2.2,tsb2.1)
#Convert
#dlt_trains.b1<-dlt.convert(trains.b1$trn2,trains.b1[,3:4],1)[,2:36]
#dlt_tests.b1<-dlt.convert(tests.b1$tsn2,tests.b1[,3:4],1)[,2:36]
#dlt_trains.b2<-dlt.convert(trains.b2$trn2,trains.b2[,3:4],1)[,2:36]
#dlt_tests.b2<-dlt.convert(tests.b2$tsn2,tests.b2[,3:4],1)[,2:36]
dlt_trains.b1<-dlt.convert(trains.b1$trn2,trains.b1[,3:5],3)[,2:m]
dlt_tests.b1<-dlt.convert(tests.b1$tsn2,tests.b1[,3:5],3)[,2:m]
dlt_trains.b2<-dlt.convert(trains.b2$trn2,trains.b2[,3:5],3)[,2:m]
dlt_tests.b2<-dlt.convert(tests.b2$tsn2,tests.b2[,3:5],3)[,2:m]
n=200
#B1:
bst.b1 <- xgboost(data = dlt_trains.b1,label = results$b1,nrounds = n)
p.b1 <- predict(object = bst.b1,newdata = t(dlt_tests.b1))
#B2:
bst.b2 <- xgboost(data = dlt_trains.b2,label = results$b2,nrounds = n)
p.b2 <- predict(object = bst.b2,newdata = t(dlt_tests.b2))



result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
result
plot(result)



