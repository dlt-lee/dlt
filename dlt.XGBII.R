dlt.XGBII <- function(data,count) {
  #Build data:
  trains_1 <-tail(data,count)[1:(count-2),]
  trains_2 <-tail(data,count)[2:(count-1),]
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
  trains.a1<-data.frame(trn1,trn2,tra5.1,tra1.1,tra2.1)
  trains.a2<-data.frame(trn1,trn2,tra1.1,tra2.1,tra3.1)
  trains.a3<-data.frame(trn1,trn2,tra2.1,tra3.1,tra4.1)
  trains.a4<-data.frame(trn1,trn2,tra3.1,tra4.1,tra5.1)
  trains.a5<-data.frame(trn1,trn2,tra4.1,tra5.1,tra1.1)
  
  results<-tail(data,count)[3:count,]
  tests_1<-tail(data,2)[1,]
  tests_2<-tail(data,2)[2,]
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
  tests.a1<-data.frame(tsn1,tsn2,tsa5.1,tsa1.1,tsa2.1)
  tests.a2<-data.frame(tsn1,tsn2,tsa1.1,tsa2.1,tsa3.1)
  tests.a3<-data.frame(tsn1,tsn2,tsa2.1,tsa3.1,tsa4.1)
  tests.a4<-data.frame(tsn1,tsn2,tsa3.1,tsa4.1,tsa5.1)
  tests.a5<-data.frame(tsn1,tsn2,tsa4.1,tsa5.1,tsa1.1)
  
  #Convert
  # dlt_trains.a1<-dlt.convert(trains.a1$trn2,trains.a1[,3:5],1)[,2:36]
  #  dlt_tests.a1<-dlt.convert(tests.a1$tsn2,tests.a1[,3:5],1)[,2:36]
  #  dlt_trains.a2<-dlt.convert(trains.a2$trn2,trains.a2[,3:5],1)[,2:36]
  #  dlt_tests.a2<-dlt.convert(tests.a2$tsn2,tests.a2[,3:5],1)[,2:36]
  #  dlt_trains.a3<-dlt.convert(trains.a3$trn2,trains.a3[,3:5],1)[,2:36]
  #  dlt_tests.a3<-dlt.convert(tests.a3$tsn2,tests.a3[,3:5],1)[,2:36]
  #  dlt_trains.a4<-dlt.convert(trains.a4$trn2,trains.a4[,3:5],1)[,2:36]
  #  dlt_tests.a4<-dlt.convert(tests.a4$tsn2,tests.a4[,3:5],1)[,2:36]
  #  dlt_trains.a5<-dlt.convert(trains.a5$trn2,trains.a5[,3:5],1)[,2:36]
  #  dlt_tests.a5<-dlt.convert(tests.a5$tsn2,tests.a5[,3:5],1)[,2:36]
  dlt_trains.a1<-Matrix(as.matrix(trains.a1[,3:5]),sparse=T)
  dlt_tests.a1 <-Matrix(as.matrix(tests.a1[,3:5]),sparse=T)
  dlt_trains.a2<-Matrix(as.matrix(trains.a2[,3:5]),sparse=T)
  dlt_tests.a2 <-Matrix(as.matrix(tests.a2[,3:5]),sparse=T)
  dlt_trains.a3<-Matrix(as.matrix(trains.a3[,3:5]),sparse=T)
  dlt_tests.a3 <-Matrix(as.matrix(tests.a3[,3:5]),sparse=T)
  dlt_trains.a4<-Matrix(as.matrix(trains.a4[,3:5]),sparse=T)
  dlt_tests.a4 <-Matrix(as.matrix(tests.a4[,3:5]),sparse=T)
  dlt_trains.a5<-Matrix(as.matrix(trains.a5[,3:5]),sparse=T)
  dlt_tests.a5 <-Matrix(as.matrix(tests.a5[,3:5]),sparse=T)
  
  n=300
  #A1:
  bst.a1 <- xgboost(data = dlt_trains.a1,label = results$a1,nrounds = n,print_every_n = 300L)
  p.a1 <- predict(object = bst.a1,newdata = t(dlt_tests.a1))
  #A2:
  bst.a2 <- xgboost(data = dlt_trains.a2,label = results$a2,nrounds = n,print_every_n = 300L)
  p.a2 <- predict(object = bst.a2,newdata = t(dlt_tests.a2))
  #A3:
  bst.a3 <- xgboost(data = dlt_trains.a3,label = results$a3,nrounds = n,print_every_n = 300L)
  p.a3 <- predict(object = bst.a3,newdata = t(dlt_tests.a3))
  #A4:
  bst.a4 <- xgboost(data = dlt_trains.a4,label = results$a4,nrounds = n,print_every_n = 300L)
  p.a4 <- predict(object = bst.a4,newdata = t(dlt_tests.a4))
  #A5:
  bst.a5 <- xgboost(data = dlt_trains.a5,label = results$a5,nrounds = n,print_every_n = 300L)
  p.a5 <- predict(object = bst.a5,newdata = t(dlt_tests.a5))
  
  
  #B:
  trb1.1<-trains_1$b1
  trb2.1<-trains_1$b2
  trb1.2<-trains_2$b1
  trb2.2<-trains_2$b2
  trains.b<-data.frame(trn1,trn2,trb1.1,trb1.2,trb2.1,trb2.2)
  tsb1.1<-tests_1$b1
  tsb2.1<-tests_1$a2
  tsb1.2<-tests_2$b1
  tsb2.2<-tests_2$b2
  tests.b<-data.frame(tsn1,tsn2,tsb1.1,tsb2.1,tsb2.1,tsb2.2)
  #Convert
  #dlt_trains.b<-dlt.convert(trains.b$trn2,trains.b[,3:6],3)[,2:25]
  #dlt_tests.b<-dlt.convert(tests.b$tsn2,tests.b[,3:6],3)[,2:25]
  dlt_trains.b<-Matrix(as.matrix(trains.b[,3:6]),sparse=T)
  dlt_tests.b <-Matrix(as.matrix(tests.b[,3:6]),sparse=T)
  
  m=200
  #B1:
  bst.b1 <- xgboost(data = dlt_trains.b,label = results$b1,nrounds = m,print_every_n = 300L)
  p.b1 <- predict(object = bst.b1,newdata = t(dlt_tests.b))
  #B2:
  bst.b2 <- xgboost(data = dlt_trains.b,label = results$b2,nrounds = m,print_every_n = 300L)
  p.b2 <- predict(object = bst.b2,newdata = t(dlt_tests.b))
  
  p.a1<-mean(p.a1)
  p.a2<-mean(p.a2)
  p.a3<-mean(p.a3)
  p.a4<-mean(p.a4)
  p.a5<-mean(p.a5)
  result.a<-c(p.a1,p.a2,p.a3,p.a4,p.a5)
  p.b1
  p.b2
  result<-c(sort(result.a),mean(p.b1),mean(p.b2))
  barplot(result,main = "XGBII")
  return(result)
  
  
}


