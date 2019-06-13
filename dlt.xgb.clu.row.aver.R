#data<-data.for.xgb.clu
dlt.xgb.clu.row.aver <- function(data){
  n<-300
  m<-length(data$pre.a1)/16
  a1<-data$pre.a1
  a2<-data$pre.a2
  a3<-data$pre.a3
  a4<-data$pre.a4
  a5<-data$pre.a5
  b1<-data$pre.b1
  b2<-data$pre.b2
  
  pre.f.ab<-data.frame(a1,a2,a3,a4,a5,b1,b2)
  trains.T.ab<-Matrix(as.matrix(pre.f.ab),sparse=T)
  
  exp.a1<-0
  exp.a2<-0
  exp.a3<-0
  exp.a4<-0
  exp.a5<-0
  exp.b1<-0
  exp.b2<-0
  
  for (i in 1:m) {
    for (j in 1:16) {
      exp.a1<-c(exp.a1,data$exp.a1[i])
      exp.a2<-c(exp.a2,data$exp.a2[i])
      exp.a3<-c(exp.a3,data$exp.a3[i])
      exp.a4<-c(exp.a4,data$exp.a4[i])
      exp.a5<-c(exp.a5,data$exp.a5[i])
      exp.b1<-c(exp.b1,data$exp.b1[i])
      exp.b2<-c(exp.b2,data$exp.b2[i])
      
    }
    
  }
  
  exp.a1<-exp.a1[-1]
  exp.a2<-exp.a2[-1]
  exp.a3<-exp.a3[-1]
  exp.a4<-exp.a4[-1]
  exp.a5<-exp.a5[-1]
  exp.b1<-exp.b1[-1]
  exp.b2<-exp.b2[-1]
  
  bst.a1 <- xgboost(data = trains.T.ab,label = exp.a1,nrounds = n,print_every_n = 300L)
  bst.a2 <- xgboost(data = trains.T.ab,label = exp.a2,nrounds = n,print_every_n = 300L)
  bst.a3 <- xgboost(data = trains.T.ab,label = exp.a3,nrounds = n,print_every_n = 300L)
  bst.a4 <- xgboost(data = trains.T.ab,label = exp.a4,nrounds = n,print_every_n = 300L)
  bst.a5 <- xgboost(data = trains.T.ab,label = exp.a5,nrounds = n,print_every_n = 300L)
  bst.b1 <- xgboost(data = trains.T.ab,label = exp.b1,nrounds = n,print_every_n = 300L)
  bst.b2 <- xgboost(data = trains.T.ab,label = exp.b2,nrounds = n,print_every_n = 300L)
  
  data<-tail(dlt,300)
  data.tr<-dlt.data.filter(data)
  pre.final.data<-dlt.xgboost.clu(data,data.tr)
  
  a1<-pre.final.data$a1.clu
  a2<-pre.final.data$a2.clu
  a3<-pre.final.data$a3.clu
  a4<-pre.final.data$a4.clu
  a5<-pre.final.data$a5.clu
  b1<-pre.final.data$b1.clu
  b2<-pre.final.data$b2.clu
  
  test.f.ab<-data.frame(a1,a2,a3,a4,a5,b1,b2)
  tests.T.ab<-Matrix(as.matrix(test.f.ab),sparse=T)
  
  testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.ab)
  testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.ab)
  testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.ab)
  testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.ab)
  testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.ab)
  testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.ab)
  testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.ab)
  
  a1<-round(testPredictions.a1)
  a2<-round(testPredictions.a2)
  a3<-round(testPredictions.a3)
  a4<-round(testPredictions.a4)
  a5<-round(testPredictions.a5)
  b1<-round(testPredictions.b1)
  b2<-round(testPredictions.b2)
  
  result<-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  return(result)
  
  
}























