#data<-data.for.xgb.clu
dlt.xgb.clu.cum <- function(data){
  n<-300
  m<-length(data$pre.a1)/16
  a1<-data$pre.a1
  a2<-data$pre.a2
  a3<-data$pre.a3
  a4<-data$pre.a4
  a5<-data$pre.a5
  b1<-data$pre.b1
  b2<-data$pre.b2
  
  a1.m<-matrix(a1,ncol = 16,byrow = TRUE)
  a2.m<-matrix(a2,ncol = 16,byrow = TRUE)
  a3.m<-matrix(a3,ncol = 16,byrow = TRUE)
  a4.m<-matrix(a4,ncol = 16,byrow = TRUE)
  a5.m<-matrix(a5,ncol = 16,byrow = TRUE)
  b1.m<-matrix(b1,ncol = 16,byrow = TRUE)
  b2.m<-matrix(b2,ncol = 16,byrow = TRUE)
  
  pre.a1<-0
  pre.a2<-0
  pre.a3<-0
  pre.a4<-0
  pre.a5<-0
  pre.b1<-0
  pre.b2<-0
  
  for(i in 1:m) {
    for (j in 1:16) {
      pre.a1<-c(pre.a1,a1.m[i,])
      pre.a2<-c(pre.a2,a2.m[i,])
      pre.a3<-c(pre.a3,a3.m[i,])
      pre.a4<-c(pre.a4,a4.m[i,])
      pre.a5<-c(pre.a5,a5.m[i,])
      pre.b1<-c(pre.b1,b1.m[i,])
      pre.b2<-c(pre.b2,b2.m[i,])
      
    }
  }
  
  pre.a1<-pre.a1[-1]
  pre.a2<-pre.a2[-1]
  pre.a3<-pre.a3[-1]
  pre.a4<-pre.a4[-1]
  pre.a5<-pre.a5[-1]
  pre.b1<-pre.b1[-1]
  pre.b2<-pre.b2[-1]
  
  a1.m<-matrix(pre.a1,ncol = 16,byrow = TRUE)
  a2.m<-matrix(pre.a2,ncol = 16,byrow = TRUE)
  a3.m<-matrix(pre.a3,ncol = 16,byrow = TRUE)
  a4.m<-matrix(pre.a4,ncol = 16,byrow = TRUE)
  a5.m<-matrix(pre.a5,ncol = 16,byrow = TRUE)
  b1.m<-matrix(pre.b1,ncol = 16,byrow = TRUE)
  b2.m<-matrix(pre.b2,ncol = 16,byrow = TRUE)
  
  pre.f.a1<-data.frame(a1.m,a1,a2,a3,a4,a5)
  pre.f.a2<-data.frame(a2.m,a1,a2,a3,a4,a5)
  pre.f.a3<-data.frame(a3.m,a1,a2,a3,a4,a5)
  pre.f.a4<-data.frame(a4.m,a1,a2,a3,a4,a5)
  pre.f.a5<-data.frame(a5.m,a1,a2,a3,a4,a5)
  pre.f.b1<-data.frame(b1.m,b1,b2)
  pre.f.b2<-data.frame(b2.m,b1,b2)
  

  trains.T.a1<-Matrix(as.matrix(pre.f.a1),sparse=T)
  trains.T.a2<-Matrix(as.matrix(pre.f.a2),sparse=T)
  trains.T.a3<-Matrix(as.matrix(pre.f.a3),sparse=T)
  trains.T.a4<-Matrix(as.matrix(pre.f.a4),sparse=T)
  trains.T.a5<-Matrix(as.matrix(pre.f.a5),sparse=T)
  trains.T.b1<-Matrix(as.matrix(pre.f.b1),sparse=T)
  trains.T.b2<-Matrix(as.matrix(pre.f.b2),sparse=T)
  
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
  
  
  bst.a1 <- xgboost(data = trains.T.a1,label = exp.a1,nrounds = n,print_every_n = 300L)
  bst.a2 <- xgboost(data = trains.T.a2,label = exp.a2,nrounds = n,print_every_n = 300L)
  bst.a3 <- xgboost(data = trains.T.a3,label = exp.a3,nrounds = n,print_every_n = 300L)
  bst.a4 <- xgboost(data = trains.T.a4,label = exp.a4,nrounds = n,print_every_n = 300L)
  bst.a5 <- xgboost(data = trains.T.a5,label = exp.a5,nrounds = n,print_every_n = 300L)
  bst.b1 <- xgboost(data = trains.T.b1,label = exp.b1,nrounds = n,print_every_n = 300L)
  bst.b2 <- xgboost(data = trains.T.b2,label = exp.b2,nrounds = n,print_every_n = 300L)
  
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
  
  test.a1<-0
  test.a2<-0
  test.a3<-0
  test.a4<-0
  test.a5<-0
  test.b1<-0
  test.b2<-0
  
  for (i in 1:16) {
    test.a1<-c(test.a1,a1)
    test.a2<-c(test.a2,a2)
    test.a3<-c(test.a3,a3)
    test.a4<-c(test.a4,a4)
    test.a5<-c(test.a5,a5)
    test.b1<-c(test.b1,b1)
    test.b2<-c(test.b2,b2)
  }
  test.a1<-test.a1[-1]
  test.a2<-test.a2[-1]
  test.a3<-test.a3[-1]
  test.a4<-test.a4[-1]
  test.a5<-test.a5[-1]
  test.b1<-test.b1[-1]
  test.b2<-test.b2[-1]
  
  
  
  a1.m<-matrix(test.a1,,ncol = 16,byrow = TRUE)
  a2.m<-matrix(test.a2,,ncol = 16,byrow = TRUE)
  a3.m<-matrix(test.a3,,ncol = 16,byrow = TRUE)
  a4.m<-matrix(test.a4,,ncol = 16,byrow = TRUE)
  a5.m<-matrix(test.a5,,ncol = 16,byrow = TRUE)
  b1.m<-matrix(test.b1,,ncol = 16,byrow = TRUE)
  b2.m<-matrix(test.b2,,ncol = 16,byrow = TRUE)
  
  test.f.a1<-data.frame(a1.m,a1,a2,a3,a4,a5)
  test.f.a2<-data.frame(a2.m,a1,a2,a3,a4,a5)
  test.f.a3<-data.frame(a3.m,a1,a2,a3,a4,a5)
  test.f.a4<-data.frame(a4.m,a1,a2,a3,a4,a5)
  test.f.a5<-data.frame(a5.m,a1,a2,a3,a4,a5)
  test.f.b1<-data.frame(b1.m,b1,b2)
  test.f.b2<-data.frame(b2.m,b1,b2)
  
  tests.T.a1<-Matrix(as.matrix(test.f.a1),sparse=T)
  tests.T.a2<-Matrix(as.matrix(test.f.a2),sparse=T)
  tests.T.a3<-Matrix(as.matrix(test.f.a3),sparse=T)
  tests.T.a4<-Matrix(as.matrix(test.f.a4),sparse=T)
  tests.T.a5<-Matrix(as.matrix(test.f.a5),sparse=T)
  tests.T.b1<-Matrix(as.matrix(test.f.b1),sparse=T)
  tests.T.b2<-Matrix(as.matrix(test.f.b2),sparse=T)
  
  
  testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.a1)
  testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.a2)
  testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.a3)
  testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.a4)
  testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.a5)
  testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.b1)
  testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.b2)
  
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


