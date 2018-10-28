
dlt.xgb.clu.clu <- function(data){
  n<-300
  #============================================================================
  
  pre.m.a1<-matrix(data$pre.a1,ncol = 16,byrow = TRUE)
  pre.m.a2<-matrix(data$pre.a2,ncol = 16,byrow = TRUE)
  pre.m.a3<-matrix(data$pre.a3,ncol = 16,byrow = TRUE)
  pre.m.a4<-matrix(data$pre.a4,ncol = 16,byrow = TRUE)
  pre.m.a5<-matrix(data$pre.a5,ncol = 16,byrow = TRUE)
  pre.m.b1<-matrix(data$pre.b1,ncol = 16,byrow = TRUE)
  pre.m.b2<-matrix(data$pre.b2,ncol = 16,byrow = TRUE)
  
  trains.T.a1<-Matrix(pre.m.a1,sparse=T)
  trains.T.a2<-Matrix(pre.m.a2,sparse=T)
  trains.T.a3<-Matrix(pre.m.a3,sparse=T)
  trains.T.a4<-Matrix(pre.m.a4,sparse=T)
  trains.T.a5<-Matrix(pre.m.a5,sparse=T)
  trains.T.b1<-Matrix(pre.m.b1,sparse=T)
  trains.T.b2<-Matrix(pre.m.b2,sparse=T)
  
  bst.a1 <- xgboost(data = trains.T.a1,label = data$exp.a1[1:90],nrounds = n,print_every_n = 300L)
  bst.a2 <- xgboost(data = trains.T.a2,label = data$exp.a2[1:90],nrounds = n,print_every_n = 300L)
  bst.a3 <- xgboost(data = trains.T.a3,label = data$exp.a3[1:90],nrounds = n,print_every_n = 300L)
  bst.a4 <- xgboost(data = trains.T.a4,label = data$exp.a4[1:90],nrounds = n,print_every_n = 300L)
  bst.a5 <- xgboost(data = trains.T.a5,label = data$exp.a5[1:90],nrounds = n,print_every_n = 300L)
  bst.b1 <- xgboost(data = trains.T.b1,label = data$exp.b1[1:90],nrounds = n,print_every_n = 300L)
  bst.b2 <- xgboost(data = trains.T.b2,label = data$exp.b2[1:90],nrounds = n,print_every_n = 300L)
  
  pre.final.data<-dlt.xgboost.clu(tail(dlt,300))
  
  
  tests.T.a1<-Matrix(as.matrix(pre.final.data$a1.clu),sparse=T)
  tests.T.a2<-Matrix(as.matrix(pre.final.data$a2.clu),sparse=T)
  tests.T.a3<-Matrix(as.matrix(pre.final.data$a3.clu),sparse=T)
  tests.T.a4<-Matrix(as.matrix(pre.final.data$a4.clu),sparse=T)
  tests.T.a5<-Matrix(as.matrix(pre.final.data$a5.clu),sparse=T)
  tests.T.b1<-Matrix(as.matrix(pre.final.data$b1.clu),sparse=T)
  tests.T.b2<-Matrix(as.matrix(pre.final.data$b2.clu),sparse=T)
  
  
  
  testPredictions.a1 <- predict(object = bst.a1,newdata = t(tests.T.a1))
  testPredictions.a2 <- predict(object = bst.a2,newdata = t(tests.T.a2))
  testPredictions.a3 <- predict(object = bst.a3,newdata = t(tests.T.a3))
  testPredictions.a4 <- predict(object = bst.a4,newdata = t(tests.T.a4))
  testPredictions.a5 <- predict(object = bst.a5,newdata = t(tests.T.a5))
  testPredictions.b1 <- predict(object = bst.b1,newdata = t(tests.T.b1))
  testPredictions.b2 <- predict(object = bst.b2,newdata = t(tests.T.b2))
  
  return(c(round(testPredictions.a1),round(testPredictions.a2),
           round(testPredictions.a3),round(testPredictions.a4),
           round(testPredictions.a5),round(testPredictions.b1),
           round(testPredictions.b2)))
}














