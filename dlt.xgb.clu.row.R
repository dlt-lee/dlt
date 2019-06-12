dlt.xgb.clu.row <- function(data){
  n<-300
  m<-length(data$pre.a1)/16
  pre.m.a1<-matrix(data$pre.a1,ncol = 16,byrow = TRUE)
  pre.m.a2<-matrix(data$pre.a2,ncol = 16,byrow = TRUE)
  pre.m.a3<-matrix(data$pre.a3,ncol = 16,byrow = TRUE)
  pre.m.a4<-matrix(data$pre.a4,ncol = 16,byrow = TRUE)
  pre.m.a5<-matrix(data$pre.a5,ncol = 16,byrow = TRUE)
  pre.m.b1<-matrix(data$pre.b1,ncol = 16,byrow = TRUE)
  pre.m.b2<-matrix(data$pre.b2,ncol = 16,byrow = TRUE)
  
  pre.m.ab<-cbind(pre.m.a1,pre.m.a2,pre.m.a3,pre.m.a4,pre.m.a5,pre.m.b1,pre.m.b2)
    
    trains.T.ab<-Matrix(pre.m.ab,sparse=T)
    
    
    
    bst.a1 <- xgboost(data = trains.T.ab,label = data$exp.a1[1:m],nrounds = n,print_every_n = 300L)
    bst.a2 <- xgboost(data = trains.T.ab,label = data$exp.a2[1:m],nrounds = n,print_every_n = 300L)
    bst.a3 <- xgboost(data = trains.T.ab,label = data$exp.a3[1:m],nrounds = n,print_every_n = 300L)
    bst.a4 <- xgboost(data = trains.T.ab,label = data$exp.a4[1:m],nrounds = n,print_every_n = 300L)
    bst.a5 <- xgboost(data = trains.T.ab,label = data$exp.a5[1:m],nrounds = n,print_every_n = 300L)
    bst.b1 <- xgboost(data = trains.T.ab,label = data$exp.b1[1:m],nrounds = n,print_every_n = 300L)
    bst.b2 <- xgboost(data = trains.T.ab,label = data$exp.b2[1:m],nrounds = n,print_every_n = 300L)
    
    data<-tail(dlt,300)
    data.tr<-dlt.data.filter(data)
    pre.final.data<-dlt.xgboost.clu(data,data.tr)
    
    test.ab<-0
    
    for (j in 1:16) {
      for (k in 1:7) {
        test.ab<-c(test.ab,pre.final.data[j,k])
        
      }
      
    }
    test.ab<-test.ab[-1]
  
    tests.T.ab<-Matrix(as.matrix(test.ab),sparse=T)
    
    testPredictions.a1 <- predict(object = bst.a1,newdata = t(tests.T.ab))
    testPredictions.a2 <- predict(object = bst.a2,newdata = t(tests.T.ab))
    testPredictions.a3 <- predict(object = bst.a3,newdata = t(tests.T.ab))
    testPredictions.a4 <- predict(object = bst.a4,newdata = t(tests.T.ab))
    testPredictions.a5 <- predict(object = bst.a5,newdata = t(tests.T.ab))
    testPredictions.b1 <- predict(object = bst.b1,newdata = t(tests.T.ab))
    testPredictions.b2 <- predict(object = bst.b2,newdata = t(tests.T.ab))
    
    return(c(round(testPredictions.a1),round(testPredictions.a2),
             round(testPredictions.a3),round(testPredictions.a4),
             round(testPredictions.a5),round(testPredictions.b1),
             round(testPredictions.b2)))
  
}




