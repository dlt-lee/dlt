dlt.xgb.clu.row <- function(){
  rows<-dim(dlt)[1]-273
  n<-300
  
  pre.ab<-0
  
  exp.a1<-0
  exp.a2<-0
  exp.a3<-0
  exp.a4<-0
  exp.a5<-0
  exp.b1<-0
  exp.b2<-0
  
  for(i in 1:90) {
    data<-head(dlt,rows)
    data<-tail(data,300)
    
    temp.data<-dlt.xgboost.clu(data)
    
    for (j in 1:16) {
      pre.ab<-c(pre.ab,temp.data[j,])
      
    }
    
    exp.a1<-c(exp.a1,dlt[rows+1,]$a1)
    exp.a2<-c(exp.a2,dlt[rows+1,]$a2)
    exp.a3<-c(exp.a3,dlt[rows+1,]$a3)
    exp.a4<-c(exp.a4,dlt[rows+1,]$a4)
    exp.a5<-c(exp.a5,dlt[rows+1,]$a5)
    exp.b1<-c(exp.b1,dlt[rows+1,]$b1)
    exp.b2<-c(exp.b2,dlt[rows+1,]$b2)
    
    rows<-rows+3
  }
    
  pre.ab<-pre.ab[-1]
    pre.m.ab<-matrix(pre.ab,ncol = 112,byrow = TRUE)
    
    trains.T.ab<-Matrix(as.matrix(pre.m.ab),sparse=T)
    
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
    
    pre.final.data<-dlt.xgboost.clu(tail(dlt,300))
    
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




