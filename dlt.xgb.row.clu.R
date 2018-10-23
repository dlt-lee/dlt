dlt.xgb.row.clu <- function(){
  rows<-dim(dlt)[1]-273
  n<-300
  
  pre.a1<-0
  pre.a2<-0
  pre.a3<-0
  pre.a4<-0
  pre.a5<-0
  pre.b1<-0
  pre.b2<-0
  
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
    
    temp.data<-dlt.xgboost.row(data)
    pre.a1<-c(pre.a1,temp.data$a1)
    pre.a2<-c(pre.a2,temp.data$a2)
    pre.a3<-c(pre.a3,temp.data$a3)
    pre.a4<-c(pre.a4,temp.data$a4)
    pre.a5<-c(pre.a5,temp.data$a5)
    pre.b1<-c(pre.b1,temp.data$b1)
    pre.b2<-c(pre.b2,temp.data$b2)
    
    exp.a1<-c(exp.a1,dlt[rows+1,]$a1)
    exp.a2<-c(exp.a2,dlt[rows+1,]$a2)
    exp.a3<-c(exp.a3,dlt[rows+1,]$a3)
    exp.a4<-c(exp.a4,dlt[rows+1,]$a4)
    exp.a5<-c(exp.a5,dlt[rows+1,]$a5)
    exp.b1<-c(exp.b1,dlt[rows+1,]$b1)
    exp.b2<-c(exp.b2,dlt[rows+1,]$b2)
    
    
    rows<-rows+3
    
  }
  
  pre.a1<-pre.a1[-1]
  pre.a2<-pre.a2[-1]
  pre.a3<-pre.a3[-1]
  pre.a4<-pre.a4[-1]
  pre.a5<-pre.a5[-1]
  pre.b1<-pre.b1[-1]
  pre.b2<-pre.b2[-1]
  
  exp.a1<-exp.a1[-1]
  exp.a2<-exp.a2[-1]
  exp.a3<-exp.a3[-1]
  exp.a4<-exp.a4[-1]
  exp.a5<-exp.a5[-1]
  exp.b1<-exp.b1[-1]
  exp.b2<-exp.b2[-1]
  
  #============================================================================
  
  pre.m.a1<-matrix(pre.a1,ncol = 16,byrow = TRUE)
  pre.m.a2<-matrix(pre.a2,ncol = 16,byrow = TRUE)
  pre.m.a3<-matrix(pre.a3,ncol = 16,byrow = TRUE)
  pre.m.a4<-matrix(pre.a4,ncol = 16,byrow = TRUE)
  pre.m.a5<-matrix(pre.a5,ncol = 16,byrow = TRUE)
  pre.m.b1<-matrix(pre.b1,ncol = 16,byrow = TRUE)
  pre.m.b2<-matrix(pre.b2,ncol = 16,byrow = TRUE)
  
  trains.T.a1<-Matrix(as.matrix(pre.m.a1[,1:16]),sparse=T)
  trains.T.a2<-Matrix(as.matrix(pre.m.a2[,1:16]),sparse=T)
  trains.T.a3<-Matrix(as.matrix(pre.m.a3[,1:16]),sparse=T)
  trains.T.a4<-Matrix(as.matrix(pre.m.a4[,1:16]),sparse=T)
  trains.T.a5<-Matrix(as.matrix(pre.m.a5[,1:16]),sparse=T)
  trains.T.b1<-Matrix(as.matrix(pre.m.b1[,1:16]),sparse=T)
  trains.T.b2<-Matrix(as.matrix(pre.m.b2[,1:16]),sparse=T)
  
  bst.a1 <- xgboost(data = trains.T.a1,label = exp.a1,nrounds = n,print_every_n = 300L)
  bst.a2 <- xgboost(data = trains.T.a2,label = exp.a2,nrounds = n,print_every_n = 300L)
  bst.a3 <- xgboost(data = trains.T.a3,label = exp.a3,nrounds = n,print_every_n = 300L)
  bst.a4 <- xgboost(data = trains.T.a4,label = exp.a4,nrounds = n,print_every_n = 300L)
  bst.a5 <- xgboost(data = trains.T.a5,label = exp.a5,nrounds = n,print_every_n = 300L)
  bst.b1 <- xgboost(data = trains.T.b1,label = exp.b1,nrounds = n,print_every_n = 300L)
  bst.b2 <- xgboost(data = trains.T.b2,label = exp.b2,nrounds = n,print_every_n = 300L)
  
  pre.final.data<-dlt.xgboost.row(tail(dlt,300))
  
  
 tests.T.a1<-Matrix(as.matrix(pre.final.data$a1.row),sparse=T)
  tests.T.a2<-Matrix(as.matrix(pre.final.data$a2.row),sparse=T)
  tests.T.a3<-Matrix(as.matrix(pre.final.data$a3.row),sparse=T)
  tests.T.a4<-Matrix(as.matrix(pre.final.data$a4.row),sparse=T)
  tests.T.a5<-Matrix(as.matrix(pre.final.data$a5.row),sparse=T)
  tests.T.b1<-Matrix(as.matrix(pre.final.data$b1.row),sparse=T)
  tests.T.b2<-Matrix(as.matrix(pre.final.data$b2.row),sparse=T)
  
  
  
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














