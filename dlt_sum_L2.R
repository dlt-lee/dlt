dlt_sum_L2<-function(data_org,n) {
  library('xgboost')
  library(quantmod)
  source("dlt_sum_L1.R")
  threads=detectCores()
  number_of_core=threads/2
  
  
  
  rows<-dim(data_org)[1]
  
  line<-rows-floor(rows/9)*n
  j<-1
  a1_temp<-c(0,0,0,0,0,0,0,0)
  a2_temp<-c(0,0,0,0,0,0,0,0)
  a3_temp<-c(0,0,0,0,0,0,0,0)
  a4_temp<-c(0,0,0,0,0,0,0,0)
  a5_temp<-c(0,0,0,0,0,0,0,0)
  b1_temp<-c(0,0,0,0,0,0,0,0)
  b2_temp<-c(0,0,0,0,0,0,0,0)
  res_temp<-c(0,0,0,0,0,0,0)
  
  

  for (i in 1:line) {
    data<-data_org[j:line,]
    temp<-dlt_sum_L1(data)
    a1_temp<-c(a1_temp,temp$a1)
    a2_temp<-c(a2_temp,temp$a2)
    a3_temp<-c(a3_temp,temp$a3)
    a4_temp<-c(a4_temp,temp$a4)
    a5_temp<-c(a5_temp,temp$a5)
    b1_temp<-c(b1_temp,temp$b1)
    b2_temp<-c(b2_temp,temp$b2)
    res_temp<-c(res_temp,data_org[line+1,4:10])
    j<-j+3
    line<-line+3
    if (line>=rows) {
      break
    }
  }
  

  a1_m<-matrix(a1_temp,ncol = 8,byrow = TRUE)[-1,]
  a2_m<-matrix(a2_temp,ncol = 8,byrow = TRUE)[-1,]
  a3_m<-matrix(a3_temp,ncol = 8,byrow = TRUE)[-1,]
  a4_m<-matrix(a4_temp,ncol = 8,byrow = TRUE)[-1,]
  a5_m<-matrix(a5_temp,ncol = 8,byrow = TRUE)[-1,]
  b1_m<-matrix(b1_temp,ncol = 8,byrow = TRUE)[-1,]
  b2_m<-matrix(b2_temp,ncol = 8,byrow = TRUE)[-1,]
  res_m<-matrix(res_temp,ncol = 7,byrow = TRUE)[-1,]
  
  trains.T.a1<-Matrix(a1_m,sparse=T)
  trains.T.a2<-Matrix(a2_m,sparse=T)
  trains.T.a3<-Matrix(a3_m,sparse=T)
  trains.T.a4<-Matrix(a4_m,sparse=T)
  trains.T.a5<-Matrix(a5_m,sparse=T)
  trains.T.b1<-Matrix(b1_m,sparse=T)
  trains.T.b2<-Matrix(b2_m,sparse=T)
  
  if (threads <= 8) {
    bst.a1<-xgboost(data = trains.T.a1,label = res_m[,1],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    bst.a2<-xgboost(data = trains.T.a2,label = res_m[,2],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    bst.a3<-xgboost(data = trains.T.a3,label = res_m[,3],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    bst.a4<-xgboost(data = trains.T.a4,label = res_m[,4],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    bst.a5<-xgboost(data = trains.T.a5,label = res_m[,5],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    bst.b1<-xgboost(data = trains.T.b1,label = res_m[,6],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    bst.b2<-xgboost(data = trains.T.b2,label = res_m[,7],nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
    
  }else{
    bst.a1<-xgboost(data = trains.T.a1,label = res_m[,1],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    bst.a2<-xgboost(data = trains.T.a2,label = res_m[,2],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    bst.a3<-xgboost(data = trains.T.a3,label = res_m[,3],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    bst.a4<-xgboost(data = trains.T.a4,label = res_m[,4],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    bst.a5<-xgboost(data = trains.T.a5,label = res_m[,5],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    bst.b1<-xgboost(data = trains.T.b1,label = res_m[,6],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    bst.b2<-xgboost(data = trains.T.b2,label = res_m[,7],nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
    
  }
  
  
  #prediect
  tests<-dlt_sum_L1(tail(data_org,floor(rows/9)*3))
  tests.T<-Matrix(as.matrix(tests),sparse=T)
  testPredictions.a1 <- predict(object = bst.a1,newdata = t(tests.T[,1]))
  testPredictions.a2 <- predict(object = bst.a2,newdata = t(tests.T[,2]))
  testPredictions.a3 <- predict(object = bst.a3,newdata = t(tests.T[,3]))
  testPredictions.a4 <- predict(object = bst.a4,newdata = t(tests.T[,4]))
  testPredictions.a5 <- predict(object = bst.a5,newdata = t(tests.T[,5]))
  testPredictions.b1 <- predict(object = bst.b1,newdata = t(tests.T[,6]))
  testPredictions.b2 <- predict(object = bst.b2,newdata = t(tests.T[,7]))
  
  
  pre_data<-c(round(tail(testPredictions.a1,1)),
              round(tail(testPredictions.a2,1)),
              round(tail(testPredictions.a3,1)),
              round(tail(testPredictions.a4,1)),
              round(tail(testPredictions.a5,1)),
              round(tail(testPredictions.b1,1)),
              round(tail(testPredictions.b2,1)))
  pre_data<-c(sort(pre_data[1:5]),sort(pre_data[6:7]))
  return(pre_data)
  
  
  
}



