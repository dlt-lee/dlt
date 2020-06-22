dlt.XGB <- function(data,count) {
  library(xgboost)
  #Build data
  trains.a <-tail(data,count)[1:(count-1),]
  results<-tail(data,(count-1))
  tests.a<-tail(data,1)
  #Convert
  a_dlt<-Matrix(as.matrix(trains.a[,4:8]),sparse=T)
  t_dlt<-Matrix(as.matrix(tests.a[,4:8]),sparse=T)
  n=300
  #A1:
  bst.a1 <- xgboost(data = a_dlt,label = results$a1,nrounds = n,print_every_n = 300L)
  p.a1 <- predict(object = bst.a1,newdata = t(t_dlt))
  #A2:
  bst.a2 <- xgboost(data = a_dlt,label = results$a2,nrounds = n,print_every_n = 300L)
  p.a2 <- predict(object = bst.a2,newdata = t(t_dlt))
  #A3:
  bst.a3 <- xgboost(data = a_dlt,label = results$a3,nrounds = n,print_every_n = 300L)
  p.a3 <- predict(object = bst.a3,newdata = t(t_dlt))
  #A4:
  bst.a4 <- xgboost(data = a_dlt,label = results$a4,nrounds = n,print_every_n = 300L)
  p.a4 <- predict(object = bst.a4,newdata = t(t_dlt))
  #A5:
  bst.a5 <- xgboost(data = a_dlt,label = results$a5,nrounds = n,print_every_n = 300L)
  p.a5 <- predict(object = bst.a5,newdata = t(t_dlt))
  
  #Build Data
  trains.b <-tail(data,count)[1:(count-1),]
  results<-tail(data,(count-1))
  tests.b<-tail(data,1)
  #Convert
  #b_dlt<-dlt.convert(trains.b$n,trains.b[,9:10],2)[,2:13]
  #t_dlt<-dlt.convert(tests.b$n,tests.b[,9:10],2)[,2:13]
  b_dlt<-Matrix(as.matrix(trains.b[,9:10]),sparse=T)
  t_dlt<-Matrix(as.matrix(tests.b[,9:10]),sparse=T)
  m<-200
  #B1:
  bst.b1 <- xgboost(data = b_dlt,label = results$b1,nrounds = m,print_every_n = 300L)
  p.b1 <- predict(object = bst.b1,newdata = t(t_dlt))
  #B2:
  bst.b2 <- xgboost(data = b_dlt,label = results$b2,nrounds = m,print_every_n = 300L)
  p.b2 <- predict(object = bst.b2,newdata = t(t_dlt))
  
  p.a1<-mean(p.a1)
  p.a2<-mean(p.a2)
  p.a3<-mean(p.a3)
  p.a4<-mean(p.a4)
  p.a5<-mean(p.a5)
  result.a<-c(p.a1,p.a2,p.a3,p.a4,p.a5)
  p.b1
  p.b2
  result<-c(sort(result.a),mean(p.b1),mean(p.b2))
  barplot(result,main = "XGB")
  return(result)
  
  
  
}


