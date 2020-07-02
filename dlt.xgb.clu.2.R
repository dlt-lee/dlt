dlt.xgb.clu.2<-function(trains,d_o) {
  library(xgboost)
  threads=detectCores()
  number_of_core=threads/2
  
  #trains<-dlt.data.reset(dlt)
  trains.T<-Matrix(as.matrix(trains[,1:63]),sparse=T)
  bst.a1<-xgboost(data = trains.T[,c(50,57)],label = trains$res.a1,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a2<-xgboost(data = trains.T[,c(51,58)],label = trains$res.a2,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a3<-xgboost(data = trains.T[,c(52,59)],label = trains$res.a3,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a4<-xgboost(data = trains.T[,c(53,60)],label = trains$res.a4,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a5<-xgboost(data = trains.T[,c(54,61)],label = trains$res.a5,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.b1<-xgboost(data = trains.T[,c(55,62)],label = trains$res.b1,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.b2<-xgboost(data = trains.T[,c(56,63)],label = trains$res.b2,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  
  #predoct
  rows<-dim(d_o)[1]
  a1.1<-d_o$a1[1:(rows-8)];a2.1<-d_o$a2[1:(rows-8)];a3.1<-d_o$a3[1:(rows-8)];a4.1<-d_o$a4[1:(rows-8)];a5.1<-d_o$a5[1:(rows-8)];b1.1<-d_o$b1[1:(rows-8)];b2.1<-d_o$b2[1:(rows-8)]
  a1.2<-d_o$a1[2:(rows-7)];a2.2<-d_o$a2[2:(rows-7)];a3.2<-d_o$a3[2:(rows-7)];a4.2<-d_o$a4[2:(rows-7)];a5.2<-d_o$a5[2:(rows-7)];b1.2<-d_o$b1[2:(rows-7)];b2.2<-d_o$b2[2:(rows-7)]
  a1.3<-d_o$a1[3:(rows-6)];a2.3<-d_o$a2[3:(rows-6)];a3.3<-d_o$a3[3:(rows-6)];a4.3<-d_o$a4[3:(rows-6)];a5.3<-d_o$a5[3:(rows-6)];b1.3<-d_o$b1[3:(rows-6)];b2.3<-d_o$b2[3:(rows-6)]
  a1.4<-d_o$a1[4:(rows-5)];a2.4<-d_o$a2[4:(rows-5)];a3.4<-d_o$a3[4:(rows-5)];a4.4<-d_o$a4[4:(rows-5)];a5.4<-d_o$a5[4:(rows-5)];b1.4<-d_o$b1[4:(rows-5)];b2.4<-d_o$b2[4:(rows-5)]
  a1.5<-d_o$a1[5:(rows-4)];a2.5<-d_o$a2[5:(rows-4)];a3.5<-d_o$a3[5:(rows-4)];a4.5<-d_o$a4[5:(rows-4)];a5.5<-d_o$a5[5:(rows-4)];b1.5<-d_o$b1[5:(rows-4)];b2.5<-d_o$b2[5:(rows-4)]
  a1.6<-d_o$a1[6:(rows-3)];a2.6<-d_o$a2[6:(rows-3)];a3.6<-d_o$a3[6:(rows-3)];a4.6<-d_o$a4[6:(rows-3)];a5.6<-d_o$a5[6:(rows-3)];b1.6<-d_o$b1[6:(rows-3)];b2.6<-d_o$b2[6:(rows-3)]
  a1.7<-d_o$a1[7:(rows-2)];a2.7<-d_o$a2[7:(rows-2)];a3.7<-d_o$a3[7:(rows-2)];a4.7<-d_o$a4[7:(rows-2)];a5.7<-d_o$a5[7:(rows-2)];b1.7<-d_o$b1[7:(rows-2)];b2.7<-d_o$b2[7:(rows-2)]
  a1.8<-d_o$a1[8:(rows-1)];a2.8<-d_o$a2[8:(rows-1)];a3.8<-d_o$a3[8:(rows-1)];a4.8<-d_o$a4[8:(rows-1)];a5.8<-d_o$a5[8:(rows-1)];b1.8<-d_o$b1[8:(rows-1)];b2.8<-d_o$b2[8:(rows-1)]
  a1.9<-d_o$a1[9:(rows-0)];a2.9<-d_o$a2[9:(rows-0)];a3.9<-d_o$a3[9:(rows-0)];a4.9<-d_o$a4[9:(rows-0)];a5.9<-d_o$a5[9:(rows-0)];b1.9<-d_o$b1[9:(rows-0)];b2.9<-d_o$b2[9:(rows-0)]
  
  tests<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                    a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                    a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                    a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                    a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                    a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                    a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                    a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                    a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9)
  tests.T<-Matrix(as.matrix(tests),sparse=T)
  testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T[,c(50,57)])
  testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T[,c(51,58)])
  testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T[,c(52,59)])
  testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T[,c(53,60)])
  testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T[,c(54,61)])
  testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T[,c(55,62)])
  testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T[,c(56,63)])
  
  
  #result
  return(c(round(tail(testPredictions.a1,1)),
           round(tail(testPredictions.a2,1)),
           round(tail(testPredictions.a3,1)),
           round(tail(testPredictions.a4,1)),
           round(tail(testPredictions.a5,1)),
           round(tail(testPredictions.b1,1)),
           round(tail(testPredictions.b2,1))
  ))
  
  
}

