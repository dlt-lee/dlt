source("element.R")
library(xgboost)
threads=detectCores()
number_of_core=threads/2

time_start<-Sys.time()
source("dlt_sum_L3.R")


m_r_ab_org<-as.matrix(read.csv(file = "l3_ab.csv", header = FALSE))[-1,]
m_r_ab_org<-m_r_ab_org[,-1]
row_result<-dim(m_r_ab_org)[1]-1
m_r_ab<-head(m_r_ab_org,row_result)
result<-tail(dlt,row_result)

m_record_l4<-c(
  20075,02,09,09,18,33,04,10,
  20076,02,11,19,27,29,02,08,
  20077,02,11,23,29,29,02,04,
  20078,06,15,17,19,34,04,11,
  20079,04,12,24,26,33,03,09,
  20080,05,14,17,26,29,08,10,
  20081,08,10,21,25,27,03,06,
  20082,02,10,16,23,30,06,09,
  20083,06,11,15,23,30,05,06,
  20084,06,13,16,21,28,04,05
)

trains.T.ab<-Matrix(m_r_ab,sparse=T)

if (threads <= 8) {
  bst.a1<-xgboost(data = trains.T.ab,label = result$a1,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  bst.a2<-xgboost(data = trains.T.ab,label = result$a2,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  bst.a3<-xgboost(data = trains.T.ab,label = result$a3,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  bst.a4<-xgboost(data = trains.T.ab,label = result$a4,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  bst.a5<-xgboost(data = trains.T.ab,label = result$a5,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  bst.b1<-xgboost(data = trains.T.ab,label = result$b1,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  bst.b2<-xgboost(data = trains.T.ab,label = result$b2,nrounds = 300,verbose=0,params = list(tree_method = 'hist'))
  
}else{
  bst.a1<-xgboost(data = trains.T.ab,label = result$a1,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a2<-xgboost(data = trains.T.ab,label = result$a2,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a3<-xgboost(data = trains.T.ab,label = result$a3,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a4<-xgboost(data = trains.T.ab,label = result$a4,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a5<-xgboost(data = trains.T.ab,label = result$a5,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.b1<-xgboost(data = trains.T.ab,label = result$b1,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.b2<-xgboost(data = trains.T.ab,label = result$b2,nrounds = 300,verbose=0,params = list(tree_method = 'hist',nthread=number_of_core))
  
}

tests.T.ab<-Matrix(tail(m_r_ab_org,1),sparse=T)
testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.ab)
testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.ab)
testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.ab)
testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.ab)
testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.ab)
testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.ab)
testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.ab)

sum_l4_dlt<-c(sort(c(round(testPredictions.a1),
                     round(testPredictions.a2),
                     round(testPredictions.a3),
                     round(testPredictions.a4),
                     round(testPredictions.a5)
)),
sort(c(round(testPredictions.b1),
       round(testPredictions.b2))))

time_end<-Sys.time()
time_dur<-time_end-time_start
time_dur

sum_l4_dlt<-c(max(dlt$n)+1,sum_l4_dlt)
sum_l4_dlt

