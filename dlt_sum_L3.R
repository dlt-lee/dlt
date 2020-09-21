library(HMM)
library(depmixS4)
library(matlab)
library(quantmod)
library(xgboost)
source("element.R")
source("dlt_sum_L2.R")
source("dlt_sum_L1.R")
source("dlt_sum_cum.R")
threads=detectCores()
number_of_core=threads/2

record_ab<-c(
  20066,09,15,18,26,28,06,07,
  20067,02,15,20,26,33,02,06
  
)

time_start<-Sys.time()

unit<-floor(dim(dlt)[1]/9/3)*3
L2_1<-dlt_sum_cum(dlt,unit*1)
L2_2<-dlt_sum_cum(dlt,unit*2)
L2_3<-dlt_sum_cum(dlt,unit*3)
L2_4<-dlt_sum_cum(dlt,unit*4)
L2_5<-dlt_sum_cum(dlt,unit*5)
L2_6<-dlt_sum_cum(dlt,unit*6)
L2_7<-dlt_sum_cum(dlt,unit*7)
L2_8<-dlt_sum_cum(dlt,unit*8)


sum_l2_ab_3<-dlt_sum_L2(dlt,3)
sum_l2_ab_6<-dlt_sum_L2(dlt,6)

sum_l2<-c(sum_l2_ab_3[1],L2_1[1],L2_2[1],L2_3[1],L2_4[1],L2_5[1],L2_6[1],L2_7[1],L2_8[1],sum_l2_ab_6[1],
          sum_l2_ab_3[2],L2_1[2],L2_2[2],L2_3[2],L2_4[2],L2_5[2],L2_6[2],L2_7[2],L2_8[2],sum_l2_ab_6[2],
          sum_l2_ab_3[3],L2_1[3],L2_2[3],L2_3[3],L2_4[3],L2_5[3],L2_6[3],L2_7[3],L2_8[3],sum_l2_ab_6[3],
          sum_l2_ab_3[4],L2_1[4],L2_2[4],L2_3[4],L2_4[4],L2_5[4],L2_6[4],L2_7[4],L2_8[4],sum_l2_ab_6[4],
          sum_l2_ab_3[5],L2_1[5],L2_2[5],L2_3[5],L2_4[5],L2_5[5],L2_6[5],L2_7[5],L2_8[5],sum_l2_ab_6[5],
          sum_l2_ab_3[6],L2_1[6],L2_2[6],L2_3[6],L2_4[6],L2_5[6],L2_6[6],L2_7[6],L2_8[6],sum_l2_ab_6[6],
          sum_l2_ab_3[7],L2_1[7],L2_2[7],L2_3[7],L2_4[7],L2_5[7],L2_6[7],L2_7[7],L2_8[7],sum_l2_ab_6[7])
m_sum_l2<-matrix(sum_l2,ncol = 10,byrow = TRUE)

m_r_ab_org<-as.matrix(read.csv(file = "l2_ab.csv", header = FALSE))[-1,]
m_r_ab<-m_r_ab_org[,-1]
m_r_a1<-m_r_ab[,01:10]
m_r_a2<-m_r_ab[,11:20]
m_r_a3<-m_r_ab[,21:30]
m_r_a4<-m_r_ab[,31:40]
m_r_a5<-m_r_ab[,41:50]
m_r_b1<-m_r_ab[,51:60]
m_r_b2<-m_r_ab[,61:70]

row_result<-dim(m_r_ab)[1]
result<-tail(dlt,row_result)

trains.T.a1<-Matrix(m_r_a1,sparse=T)
trains.T.a2<-Matrix(m_r_a2,sparse=T)
trains.T.a3<-Matrix(m_r_a3,sparse=T)
trains.T.a4<-Matrix(m_r_a4,sparse=T)
trains.T.a5<-Matrix(m_r_a5,sparse=T)
trains.T.b1<-Matrix(m_r_b1,sparse=T)
trains.T.b2<-Matrix(m_r_b2,sparse=T)


if (threads <= 8) {
  bst.a1<-xgboost(data = trains.T.a1,label = result$a1,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  bst.a2<-xgboost(data = trains.T.a2,label = result$a2,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  bst.a3<-xgboost(data = trains.T.a3,label = result$a3,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  bst.a4<-xgboost(data = trains.T.a4,label = result$a4,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  bst.a5<-xgboost(data = trains.T.a5,label = result$a5,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  bst.b1<-xgboost(data = trains.T.b1,label = result$b1,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  bst.b2<-xgboost(data = trains.T.b2,label = result$b2,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist'))
  
}else{
  bst.a1<-xgboost(data = trains.T.a1,label = result$a1,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a2<-xgboost(data = trains.T.a2,label = result$a2,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a3<-xgboost(data = trains.T.a3,label = result$a3,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a4<-xgboost(data = trains.T.a4,label = result$a4,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.a5<-xgboost(data = trains.T.a5,label = result$a5,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.b1<-xgboost(data = trains.T.b1,label = result$b1,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  bst.b2<-xgboost(data = trains.T.b2,label = result$b2,nrounds = 300,print_every_n = 300L,params = list(tree_method = 'hist',nthread=number_of_core))
  
}

tests.T<-Matrix(as.matrix(m_sum_l2),sparse=T)
testPredictions.a1 <- predict(object = bst.a1,newdata = t(tests.T[1,]))
testPredictions.a2 <- predict(object = bst.a2,newdata = t(tests.T[2,]))
testPredictions.a3 <- predict(object = bst.a3,newdata = t(tests.T[3,]))
testPredictions.a4 <- predict(object = bst.a4,newdata = t(tests.T[4,]))
testPredictions.a5 <- predict(object = bst.a5,newdata = t(tests.T[5,]))
testPredictions.b1 <- predict(object = bst.b1,newdata = t(tests.T[6,]))
testPredictions.b2 <- predict(object = bst.b2,newdata = t(tests.T[7,]))

sum_L3_ab<-c(sort(c(round(testPredictions.a1),
                    round(testPredictions.a2),
                    round(testPredictions.a3),
                    round(testPredictions.a4),
                    round(testPredictions.a5))),
             sort(c(round(testPredictions.b1),
                    round(testPredictions.b2))))

sum_l1_ab<-dlt_sum_L1(dlt)

time_end<-Sys.time()

#sum_l2_ab_3
#sum_l2_ab_6

#L2_1
#L2_2
#L2_3
#L2_4
#L2_5
#L2_6
#L2_7
#L2_8

sum_l2<-c(max(dlt$n)+1,sum_l2)
m_r_ab_delta<-rbind(m_r_ab_org,sum_l2)
write.csv(m_r_ab_delta, file = "l2_ab.csv",row.names = FALSE)

sum_L3_ab

time_dur<-time_end-time_start  
time_dur
 
#m_sum_l2
  



