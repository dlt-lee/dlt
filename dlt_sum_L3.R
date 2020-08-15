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
  20049,06,07,03,06,10,02,02,10,05,10,
        17,12,11,17,14,07,15,18,12,11,
        18,20,18,18,20,18,16,21,14,14,
        21,22,24,21,21,22,25,24,25,24,
        32,29,29,32,27,30,35,31,29,35,
        03,03,07,03,05,03,04,04,02,05,
        06,11,08,06,09,06,10,012,09,13,
  20050,06,07,13,06,07,05,08,05,08,07,
        12,14,14,12,16,13,14,09,15,09,
        12,17,18,12,16,15,24,19,19,15,
        25,22,19,25,25,28,25,21,21,27,
        32,34,31,32,33,31,33,33,32,32,
        02,04,06,02,02,04,05,07,05,03,
        08,08,09,08,09,07,11,09,07,10,
  20051,04,03,06,04,04,03,01,10,07,08,
        05,08,10,05,10,14,12,12,16,09,
        09,22,16,09,17,20,21,23,17,19,
        13,22,20,13,26,23,26,28,27,19,
        23,30,32,23,29,27,33,31,32,26,
        03,05,06,03,04,05,07,05,07,07,
        09,06,11,09,10,08,09,10,10,11,
  20052,03,03,07,06,02,04,05,10,05,09,
        12,12,08,11,11,12,07,13,11,14,
        18,19,14,18,14,20,13,18,15,14,
        26,24,19,19,23,25,19,23,25,24,
        31,31,29,32,30,28,32,28,28,33,
        04,03,06,04,03,05,07,06,05,05,
        06,05,06,09,06,08,08,06,09,05,
  20053,03,04,08,06,07,04,04,09,09,03,
        11,11,13,06,11,13,12,11,11,14,
        21,11,20,19,27,19,13,20,16,18,
        22,26,21,24,28,20,28,26,23,23,
        30,32,26,31,32,30,30,33,31,34,
        06,04,04,05,07,03,04,03,03,04,
        11,08,09,07,09,09,06,07,07,04,
  20054,03,05,03,06,03,07,08,04,05,05,
        10,09,07,14,15,09,09,06,09,07,
        12,13,21,14,19,16,11,24,23,10,
        18,26,22,18,24,26,24,25,28,16,
        32,27,27,27,33,28,30,30,31,31,
        05,05,03,05,06,02,07,04,03,03,
        08,07,09,06,08,11,09,08,06,08,
  20055,10,05,03,03,06,05,07,12,08,04,
        11,09,04,06,10,13,09,14,10,16,
        19,18,17,14,14,18,15,16,18,20,
        30,28,26,24,24,22,25,27,24,24,
        33,33,32,33,32,28,31,32,30,29,
        02,05,04,03,00,05,04,04,03,05,
        05,06,07,05,07,08,06,08,11,08,
  20056,07,03,06,08,12,08,06,07,08,04,
        10,16,13,14,14,11,08,08,12,13,
        16,20,16,21,24,20,21,14,21,14,
        21,20,18,28,27,24,24,25,30,28,
        28,30,30,29,29,29,34,32,35,35,
        02,04,06,04,05,03,06,05,06,07,
        10,09,08,09,07,09,07,10,09,10,
  20057,10,03,03,05,02,12,-2,06,07,06,
        12,11,10,14,08,16,06,06,15,16,
        19,13,20,16,15,19,17,19,23,18,
        21,29,24,18,27,23,27,22,25,22,
        23,32,26,28,35,31,29,29,32,30,
        05,05,02,03,05,05,04,04,04,02,
        10,10,11,11,08,08,10,09,09,07,
  20058,06,10,05,00,05,07,06,05,04,-1,
        12,10,11,12,15,15,11,12,07,13,
        13,18,18,19,16,20,11,17,23,20,
        24,19,29,25,28,27,27,26,32,24,
        31,25,31,33,32,33,31,29,34,33,
        05,04,04,04,05,03,05,03,03,04,
        07,10,06,08,08,07,10,13,09,07,
  20059,09,06,07,03,06,06,-1,07,09,13,
        12,14,09,03,11,11,13,11,13,16,
        20,16,19,11,24,19,20,19,21,24,
        21,19,23,25,26,19,28,29,21,29,
        29,28,26,31,28,29,32,30,30,31,
        06,04,03,06,06,06,05,04,04,04,
        08,08,08,07,11,07,09,07,07,09,
  20060,03,03,04,06,09,11,04,02,06,03,
        06,09,05,10,14,12,07,19,19,12,
        16,17,13,17,17,26,17,19,22,14,
        20,20,21,21,25,26,22,24,29,20,
        27,30,28,28,26,32,23,24,34,31,
        03,02,03,04,01,05,06,06,02,04,
        06,12,08,06,07,10,09,12,08,09,
  20061,06,05,04,12,00,02,02,04,07,08,
        11,08,15,16,09,09,10,15,11,10,
        12,16,25,20,23,17,17,20,25,16,
        27,18,25,20,24,18,18,22,26,19,
        31,33,30,31,28,31,29,31,33,28,
        07,04,05,04,03,07,04,04,03,07,
        10,07,06,07,06,07,09,11,09,09,
  20062,04,09,04,04,05,08,07,07,11,07,
        11,14,14,14,15,10,13,09,14,16,
        22,26,15,19,21,15,14,15,21,19,
        24,26,19,27,22,25,23,22,28,26,
        31,31,31,32,27,33,29,27,29,26,
        03,01,07,03,05,03,01,05,04,04,
        07,08,08,07,12,10,07,09,10,10,
  20063,07,02,03,05,07,11,04,00,05,04,
        10,10,16,14,13,14,14,05,18,11,
        19,12,19,20,14,19,19,21,19,19,
        19,21,19,23,14,26,23,26,26,21,
        32,29,20,33,21,26,31,28,33,28,
        06,08,09,08,05,05,05,04,04,07,
        11,09,09,10,07,11,09,10,11,10,
  20064,08,06,03,07,11,11,03,05,04,06,
        17,12,13,11,14,14,21,12,13,16,
        19,12,18,21,17,18,23,16,27,20,
        28,22,24,27,24,25,26,24,32,27,
        30,30,29,28,32,28,27,25,34,31,
        03,04,02,05,05,05,03,04,05,04,
        08,09,09,07,09,09,09,08,07,06,
  20065,05,05,03,07,09,10,08,06,06,04,
        06,10,09,12,11,14,12,10,15,16,
        19,14,17,19,17,15,15,16,19,25,
        22,18,17,23,27,18,29,24,27,27,
        33,25,29,28,32,32,31,34,32,35,
        04,01,06,01,05,04,05,01,04,04,
        11,06,06,11,08,08,10,08,11,06,
  20067,05,08,02,01,03,04,02,04,06,05,
        14,13,12,12,16,11,11,15,11,11,
        21,16,19,23,17,21,21,19,22,21,
        29,28,21,27,21,23,24,25,29,24,
        31,31,32,31,30,32,29,29,31,28,
        06,04,06,04,04,04,01,03,04,07,
        07,08,09,07,06,07,09,10,12,08,
  20068,03,07,06,04,07,03,06,10,09,06,
        10,13,11,05,10,08,12,12,10,12,
        12,21,21,17,19,18,17,20,18,17,
        24,22,06,25,22,25,24,21,23,24,
        31,32,28,31,32,26,28,29,26,33,
        06,02,03,00,03,07,06,03,06,06,
        07,04,10,08,10,09,06,06,06,10
  
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

m_r_ab<-matrix(record_ab,ncol = 71,byrow = TRUE)
m_r_ab<-m_r_ab[,-1]
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

sum_L3_ab<-c(round(testPredictions.a1),
             round(testPredictions.a2),
             round(testPredictions.a3),
             round(testPredictions.a4),
             round(testPredictions.a5),
             round(testPredictions.b1),
             round(testPredictions.b2))

sum_l1_ab<-dlt_sum_L1(dlt)

time_end<-Sys.time()

sum_l2_ab_3
sum_l2_ab_6

L2_1
L2_2
L2_3
L2_4
L2_5
L2_6
L2_7
L2_8

sum_L3_ab

time_dur<-time_end-time_start  
time_dur
 
m_sum_l2
sum_l2<-c(max(dlt$n)+1,sum_l2)
write.table(m_r_ab, file = "dlt_ab.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")
directory <-getwd()
t_ab<-read.table(file = "dlt_ab.txt",row.names = NULL)
