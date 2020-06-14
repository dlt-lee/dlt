library(HMM)
library(depmixS4)
library(matlab)
library(quantmod)
library(xgboost)
source("element.R")
source("dlt_sum_L2.R")
source("dlt_sum_L1.R")
source("dlt_sum_cum.R")

record_ab<-c(
  20050,06,12,14,07,14,05,
  15,14,15,07,18,09,
  19,22,16,17,19,17,
  26,23,28,17,22,22,
  27,28,32,33,32,30,
  02,05,03,07,06,04,
  05,08,11,07,11,07
)

time_start<-Sys.time()

unit<-floor(dim(dlt)[1]/6/3)*3
L2_1<-dlt_sum_cum(dlt,unit*1)
L2_2<-dlt_sum_cum(dlt,unit*2)
L2_3<-dlt_sum_cum(dlt,unit*3)
L2_4<-dlt_sum_cum(dlt,unit*4)
L2_5<-dlt_sum_cum(dlt,unit*5)


sum_l2_ab<-dlt_sum_L2(dlt)
sum_l1_ab<-dlt_sum_L1(dlt)

time_end<-Sys.time()

sum_l2_ab

L2_1
L2_2
L2_3
L2_4
L2_5

time_dur<-time_end-time_start  
time_dur
 
c(sum_l2_ab[1],L2_1[1],L2_2[1],L2_3[1],L2_4[1],L2_5[1],
  sum_l2_ab[2],L2_1[2],L2_2[2],L2_3[2],L2_4[2],L2_5[2],
  sum_l2_ab[3],L2_1[3],L2_2[3],L2_3[3],L2_4[3],L2_5[3],
  sum_l2_ab[4],L2_1[4],L2_2[4],L2_3[4],L2_4[4],L2_5[4],
  sum_l2_ab[5],L2_1[5],L2_2[5],L2_3[5],L2_4[5],L2_5[5],
  sum_l2_ab[6],L2_1[6],L2_2[6],L2_3[6],L2_4[6],L2_5[6],
  sum_l2_ab[7],L2_1[7],L2_2[7],L2_3[7],L2_4[7],L2_5[7])