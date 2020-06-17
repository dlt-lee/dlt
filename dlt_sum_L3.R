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
  20046,07,06,07,02,05,05,
  09,07,09,13,16,10,
  16,15,16,21,24,16,
  20,22,20,28,27,25,
  30,30,30,29,34,31,
  01,07,01,04,01,03,
  06,09,06,09,09,10,
  20047,03,06,05,07,06,10,
  05,07,07,08,10,13,
  16,12,17,15,14,18,
  22,19,22,20,24,24,
  29,30,32,32,30,32,
  06,04,05,06,04,04,
  09,05,07,08,07,09,
  20048,07,06,06,05,06,04,
  08,09,13,15,13,17,
  21,10,20,17,19,20,
  22,22,23,24,25,28,
  31,27,28,31,30,31,
  04,05,04,03,03,05,
  10,08,08,03,11,08,
  20049,14,10,09,01,05,04,
  15,10,13,11,13,16,
  15,18,20,19,19,16,
  18,21,23,20,20,17,
  27,24,30,29,30,35,
  04,04,04,05,04,03,
  10,12,10,07,10,08,
  20050,06,12,14,07,14,05,
  15,14,15,07,18,09,
  19,22,16,17,19,17,
  26,23,28,17,22,22,
  27,28,32,33,32,30,
  02,05,03,07,06,04,
  05,08,11,07,11,07
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


sum_l2_ab<-dlt_sum_L2(dlt)
sum_l1_ab<-dlt_sum_L1(dlt)

time_end<-Sys.time()

sum_l2_ab

L2_1
L2_2
L2_3
L2_4
L2_5
L2_6
L2_7
L2_8

time_dur<-time_end-time_start  
time_dur
 
sum_l2<-c(sum_l2_ab[1],L2_1[1],L2_2[1],L2_3[1],L2_4[1],L2_5[1],L2_6[1],L2_7[1],L2_8[1],
          sum_l2_ab[2],L2_1[2],L2_2[2],L2_3[2],L2_4[2],L2_5[2],L2_6[2],L2_7[2],L2_8[2],
          sum_l2_ab[3],L2_1[3],L2_2[3],L2_3[3],L2_4[3],L2_5[3],L2_6[3],L2_7[3],L2_8[3],
          sum_l2_ab[4],L2_1[4],L2_2[4],L2_3[4],L2_4[4],L2_5[4],L2_6[4],L2_7[4],L2_8[4],
          sum_l2_ab[5],L2_1[5],L2_2[5],L2_3[5],L2_4[5],L2_5[5],L2_6[5],L2_7[5],L2_8[5],
          sum_l2_ab[6],L2_1[6],L2_2[6],L2_3[6],L2_4[6],L2_5[6],L2_6[6],L2_7[6],L2_8[6],
          sum_l2_ab[7],L2_1[7],L2_2[7],L2_3[7],L2_4[7],L2_5[7],L2_6[7],L2_7[7],L2_8[7])
m_sum_l2<-matrix(sum_l2,ncol = 9,byrow = TRUE)
m_sum_l2
sort(table(m_sum_l2[1,]))
sort(table(m_sum_l2[2,]))
sort(table(m_sum_l2[3,]))
sort(table(m_sum_l2[4,]))
sort(table(m_sum_l2[5,]))
sort(table(m_sum_l2[6,]))
sort(table(m_sum_l2[7,]))

