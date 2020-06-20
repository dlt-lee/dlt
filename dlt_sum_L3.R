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

time_dur<-time_end-time_start  
time_dur
 
sum_l2<-c(sum_l2_ab_3[1],L2_1[1],L2_2[1],L2_3[1],L2_4[1],L2_5[1],L2_6[1],L2_7[1],L2_8[1],sum_l2_ab_3[1],
          sum_l2_ab_3[2],L2_1[2],L2_2[2],L2_3[2],L2_4[2],L2_5[2],L2_6[2],L2_7[2],L2_8[2],sum_l2_ab_3[2],
          sum_l2_ab_3[3],L2_1[3],L2_2[3],L2_3[3],L2_4[3],L2_5[3],L2_6[3],L2_7[3],L2_8[3],sum_l2_ab_3[3],
          sum_l2_ab_3[4],L2_1[4],L2_2[4],L2_3[4],L2_4[4],L2_5[4],L2_6[4],L2_7[4],L2_8[4],sum_l2_ab_3[4],
          sum_l2_ab_3[5],L2_1[5],L2_2[5],L2_3[5],L2_4[5],L2_5[5],L2_6[5],L2_7[5],L2_8[5],sum_l2_ab_3[5],
          sum_l2_ab_3[6],L2_1[6],L2_2[6],L2_3[6],L2_4[6],L2_5[6],L2_6[6],L2_7[6],L2_8[6],sum_l2_ab_3[6],
          sum_l2_ab_3[7],L2_1[7],L2_2[7],L2_3[7],L2_4[7],L2_5[7],L2_6[7],L2_7[7],L2_8[7],sum_l2_ab_3[7])
m_sum_l2<-matrix(sum_l2,ncol = 10,byrow = TRUE)
m_sum_l2
#sort(table(m_sum_l2[1,]))
#sort(table(m_sum_l2[2,]))
#sort(table(m_sum_l2[3,]))
#sort(table(m_sum_l2[4,]))
#sort(table(m_sum_l2[5,]))
#sort(table(m_sum_l2[6,]))
#sort(table(m_sum_l2[7,]))

