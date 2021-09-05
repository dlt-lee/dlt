dlt_L2<-function(num) {
  library(HMM)
  library(depmixS4)
  library(matlab)
  library(quantmod)
  library(xgboost)
  #source("element.R")
  source("dlt_sum_L2.R")
  source("dlt_sum_L1.R")
  source("dlt_sum_cum.R")
  #read data
  m_record_l2<-as.matrix(read.csv(file = "dlt_data_l2.csv", header = FALSE))[-1,]
  #get training data
  dlt<-dlt[dlt$n<=num,]
  
  unit_dlt<-floor(dim(dlt)[1]/9/3)*3
  L2_1_dlt<-dlt_sum_cum(dlt,unit_dlt*1)
  L2_2_dlt<-dlt_sum_cum(dlt,unit_dlt*2)
  L2_3_dlt<-dlt_sum_cum(dlt,unit_dlt*3)
  L2_4_dlt<-dlt_sum_cum(dlt,unit_dlt*4)
  L2_5_dlt<-dlt_sum_cum(dlt,unit_dlt*5)
  L2_6_dlt<-dlt_sum_cum(dlt,unit_dlt*6)
  L2_7_dlt<-dlt_sum_cum(dlt,unit_dlt*7)
  L2_8_dlt<-dlt_sum_cum(dlt,unit_dlt*8)
  
  sum_l2_dlt_3<-dlt_sum_L2(dlt,3)
  sum_l2_dlt_6<-dlt_sum_L2(dlt,6)
  
  
  sum_l2_dlt<-c(sum_l2_dlt_3[1],L2_1_dlt[1],L2_2_dlt[1],L2_3_dlt[1],L2_4_dlt[1],L2_5_dlt[1],L2_6_dlt[1],L2_7_dlt[1],L2_8_dlt[1],sum_l2_dlt_6[1],
                sum_l2_dlt_3[2],L2_1_dlt[2],L2_2_dlt[2],L2_3_dlt[2],L2_4_dlt[2],L2_5_dlt[2],L2_6_dlt[2],L2_7_dlt[2],L2_8_dlt[2],sum_l2_dlt_6[2],
                sum_l2_dlt_3[3],L2_1_dlt[3],L2_2_dlt[3],L2_3_dlt[3],L2_4_dlt[3],L2_5_dlt[3],L2_6_dlt[3],L2_7_dlt[3],L2_8_dlt[3],sum_l2_dlt_6[3],
                sum_l2_dlt_3[4],L2_1_dlt[4],L2_2_dlt[4],L2_3_dlt[4],L2_4_dlt[4],L2_5_dlt[4],L2_6_dlt[4],L2_7_dlt[4],L2_8_dlt[4],sum_l2_dlt_6[4],
                sum_l2_dlt_3[5],L2_1_dlt[5],L2_2_dlt[5],L2_3_dlt[5],L2_4_dlt[5],L2_5_dlt[5],L2_6_dlt[5],L2_7_dlt[5],L2_8_dlt[5],sum_l2_dlt_6[5],
                sum_l2_dlt_3[6],L2_1_dlt[6],L2_2_dlt[6],L2_3_dlt[6],L2_4_dlt[6],L2_5_dlt[6],L2_6_dlt[6],L2_7_dlt[6],L2_8_dlt[6],sum_l2_dlt_6[6],
                sum_l2_dlt_3[7],L2_1_dlt[7],L2_2_dlt[7],L2_3_dlt[7],L2_4_dlt[7],L2_5_dlt[7],L2_6_dlt[7],L2_7_dlt[7],L2_8_dlt[7],sum_l2_dlt_6[7])
  
  sum_l2<-c(max(dlt$n)+1,sum_l2_dlt)
  m_r_ab_delta<-rbind(m_record_l2,sum_l2)
  write.csv(m_r_ab_delta, file = "dlt_data_l2.csv",row.names = FALSE)
  
}