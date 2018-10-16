data<-dlt
count<-dim(dlt)[1]
dlt.clu.mda.VI <- function(data,count) {
  library(mda)
  library(caret)
  
  data.train<-head(data,count-2)
  rows<-dim(data.train)[1]
  row<-0
  row.line<-count-2
  for (i in 1:rows) {
    row<-c(row,row.line)
    row.line<-row.line-3
    if (row.line<=42) {
      break
    }
  }
  row<-sort(row[-1])
  data.test<-data
  
  #Trains_1
  trains_1_a1_temp<-0
  trains_1_a2_temp<-0
  trains_1_a3_temp<-0
  trains_1_a4_temp<-0
  trains_1_a5_temp<-0
  trains_1_b1_temp<-0
  trains_1_b2_temp<-0
  for (j in row) {
    trains_1_a1_temp<-c(trains_1_a1_temp,data.train[j-42,]$a1)
    trains_1_a2_temp<-c(trains_1_a2_temp,data.train[j-42,]$a2)
    trains_1_a3_temp<-c(trains_1_a3_temp,data.train[j-42,]$a3)
    trains_1_a4_temp<-c(trains_1_a4_temp,data.train[j-42,]$a4)
    trains_1_a5_temp<-c(trains_1_a5_temp,data.train[j-42,]$a5)
    trains_1_b1_temp<-c(trains_1_b1_temp,data.train[j-42,]$b1)
    trains_1_b2_temp<-c(trains_1_b2_temp,data.train[j-42,]$b2)
  }
  a1<-trains_1_a1_temp[-1]
  a2<-trains_1_a2_temp[-1]
  a3<-trains_1_a3_temp[-1]
  a4<-trains_1_a4_temp[-1]
  a5<-trains_1_a5_temp[-1]
  b1<-trains_1_b1_temp[-1]
  b2<-trains_1_b2_temp[-1]
  
  trains_1  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  
  #Trains_2
  trains_2_a1_temp<-0
  trains_2_a2_temp<-0
  trains_2_a3_temp<-0
  trains_2_a4_temp<-0
  trains_2_a5_temp<-0
  trains_2_b1_temp<-0
  trains_2_b2_temp<-0
  for (j in row) {
    trains_2_a1_temp<-c(trains_2_a1_temp,data.train[j-41,]$a1)
    trains_2_a2_temp<-c(trains_2_a2_temp,data.train[j-41,]$a2)
    trains_2_a3_temp<-c(trains_2_a3_temp,data.train[j-41,]$a3)
    trains_2_a4_temp<-c(trains_2_a4_temp,data.train[j-41,]$a4)
    trains_2_a5_temp<-c(trains_2_a5_temp,data.train[j-41,]$a5)
    trains_2_b1_temp<-c(trains_2_b1_temp,data.train[j-41,]$b1)
    trains_2_b2_temp<-c(trains_2_b2_temp,data.train[j-41,]$b2)
  }
  a1<-trains_2_a1_temp[-1]
  a2<-trains_2_a2_temp[-1]
  a3<-trains_2_a3_temp[-1]
  a4<-trains_2_a4_temp[-1]
  a5<-trains_2_a5_temp[-1]
  b1<-trains_2_b1_temp[-1]
  b2<-trains_2_b2_temp[-1]
  
  trains_2  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_3
  trains_3_a1_temp<-0
  trains_3_a2_temp<-0
  trains_3_a3_temp<-0
  trains_3_a4_temp<-0
  trains_3_a5_temp<-0
  trains_3_b1_temp<-0
  trains_3_b2_temp<-0
  for (j in row) {
    trains_3_a1_temp<-c(trains_3_a1_temp,data.train[j-40,]$a1)
    trains_3_a2_temp<-c(trains_3_a2_temp,data.train[j-40,]$a2)
    trains_3_a3_temp<-c(trains_3_a3_temp,data.train[j-40,]$a3)
    trains_3_a4_temp<-c(trains_3_a4_temp,data.train[j-40,]$a4)
    trains_3_a5_temp<-c(trains_3_a5_temp,data.train[j-40,]$a5)
    trains_3_b1_temp<-c(trains_3_b1_temp,data.train[j-40,]$b1)
    trains_3_b2_temp<-c(trains_3_b2_temp,data.train[j-40,]$b2)
  }
  a1<-trains_3_a1_temp[-1]
  a2<-trains_3_a2_temp[-1]
  a3<-trains_3_a3_temp[-1]
  a4<-trains_3_a4_temp[-1]
  a5<-trains_3_a5_temp[-1]
  b1<-trains_3_b1_temp[-1]
  b2<-trains_3_b2_temp[-1]
  
  trains_3  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_4
  trains_4_a1_temp<-0
  trains_4_a2_temp<-0
  trains_4_a3_temp<-0
  trains_4_a4_temp<-0
  trains_4_a5_temp<-0
  trains_4_b1_temp<-0
  trains_4_b2_temp<-0
  for (j in row) {
    trains_4_a1_temp<-c(trains_4_a1_temp,data.train[j-39,]$a1)
    trains_4_a2_temp<-c(trains_4_a2_temp,data.train[j-39,]$a2)
    trains_4_a3_temp<-c(trains_4_a3_temp,data.train[j-39,]$a3)
    trains_4_a4_temp<-c(trains_4_a4_temp,data.train[j-39,]$a4)
    trains_4_a5_temp<-c(trains_4_a5_temp,data.train[j-39,]$a5)
    trains_4_b1_temp<-c(trains_4_b1_temp,data.train[j-39,]$b1)
    trains_4_b2_temp<-c(trains_4_b2_temp,data.train[j-39,]$b2)
  }
  a1<-trains_4_a1_temp[-1]
  a2<-trains_4_a2_temp[-1]
  a3<-trains_4_a3_temp[-1]
  a4<-trains_4_a4_temp[-1]
  a5<-trains_4_a5_temp[-1]
  b1<-trains_4_b1_temp[-1]
  b2<-trains_4_b2_temp[-1]
  
  trains_4  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_5
  trains_5_a1_temp<-0
  trains_5_a2_temp<-0
  trains_5_a3_temp<-0
  trains_5_a4_temp<-0
  trains_5_a5_temp<-0
  trains_5_b1_temp<-0
  trains_5_b2_temp<-0
  for (j in row) {
    trains_5_a1_temp<-c(trains_5_a1_temp,data.train[j-38,]$a1)
    trains_5_a2_temp<-c(trains_5_a2_temp,data.train[j-38,]$a2)
    trains_5_a3_temp<-c(trains_5_a3_temp,data.train[j-38,]$a3)
    trains_5_a4_temp<-c(trains_5_a4_temp,data.train[j-38,]$a4)
    trains_5_a5_temp<-c(trains_5_a5_temp,data.train[j-38,]$a5)
    trains_5_b1_temp<-c(trains_5_b1_temp,data.train[j-38,]$b1)
    trains_5_b2_temp<-c(trains_5_b2_temp,data.train[j-38,]$b2)
  }
  a1<-trains_5_a1_temp[-1]
  a2<-trains_5_a2_temp[-1]
  a3<-trains_5_a3_temp[-1]
  a4<-trains_5_a4_temp[-1]
  a5<-trains_5_a5_temp[-1]
  b1<-trains_5_b1_temp[-1]
  b2<-trains_5_b2_temp[-1]
  
  trains_5  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_6
  trains_6_a1_temp<-0
  trains_6_a2_temp<-0
  trains_6_a3_temp<-0
  trains_6_a4_temp<-0
  trains_6_a5_temp<-0
  trains_6_b1_temp<-0
  trains_6_b2_temp<-0
  for (j in row) {
    trains_6_a1_temp<-c(trains_6_a1_temp,data.train[j-37,]$a1)
    trains_6_a2_temp<-c(trains_6_a2_temp,data.train[j-37,]$a2)
    trains_6_a3_temp<-c(trains_6_a3_temp,data.train[j-37,]$a3)
    trains_6_a4_temp<-c(trains_6_a4_temp,data.train[j-37,]$a4)
    trains_6_a5_temp<-c(trains_6_a5_temp,data.train[j-37,]$a5)
    trains_6_b1_temp<-c(trains_6_b1_temp,data.train[j-37,]$b1)
    trains_6_b2_temp<-c(trains_6_b2_temp,data.train[j-37,]$b2)
  }
  a1<-trains_6_a1_temp[-1]
  a2<-trains_6_a2_temp[-1]
  a3<-trains_6_a3_temp[-1]
  a4<-trains_6_a4_temp[-1]
  a5<-trains_6_a5_temp[-1]
  b1<-trains_6_b1_temp[-1]
  b2<-trains_6_b2_temp[-1]
  
  trains_6  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_7
  trains_7_a1_temp<-0
  trains_7_a2_temp<-0
  trains_7_a3_temp<-0
  trains_7_a4_temp<-0
  trains_7_a5_temp<-0
  trains_7_b1_temp<-0
  trains_7_b2_temp<-0
  for (j in row) {
    trains_7_a1_temp<-c(trains_7_a1_temp,data.train[j-36,]$a1)
    trains_7_a2_temp<-c(trains_7_a2_temp,data.train[j-36,]$a2)
    trains_7_a3_temp<-c(trains_7_a3_temp,data.train[j-36,]$a3)
    trains_7_a4_temp<-c(trains_7_a4_temp,data.train[j-36,]$a4)
    trains_7_a5_temp<-c(trains_7_a5_temp,data.train[j-36,]$a5)
    trains_7_b1_temp<-c(trains_7_b1_temp,data.train[j-36,]$b1)
    trains_7_b2_temp<-c(trains_7_b2_temp,data.train[j-36,]$b2)
  }
  a1<-trains_7_a1_temp[-1]
  a2<-trains_7_a2_temp[-1]
  a3<-trains_7_a3_temp[-1]
  a4<-trains_7_a4_temp[-1]
  a5<-trains_7_a5_temp[-1]
  b1<-trains_7_b1_temp[-1]
  b2<-trains_7_b2_temp[-1]
  
  trains_7  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_8
  trains_8_a1_temp<-0
  trains_8_a2_temp<-0
  trains_8_a3_temp<-0
  trains_8_a4_temp<-0
  trains_8_a5_temp<-0
  trains_8_b1_temp<-0
  trains_8_b2_temp<-0
  for (j in row) {
    trains_8_a1_temp<-c(trains_8_a1_temp,data.train[j-35,]$a1)
    trains_8_a2_temp<-c(trains_8_a2_temp,data.train[j-35,]$a2)
    trains_8_a3_temp<-c(trains_8_a3_temp,data.train[j-35,]$a3)
    trains_8_a4_temp<-c(trains_8_a4_temp,data.train[j-35,]$a4)
    trains_8_a5_temp<-c(trains_8_a5_temp,data.train[j-35,]$a5)
    trains_8_b1_temp<-c(trains_8_b1_temp,data.train[j-35,]$b1)
    trains_8_b2_temp<-c(trains_8_b2_temp,data.train[j-35,]$b2)
  }
  a1<-trains_8_a1_temp[-1]
  a2<-trains_8_a2_temp[-1]
  a3<-trains_8_a3_temp[-1]
  a4<-trains_8_a4_temp[-1]
  a5<-trains_8_a5_temp[-1]
  b1<-trains_8_b1_temp[-1]
  b2<-trains_8_b2_temp[-1]
  
  trains_8  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_9
  trains_9_a1_temp<-0
  trains_9_a2_temp<-0
  trains_9_a3_temp<-0
  trains_9_a4_temp<-0
  trains_9_a5_temp<-0
  trains_9_b1_temp<-0
  trains_9_b2_temp<-0
  for (j in row) {
    trains_9_a1_temp<-c(trains_9_a1_temp,data.train[j-34,]$a1)
    trains_9_a2_temp<-c(trains_9_a2_temp,data.train[j-34,]$a2)
    trains_9_a3_temp<-c(trains_9_a3_temp,data.train[j-34,]$a3)
    trains_9_a4_temp<-c(trains_9_a4_temp,data.train[j-34,]$a4)
    trains_9_a5_temp<-c(trains_9_a5_temp,data.train[j-34,]$a5)
    trains_9_b1_temp<-c(trains_9_b1_temp,data.train[j-34,]$b1)
    trains_9_b2_temp<-c(trains_9_b2_temp,data.train[j-34,]$b2)
  }
  a1<-trains_9_a1_temp[-1]
  a2<-trains_9_a2_temp[-1]
  a3<-trains_9_a3_temp[-1]
  a4<-trains_9_a4_temp[-1]
  a5<-trains_9_a5_temp[-1]
  b1<-trains_9_b1_temp[-1]
  b2<-trains_9_b2_temp[-1]
  
  trains_9  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_10
  trains_10_a1_temp<-0
  trains_10_a2_temp<-0
  trains_10_a3_temp<-0
  trains_10_a4_temp<-0
  trains_10_a5_temp<-0
  trains_10_b1_temp<-0
  trains_10_b2_temp<-0
  for (j in row) {
    trains_10_a1_temp<-c(trains_10_a1_temp,data.train[j-33,]$a1)
    trains_10_a2_temp<-c(trains_10_a2_temp,data.train[j-33,]$a2)
    trains_10_a3_temp<-c(trains_10_a3_temp,data.train[j-33,]$a3)
    trains_10_a4_temp<-c(trains_10_a4_temp,data.train[j-33,]$a4)
    trains_10_a5_temp<-c(trains_10_a5_temp,data.train[j-33,]$a5)
    trains_10_b1_temp<-c(trains_10_b1_temp,data.train[j-33,]$b1)
    trains_10_b2_temp<-c(trains_10_b2_temp,data.train[j-33,]$b2)
  }
  a1<-trains_10_a1_temp[-1]
  a2<-trains_10_a2_temp[-1]
  a3<-trains_10_a3_temp[-1]
  a4<-trains_10_a4_temp[-1]
  a5<-trains_10_a5_temp[-1]
  b1<-trains_10_b1_temp[-1]
  b2<-trains_10_b2_temp[-1]
  
  trains_10  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_11
  trains_11_a1_temp<-0
  trains_11_a2_temp<-0
  trains_11_a3_temp<-0
  trains_11_a4_temp<-0
  trains_11_a5_temp<-0
  trains_11_b1_temp<-0
  trains_11_b2_temp<-0
  for (j in row) {
    trains_11_a1_temp<-c(trains_11_a1_temp,data.train[j-32,]$a1)
    trains_11_a2_temp<-c(trains_11_a2_temp,data.train[j-32,]$a2)
    trains_11_a3_temp<-c(trains_11_a3_temp,data.train[j-32,]$a3)
    trains_11_a4_temp<-c(trains_11_a4_temp,data.train[j-32,]$a4)
    trains_11_a5_temp<-c(trains_11_a5_temp,data.train[j-32,]$a5)
    trains_11_b1_temp<-c(trains_11_b1_temp,data.train[j-32,]$b1)
    trains_11_b2_temp<-c(trains_11_b2_temp,data.train[j-32,]$b2)
  }
  a1<-trains_11_a1_temp[-1]
  a2<-trains_11_a2_temp[-1]
  a3<-trains_11_a3_temp[-1]
  a4<-trains_11_a4_temp[-1]
  a5<-trains_11_a5_temp[-1]
  b1<-trains_11_b1_temp[-1]
  b2<-trains_11_b2_temp[-1]
  
  trains_11  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_12
  trains_12_a1_temp<-0
  trains_12_a2_temp<-0
  trains_12_a3_temp<-0
  trains_12_a4_temp<-0
  trains_12_a5_temp<-0
  trains_12_b1_temp<-0
  trains_12_b2_temp<-0
  for (j in row) {
    trains_12_a1_temp<-c(trains_12_a1_temp,data.train[j-31,]$a1)
    trains_12_a2_temp<-c(trains_12_a2_temp,data.train[j-31,]$a2)
    trains_12_a3_temp<-c(trains_12_a3_temp,data.train[j-31,]$a3)
    trains_12_a4_temp<-c(trains_12_a4_temp,data.train[j-31,]$a4)
    trains_12_a5_temp<-c(trains_12_a5_temp,data.train[j-31,]$a5)
    trains_12_b1_temp<-c(trains_12_b1_temp,data.train[j-31,]$b1)
    trains_12_b2_temp<-c(trains_12_b2_temp,data.train[j-31,]$b2)
  }
  a1<-trains_12_a1_temp[-1]
  a2<-trains_12_a2_temp[-1]
  a3<-trains_12_a3_temp[-1]
  a4<-trains_12_a4_temp[-1]
  a5<-trains_12_a5_temp[-1]
  b1<-trains_12_b1_temp[-1]
  b2<-trains_12_b2_temp[-1]
  
  trains_12  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_13
  trains_13_a1_temp<-0
  trains_13_a2_temp<-0
  trains_13_a3_temp<-0
  trains_13_a4_temp<-0
  trains_13_a5_temp<-0
  trains_13_b1_temp<-0
  trains_13_b2_temp<-0
  for (j in row) {
    trains_13_a1_temp<-c(trains_13_a1_temp,data.train[j-30,]$a1)
    trains_13_a2_temp<-c(trains_13_a2_temp,data.train[j-30,]$a2)
    trains_13_a3_temp<-c(trains_13_a3_temp,data.train[j-30,]$a3)
    trains_13_a4_temp<-c(trains_13_a4_temp,data.train[j-30,]$a4)
    trains_13_a5_temp<-c(trains_13_a5_temp,data.train[j-30,]$a5)
    trains_13_b1_temp<-c(trains_13_b1_temp,data.train[j-30,]$b1)
    trains_13_b2_temp<-c(trains_13_b2_temp,data.train[j-30,]$b2)
  }
  a1<-trains_13_a1_temp[-1]
  a2<-trains_13_a2_temp[-1]
  a3<-trains_13_a3_temp[-1]
  a4<-trains_13_a4_temp[-1]
  a5<-trains_13_a5_temp[-1]
  b1<-trains_13_b1_temp[-1]
  b2<-trains_13_b2_temp[-1]
  
  trains_13  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_14
  trains_14_a1_temp<-0
  trains_14_a2_temp<-0
  trains_14_a3_temp<-0
  trains_14_a4_temp<-0
  trains_14_a5_temp<-0
  trains_14_b1_temp<-0
  trains_14_b2_temp<-0
  for (j in row) {
    trains_14_a1_temp<-c(trains_14_a1_temp,data.train[j-29,]$a1)
    trains_14_a2_temp<-c(trains_14_a2_temp,data.train[j-29,]$a2)
    trains_14_a3_temp<-c(trains_14_a3_temp,data.train[j-29,]$a3)
    trains_14_a4_temp<-c(trains_14_a4_temp,data.train[j-29,]$a4)
    trains_14_a5_temp<-c(trains_14_a5_temp,data.train[j-29,]$a5)
    trains_14_b1_temp<-c(trains_14_b1_temp,data.train[j-29,]$b1)
    trains_14_b2_temp<-c(trains_14_b2_temp,data.train[j-29,]$b2)
  }
  a1<-trains_14_a1_temp[-1]
  a2<-trains_14_a2_temp[-1]
  a3<-trains_14_a3_temp[-1]
  a4<-trains_14_a4_temp[-1]
  a5<-trains_14_a5_temp[-1]
  b1<-trains_14_b1_temp[-1]
  b2<-trains_14_b2_temp[-1]
  
  trains_14  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_15
  trains_15_a1_temp<-0
  trains_15_a2_temp<-0
  trains_15_a3_temp<-0
  trains_15_a4_temp<-0
  trains_15_a5_temp<-0
  trains_15_b1_temp<-0
  trains_15_b2_temp<-0
  for (j in row) {
    trains_15_a1_temp<-c(trains_15_a1_temp,data.train[j-28,]$a1)
    trains_15_a2_temp<-c(trains_15_a2_temp,data.train[j-28,]$a2)
    trains_15_a3_temp<-c(trains_15_a3_temp,data.train[j-28,]$a3)
    trains_15_a4_temp<-c(trains_15_a4_temp,data.train[j-28,]$a4)
    trains_15_a5_temp<-c(trains_15_a5_temp,data.train[j-28,]$a5)
    trains_15_b1_temp<-c(trains_15_b1_temp,data.train[j-28,]$b1)
    trains_15_b2_temp<-c(trains_15_b2_temp,data.train[j-28,]$b2)
  }
  a1<-trains_15_a1_temp[-1]
  a2<-trains_15_a2_temp[-1]
  a3<-trains_15_a3_temp[-1]
  a4<-trains_15_a4_temp[-1]
  a5<-trains_15_a5_temp[-1]
  b1<-trains_15_b1_temp[-1]
  b2<-trains_15_b2_temp[-1]
  
  trains_15  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_16
  trains_16_a1_temp<-0
  trains_16_a2_temp<-0
  trains_16_a3_temp<-0
  trains_16_a4_temp<-0
  trains_16_a5_temp<-0
  trains_16_b1_temp<-0
  trains_16_b2_temp<-0
  for (j in row) {
    trains_16_a1_temp<-c(trains_16_a1_temp,data.train[j-27,]$a1)
    trains_16_a2_temp<-c(trains_16_a2_temp,data.train[j-27,]$a2)
    trains_16_a3_temp<-c(trains_16_a3_temp,data.train[j-27,]$a3)
    trains_16_a4_temp<-c(trains_16_a4_temp,data.train[j-27,]$a4)
    trains_16_a5_temp<-c(trains_16_a5_temp,data.train[j-27,]$a5)
    trains_16_b1_temp<-c(trains_16_b1_temp,data.train[j-27,]$b1)
    trains_16_b2_temp<-c(trains_16_b2_temp,data.train[j-27,]$b2)
  }
  a1<-trains_16_a1_temp[-1]
  a2<-trains_16_a2_temp[-1]
  a3<-trains_16_a3_temp[-1]
  a4<-trains_16_a4_temp[-1]
  a5<-trains_16_a5_temp[-1]
  b1<-trains_16_b1_temp[-1]
  b2<-trains_16_b2_temp[-1]
  
  trains_16  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_17
  trains_17_a1_temp<-0
  trains_17_a2_temp<-0
  trains_17_a3_temp<-0
  trains_17_a4_temp<-0
  trains_17_a5_temp<-0
  trains_17_b1_temp<-0
  trains_17_b2_temp<-0
  for (j in row) {
    trains_17_a1_temp<-c(trains_17_a1_temp,data.train[j-26,]$a1)
    trains_17_a2_temp<-c(trains_17_a2_temp,data.train[j-26,]$a2)
    trains_17_a3_temp<-c(trains_17_a3_temp,data.train[j-26,]$a3)
    trains_17_a4_temp<-c(trains_17_a4_temp,data.train[j-26,]$a4)
    trains_17_a5_temp<-c(trains_17_a5_temp,data.train[j-26,]$a5)
    trains_17_b1_temp<-c(trains_17_b1_temp,data.train[j-26,]$b1)
    trains_17_b2_temp<-c(trains_17_b2_temp,data.train[j-26,]$b2)
  }
  a1<-trains_17_a1_temp[-1]
  a2<-trains_17_a2_temp[-1]
  a3<-trains_17_a3_temp[-1]
  a4<-trains_17_a4_temp[-1]
  a5<-trains_17_a5_temp[-1]
  b1<-trains_17_b1_temp[-1]
  b2<-trains_17_b2_temp[-1]
  
  trains_17  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_18
  trains_18_a1_temp<-0
  trains_18_a2_temp<-0
  trains_18_a3_temp<-0
  trains_18_a4_temp<-0
  trains_18_a5_temp<-0
  trains_18_b1_temp<-0
  trains_18_b2_temp<-0
  for (j in row) {
    trains_18_a1_temp<-c(trains_18_a1_temp,data.train[j-25,]$a1)
    trains_18_a2_temp<-c(trains_18_a2_temp,data.train[j-25,]$a2)
    trains_18_a3_temp<-c(trains_18_a3_temp,data.train[j-25,]$a3)
    trains_18_a4_temp<-c(trains_18_a4_temp,data.train[j-25,]$a4)
    trains_18_a5_temp<-c(trains_18_a5_temp,data.train[j-25,]$a5)
    trains_18_b1_temp<-c(trains_18_b1_temp,data.train[j-25,]$b1)
    trains_18_b2_temp<-c(trains_18_b2_temp,data.train[j-25,]$b2)
  }
  a1<-trains_18_a1_temp[-1]
  a2<-trains_18_a2_temp[-1]
  a3<-trains_18_a3_temp[-1]
  a4<-trains_18_a4_temp[-1]
  a5<-trains_18_a5_temp[-1]
  b1<-trains_18_b1_temp[-1]
  b2<-trains_18_b2_temp[-1]
  
  trains_18  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_19
  trains_19_a1_temp<-0
  trains_19_a2_temp<-0
  trains_19_a3_temp<-0
  trains_19_a4_temp<-0
  trains_19_a5_temp<-0
  trains_19_b1_temp<-0
  trains_19_b2_temp<-0
  for (j in row) {
    trains_19_a1_temp<-c(trains_19_a1_temp,data.train[j-24,]$a1)
    trains_19_a2_temp<-c(trains_19_a2_temp,data.train[j-24,]$a2)
    trains_19_a3_temp<-c(trains_19_a3_temp,data.train[j-24,]$a3)
    trains_19_a4_temp<-c(trains_19_a4_temp,data.train[j-24,]$a4)
    trains_19_a5_temp<-c(trains_19_a5_temp,data.train[j-24,]$a5)
    trains_19_b1_temp<-c(trains_19_b1_temp,data.train[j-24,]$b1)
    trains_19_b2_temp<-c(trains_19_b2_temp,data.train[j-24,]$b2)
  }
  a1<-trains_19_a1_temp[-1]
  a2<-trains_19_a2_temp[-1]
  a3<-trains_19_a3_temp[-1]
  a4<-trains_19_a4_temp[-1]
  a5<-trains_19_a5_temp[-1]
  b1<-trains_19_b1_temp[-1]
  b2<-trains_19_b2_temp[-1]
  
  trains_19  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_20
  trains_20_a1_temp<-0
  trains_20_a2_temp<-0
  trains_20_a3_temp<-0
  trains_20_a4_temp<-0
  trains_20_a5_temp<-0
  trains_20_b1_temp<-0
  trains_20_b2_temp<-0
  for (j in row) {
    trains_20_a1_temp<-c(trains_20_a1_temp,data.train[j-23,]$a1)
    trains_20_a2_temp<-c(trains_20_a2_temp,data.train[j-23,]$a2)
    trains_20_a3_temp<-c(trains_20_a3_temp,data.train[j-23,]$a3)
    trains_20_a4_temp<-c(trains_20_a4_temp,data.train[j-23,]$a4)
    trains_20_a5_temp<-c(trains_20_a5_temp,data.train[j-23,]$a5)
    trains_20_b1_temp<-c(trains_20_b1_temp,data.train[j-23,]$b1)
    trains_20_b2_temp<-c(trains_20_b2_temp,data.train[j-23,]$b2)
  }
  a1<-trains_20_a1_temp[-1]
  a2<-trains_20_a2_temp[-1]
  a3<-trains_20_a3_temp[-1]
  a4<-trains_20_a4_temp[-1]
  a5<-trains_20_a5_temp[-1]
  b1<-trains_20_b1_temp[-1]
  b2<-trains_20_b2_temp[-1]
  
  trains_20  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_21
  trains_21_a1_temp<-0
  trains_21_a2_temp<-0
  trains_21_a3_temp<-0
  trains_21_a4_temp<-0
  trains_21_a5_temp<-0
  trains_21_b1_temp<-0
  trains_21_b2_temp<-0
  for (j in row) {
    trains_21_a1_temp<-c(trains_21_a1_temp,data.train[j-22,]$a1)
    trains_21_a2_temp<-c(trains_21_a2_temp,data.train[j-22,]$a2)
    trains_21_a3_temp<-c(trains_21_a3_temp,data.train[j-22,]$a3)
    trains_21_a4_temp<-c(trains_21_a4_temp,data.train[j-22,]$a4)
    trains_21_a5_temp<-c(trains_21_a5_temp,data.train[j-22,]$a5)
    trains_21_b1_temp<-c(trains_21_b1_temp,data.train[j-22,]$b1)
    trains_21_b2_temp<-c(trains_21_b2_temp,data.train[j-22,]$b2)
  }
  a1<-trains_21_a1_temp[-1]
  a2<-trains_21_a2_temp[-1]
  a3<-trains_21_a3_temp[-1]
  a4<-trains_21_a4_temp[-1]
  a5<-trains_21_a5_temp[-1]
  b1<-trains_21_b1_temp[-1]
  b2<-trains_21_b2_temp[-1]
  
  trains_21  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_22
  trains_22_a1_temp<-0
  trains_22_a2_temp<-0
  trains_22_a3_temp<-0
  trains_22_a4_temp<-0
  trains_22_a5_temp<-0
  trains_22_b1_temp<-0
  trains_22_b2_temp<-0
  for (j in row) {
    trains_22_a1_temp<-c(trains_22_a1_temp,data.train[j-21,]$a1)
    trains_22_a2_temp<-c(trains_22_a2_temp,data.train[j-21,]$a2)
    trains_22_a3_temp<-c(trains_22_a3_temp,data.train[j-21,]$a3)
    trains_22_a4_temp<-c(trains_22_a4_temp,data.train[j-21,]$a4)
    trains_22_a5_temp<-c(trains_22_a5_temp,data.train[j-21,]$a5)
    trains_22_b1_temp<-c(trains_22_b1_temp,data.train[j-21,]$b1)
    trains_22_b2_temp<-c(trains_22_b2_temp,data.train[j-21,]$b2)
  }
  a1<-trains_22_a1_temp[-1]
  a2<-trains_22_a2_temp[-1]
  a3<-trains_22_a3_temp[-1]
  a4<-trains_22_a4_temp[-1]
  a5<-trains_22_a5_temp[-1]
  b1<-trains_22_b1_temp[-1]
  b2<-trains_22_b2_temp[-1]
  
  trains_22  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_23
  trains_23_a1_temp<-0
  trains_23_a2_temp<-0
  trains_23_a3_temp<-0
  trains_23_a4_temp<-0
  trains_23_a5_temp<-0
  trains_23_b1_temp<-0
  trains_23_b2_temp<-0
  for (j in row) {
    trains_23_a1_temp<-c(trains_23_a1_temp,data.train[j-20,]$a1)
    trains_23_a2_temp<-c(trains_23_a2_temp,data.train[j-20,]$a2)
    trains_23_a3_temp<-c(trains_23_a3_temp,data.train[j-20,]$a3)
    trains_23_a4_temp<-c(trains_23_a4_temp,data.train[j-20,]$a4)
    trains_23_a5_temp<-c(trains_23_a5_temp,data.train[j-20,]$a5)
    trains_23_b1_temp<-c(trains_23_b1_temp,data.train[j-20,]$b1)
    trains_23_b2_temp<-c(trains_23_b2_temp,data.train[j-20,]$b2)
  }
  a1<-trains_23_a1_temp[-1]
  a2<-trains_23_a2_temp[-1]
  a3<-trains_23_a3_temp[-1]
  a4<-trains_23_a4_temp[-1]
  a5<-trains_23_a5_temp[-1]
  b1<-trains_23_b1_temp[-1]
  b2<-trains_23_b2_temp[-1]
  
  trains_23  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_24
  trains_24_a1_temp<-0
  trains_24_a2_temp<-0
  trains_24_a3_temp<-0
  trains_24_a4_temp<-0
  trains_24_a5_temp<-0
  trains_24_b1_temp<-0
  trains_24_b2_temp<-0
  for (j in row) {
    trains_24_a1_temp<-c(trains_24_a1_temp,data.train[j-19,]$a1)
    trains_24_a2_temp<-c(trains_24_a2_temp,data.train[j-19,]$a2)
    trains_24_a3_temp<-c(trains_24_a3_temp,data.train[j-19,]$a3)
    trains_24_a4_temp<-c(trains_24_a4_temp,data.train[j-19,]$a4)
    trains_24_a5_temp<-c(trains_24_a5_temp,data.train[j-19,]$a5)
    trains_24_b1_temp<-c(trains_24_b1_temp,data.train[j-19,]$b1)
    trains_24_b2_temp<-c(trains_24_b2_temp,data.train[j-19,]$b2)
  }
  a1<-trains_24_a1_temp[-1]
  a2<-trains_24_a2_temp[-1]
  a3<-trains_24_a3_temp[-1]
  a4<-trains_24_a4_temp[-1]
  a5<-trains_24_a5_temp[-1]
  b1<-trains_24_b1_temp[-1]
  b2<-trains_24_b2_temp[-1]
  
  trains_24  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_25
  trains_25_a1_temp<-0
  trains_25_a2_temp<-0
  trains_25_a3_temp<-0
  trains_25_a4_temp<-0
  trains_25_a5_temp<-0
  trains_25_b1_temp<-0
  trains_25_b2_temp<-0
  for (j in row) {
    trains_25_a1_temp<-c(trains_25_a1_temp,data.train[j-18,]$a1)
    trains_25_a2_temp<-c(trains_25_a2_temp,data.train[j-18,]$a2)
    trains_25_a3_temp<-c(trains_25_a3_temp,data.train[j-18,]$a3)
    trains_25_a4_temp<-c(trains_25_a4_temp,data.train[j-18,]$a4)
    trains_25_a5_temp<-c(trains_25_a5_temp,data.train[j-18,]$a5)
    trains_25_b1_temp<-c(trains_25_b1_temp,data.train[j-18,]$b1)
    trains_25_b2_temp<-c(trains_25_b2_temp,data.train[j-18,]$b2)
  }
  a1<-trains_25_a1_temp[-1]
  a2<-trains_25_a2_temp[-1]
  a3<-trains_25_a3_temp[-1]
  a4<-trains_25_a4_temp[-1]
  a5<-trains_25_a5_temp[-1]
  b1<-trains_25_b1_temp[-1]
  b2<-trains_25_b2_temp[-1]
  
  trains_25  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_26
  trains_26_a1_temp<-0
  trains_26_a2_temp<-0
  trains_26_a3_temp<-0
  trains_26_a4_temp<-0
  trains_26_a5_temp<-0
  trains_26_b1_temp<-0
  trains_26_b2_temp<-0
  for (j in row) {
    trains_26_a1_temp<-c(trains_26_a1_temp,data.train[j-17,]$a1)
    trains_26_a2_temp<-c(trains_26_a2_temp,data.train[j-17,]$a2)
    trains_26_a3_temp<-c(trains_26_a3_temp,data.train[j-17,]$a3)
    trains_26_a4_temp<-c(trains_26_a4_temp,data.train[j-17,]$a4)
    trains_26_a5_temp<-c(trains_26_a5_temp,data.train[j-17,]$a5)
    trains_26_b1_temp<-c(trains_26_b1_temp,data.train[j-17,]$b1)
    trains_26_b2_temp<-c(trains_26_b2_temp,data.train[j-17,]$b2)
  }
  a1<-trains_26_a1_temp[-1]
  a2<-trains_26_a2_temp[-1]
  a3<-trains_26_a3_temp[-1]
  a4<-trains_26_a4_temp[-1]
  a5<-trains_26_a5_temp[-1]
  b1<-trains_26_b1_temp[-1]
  b2<-trains_26_b2_temp[-1]
  
  trains_26  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_27
  trains_27_a1_temp<-0
  trains_27_a2_temp<-0
  trains_27_a3_temp<-0
  trains_27_a4_temp<-0
  trains_27_a5_temp<-0
  trains_27_b1_temp<-0
  trains_27_b2_temp<-0
  for (j in row) {
    trains_27_a1_temp<-c(trains_27_a1_temp,data.train[j-16,]$a1)
    trains_27_a2_temp<-c(trains_27_a2_temp,data.train[j-16,]$a2)
    trains_27_a3_temp<-c(trains_27_a3_temp,data.train[j-16,]$a3)
    trains_27_a4_temp<-c(trains_27_a4_temp,data.train[j-16,]$a4)
    trains_27_a5_temp<-c(trains_27_a5_temp,data.train[j-16,]$a5)
    trains_27_b1_temp<-c(trains_27_b1_temp,data.train[j-16,]$b1)
    trains_27_b2_temp<-c(trains_27_b2_temp,data.train[j-16,]$b2)
  }
  a1<-trains_27_a1_temp[-1]
  a2<-trains_27_a2_temp[-1]
  a3<-trains_27_a3_temp[-1]
  a4<-trains_27_a4_temp[-1]
  a5<-trains_27_a5_temp[-1]
  b1<-trains_27_b1_temp[-1]
  b2<-trains_27_b2_temp[-1]
  
  trains_27  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_28
  trains_28_a1_temp<-0
  trains_28_a2_temp<-0
  trains_28_a3_temp<-0
  trains_28_a4_temp<-0
  trains_28_a5_temp<-0
  trains_28_b1_temp<-0
  trains_28_b2_temp<-0
  for (j in row) {
    trains_28_a1_temp<-c(trains_28_a1_temp,data.train[j-15,]$a1)
    trains_28_a2_temp<-c(trains_28_a2_temp,data.train[j-15,]$a2)
    trains_28_a3_temp<-c(trains_28_a3_temp,data.train[j-15,]$a3)
    trains_28_a4_temp<-c(trains_28_a4_temp,data.train[j-15,]$a4)
    trains_28_a5_temp<-c(trains_28_a5_temp,data.train[j-15,]$a5)
    trains_28_b1_temp<-c(trains_28_b1_temp,data.train[j-15,]$b1)
    trains_28_b2_temp<-c(trains_28_b2_temp,data.train[j-15,]$b2)
  }
  a1<-trains_28_a1_temp[-1]
  a2<-trains_28_a2_temp[-1]
  a3<-trains_28_a3_temp[-1]
  a4<-trains_28_a4_temp[-1]
  a5<-trains_28_a5_temp[-1]
  b1<-trains_28_b1_temp[-1]
  b2<-trains_28_b2_temp[-1]
  
  trains_28  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_29
  trains_29_a1_temp<-0
  trains_29_a2_temp<-0
  trains_29_a3_temp<-0
  trains_29_a4_temp<-0
  trains_29_a5_temp<-0
  trains_29_b1_temp<-0
  trains_29_b2_temp<-0
  for (j in row) {
    trains_29_a1_temp<-c(trains_29_a1_temp,data.train[j-14,]$a1)
    trains_29_a2_temp<-c(trains_29_a2_temp,data.train[j-14,]$a2)
    trains_29_a3_temp<-c(trains_29_a3_temp,data.train[j-14,]$a3)
    trains_29_a4_temp<-c(trains_29_a4_temp,data.train[j-14,]$a4)
    trains_29_a5_temp<-c(trains_29_a5_temp,data.train[j-14,]$a5)
    trains_29_b1_temp<-c(trains_29_b1_temp,data.train[j-14,]$b1)
    trains_29_b2_temp<-c(trains_29_b2_temp,data.train[j-14,]$b2)
  }
  a1<-trains_29_a1_temp[-1]
  a2<-trains_29_a2_temp[-1]
  a3<-trains_29_a3_temp[-1]
  a4<-trains_29_a4_temp[-1]
  a5<-trains_29_a5_temp[-1]
  b1<-trains_29_b1_temp[-1]
  b2<-trains_29_b2_temp[-1]
  
  trains_29  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_30
  trains_30_a1_temp<-0
  trains_30_a2_temp<-0
  trains_30_a3_temp<-0
  trains_30_a4_temp<-0
  trains_30_a5_temp<-0
  trains_30_b1_temp<-0
  trains_30_b2_temp<-0
  for (j in row) {
    trains_30_a1_temp<-c(trains_30_a1_temp,data.train[j-13,]$a1)
    trains_30_a2_temp<-c(trains_30_a2_temp,data.train[j-13,]$a2)
    trains_30_a3_temp<-c(trains_30_a3_temp,data.train[j-13,]$a3)
    trains_30_a4_temp<-c(trains_30_a4_temp,data.train[j-13,]$a4)
    trains_30_a5_temp<-c(trains_30_a5_temp,data.train[j-13,]$a5)
    trains_30_b1_temp<-c(trains_30_b1_temp,data.train[j-13,]$b1)
    trains_30_b2_temp<-c(trains_30_b2_temp,data.train[j-13,]$b2)
  }
  a1<-trains_30_a1_temp[-1]
  a2<-trains_30_a2_temp[-1]
  a3<-trains_30_a3_temp[-1]
  a4<-trains_30_a4_temp[-1]
  a5<-trains_30_a5_temp[-1]
  b1<-trains_30_b1_temp[-1]
  b2<-trains_30_b2_temp[-1]
  
  trains_30  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_31
  trains_31_a1_temp<-0
  trains_31_a2_temp<-0
  trains_31_a3_temp<-0
  trains_31_a4_temp<-0
  trains_31_a5_temp<-0
  trains_31_b1_temp<-0
  trains_31_b2_temp<-0
  for (j in row) {
    trains_31_a1_temp<-c(trains_31_a1_temp,data.train[j-12,]$a1)
    trains_31_a2_temp<-c(trains_31_a2_temp,data.train[j-12,]$a2)
    trains_31_a3_temp<-c(trains_31_a3_temp,data.train[j-12,]$a3)
    trains_31_a4_temp<-c(trains_31_a4_temp,data.train[j-12,]$a4)
    trains_31_a5_temp<-c(trains_31_a5_temp,data.train[j-12,]$a5)
    trains_31_b1_temp<-c(trains_31_b1_temp,data.train[j-12,]$b1)
    trains_31_b2_temp<-c(trains_31_b2_temp,data.train[j-12,]$b2)
  }
  a1<-trains_31_a1_temp[-1]
  a2<-trains_31_a2_temp[-1]
  a3<-trains_31_a3_temp[-1]
  a4<-trains_31_a4_temp[-1]
  a5<-trains_31_a5_temp[-1]
  b1<-trains_31_b1_temp[-1]
  b2<-trains_31_b2_temp[-1]
  
  trains_31  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_32
  trains_32_a1_temp<-0
  trains_32_a2_temp<-0
  trains_32_a3_temp<-0
  trains_32_a4_temp<-0
  trains_32_a5_temp<-0
  trains_32_b1_temp<-0
  trains_32_b2_temp<-0
  for (j in row) {
    trains_32_a1_temp<-c(trains_32_a1_temp,data.train[j-11,]$a1)
    trains_32_a2_temp<-c(trains_32_a2_temp,data.train[j-11,]$a2)
    trains_32_a3_temp<-c(trains_32_a3_temp,data.train[j-11,]$a3)
    trains_32_a4_temp<-c(trains_32_a4_temp,data.train[j-11,]$a4)
    trains_32_a5_temp<-c(trains_32_a5_temp,data.train[j-11,]$a5)
    trains_32_b1_temp<-c(trains_32_b1_temp,data.train[j-11,]$b1)
    trains_32_b2_temp<-c(trains_32_b2_temp,data.train[j-11,]$b2)
  }
  a1<-trains_32_a1_temp[-1]
  a2<-trains_32_a2_temp[-1]
  a3<-trains_32_a3_temp[-1]
  a4<-trains_32_a4_temp[-1]
  a5<-trains_32_a5_temp[-1]
  b1<-trains_32_b1_temp[-1]
  b2<-trains_32_b2_temp[-1]
  
  trains_32  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_33
  trains_33_a1_temp<-0
  trains_33_a2_temp<-0
  trains_33_a3_temp<-0
  trains_33_a4_temp<-0
  trains_33_a5_temp<-0
  trains_33_b1_temp<-0
  trains_33_b2_temp<-0
  for (j in row) {
    trains_33_a1_temp<-c(trains_33_a1_temp,data.train[j-10,]$a1)
    trains_33_a2_temp<-c(trains_33_a2_temp,data.train[j-10,]$a2)
    trains_33_a3_temp<-c(trains_33_a3_temp,data.train[j-10,]$a3)
    trains_33_a4_temp<-c(trains_33_a4_temp,data.train[j-10,]$a4)
    trains_33_a5_temp<-c(trains_33_a5_temp,data.train[j-10,]$a5)
    trains_33_b1_temp<-c(trains_33_b1_temp,data.train[j-10,]$b1)
    trains_33_b2_temp<-c(trains_33_b2_temp,data.train[j-10,]$b2)
  }
  a1<-trains_33_a1_temp[-1]
  a2<-trains_33_a2_temp[-1]
  a3<-trains_33_a3_temp[-1]
  a4<-trains_33_a4_temp[-1]
  a5<-trains_33_a5_temp[-1]
  b1<-trains_33_b1_temp[-1]
  b2<-trains_33_b2_temp[-1]
  
  trains_33  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_34
  trains_34_a1_temp<-0
  trains_34_a2_temp<-0
  trains_34_a3_temp<-0
  trains_34_a4_temp<-0
  trains_34_a5_temp<-0
  trains_34_b1_temp<-0
  trains_34_b2_temp<-0
  for (j in row) {
    trains_34_a1_temp<-c(trains_34_a1_temp,data.train[j-9,]$a1)
    trains_34_a2_temp<-c(trains_34_a2_temp,data.train[j-9,]$a2)
    trains_34_a3_temp<-c(trains_34_a3_temp,data.train[j-9,]$a3)
    trains_34_a4_temp<-c(trains_34_a4_temp,data.train[j-9,]$a4)
    trains_34_a5_temp<-c(trains_34_a5_temp,data.train[j-9,]$a5)
    trains_34_b1_temp<-c(trains_34_b1_temp,data.train[j-9,]$b1)
    trains_34_b2_temp<-c(trains_34_b2_temp,data.train[j-9,]$b2)
  }
  a1<-trains_34_a1_temp[-1]
  a2<-trains_34_a2_temp[-1]
  a3<-trains_34_a3_temp[-1]
  a4<-trains_34_a4_temp[-1]
  a5<-trains_34_a5_temp[-1]
  b1<-trains_34_b1_temp[-1]
  b2<-trains_34_b2_temp[-1]
  
  trains_34  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_35
  trains_35_a1_temp<-0
  trains_35_a2_temp<-0
  trains_35_a3_temp<-0
  trains_35_a4_temp<-0
  trains_35_a5_temp<-0
  trains_35_b1_temp<-0
  trains_35_b2_temp<-0
  for (j in row) {
    trains_35_a1_temp<-c(trains_35_a1_temp,data.train[j-8,]$a1)
    trains_35_a2_temp<-c(trains_35_a2_temp,data.train[j-8,]$a2)
    trains_35_a3_temp<-c(trains_35_a3_temp,data.train[j-8,]$a3)
    trains_35_a4_temp<-c(trains_35_a4_temp,data.train[j-8,]$a4)
    trains_35_a5_temp<-c(trains_35_a5_temp,data.train[j-8,]$a5)
    trains_35_b1_temp<-c(trains_35_b1_temp,data.train[j-8,]$b1)
    trains_35_b2_temp<-c(trains_35_b2_temp,data.train[j-8,]$b2)
  }
  a1<-trains_35_a1_temp[-1]
  a2<-trains_35_a2_temp[-1]
  a3<-trains_35_a3_temp[-1]
  a4<-trains_35_a4_temp[-1]
  a5<-trains_35_a5_temp[-1]
  b1<-trains_35_b1_temp[-1]
  b2<-trains_35_b2_temp[-1]
  
  trains_35  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_36
  trains_36_a1_temp<-0
  trains_36_a2_temp<-0
  trains_36_a3_temp<-0
  trains_36_a4_temp<-0
  trains_36_a5_temp<-0
  trains_36_b1_temp<-0
  trains_36_b2_temp<-0
  for (j in row) {
    trains_36_a1_temp<-c(trains_36_a1_temp,data.train[j-7,]$a1)
    trains_36_a2_temp<-c(trains_36_a2_temp,data.train[j-7,]$a2)
    trains_36_a3_temp<-c(trains_36_a3_temp,data.train[j-7,]$a3)
    trains_36_a4_temp<-c(trains_36_a4_temp,data.train[j-7,]$a4)
    trains_36_a5_temp<-c(trains_36_a5_temp,data.train[j-7,]$a5)
    trains_36_b1_temp<-c(trains_36_b1_temp,data.train[j-7,]$b1)
    trains_36_b2_temp<-c(trains_36_b2_temp,data.train[j-7,]$b2)
  }
  a1<-trains_36_a1_temp[-1]
  a2<-trains_36_a2_temp[-1]
  a3<-trains_36_a3_temp[-1]
  a4<-trains_36_a4_temp[-1]
  a5<-trains_36_a5_temp[-1]
  b1<-trains_36_b1_temp[-1]
  b2<-trains_36_b2_temp[-1]
  
  trains_36  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_37
  trains_37_a1_temp<-0
  trains_37_a2_temp<-0
  trains_37_a3_temp<-0
  trains_37_a4_temp<-0
  trains_37_a5_temp<-0
  trains_37_b1_temp<-0
  trains_37_b2_temp<-0
  for (j in row) {
    trains_37_a1_temp<-c(trains_37_a1_temp,data.train[j-6,]$a1)
    trains_37_a2_temp<-c(trains_37_a2_temp,data.train[j-6,]$a2)
    trains_37_a3_temp<-c(trains_37_a3_temp,data.train[j-6,]$a3)
    trains_37_a4_temp<-c(trains_37_a4_temp,data.train[j-6,]$a4)
    trains_37_a5_temp<-c(trains_37_a5_temp,data.train[j-6,]$a5)
    trains_37_b1_temp<-c(trains_37_b1_temp,data.train[j-6,]$b1)
    trains_37_b2_temp<-c(trains_37_b2_temp,data.train[j-6,]$b2)
  }
  a1<-trains_37_a1_temp[-1]
  a2<-trains_37_a2_temp[-1]
  a3<-trains_37_a3_temp[-1]
  a4<-trains_37_a4_temp[-1]
  a5<-trains_37_a5_temp[-1]
  b1<-trains_37_b1_temp[-1]
  b2<-trains_37_b2_temp[-1]
  
  trains_37  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_38
  trains_38_a1_temp<-0
  trains_38_a2_temp<-0
  trains_38_a3_temp<-0
  trains_38_a4_temp<-0
  trains_38_a5_temp<-0
  trains_38_b1_temp<-0
  trains_38_b2_temp<-0
  for (j in row) {
    trains_38_a1_temp<-c(trains_38_a1_temp,data.train[j-5,]$a1)
    trains_38_a2_temp<-c(trains_38_a2_temp,data.train[j-5,]$a2)
    trains_38_a3_temp<-c(trains_38_a3_temp,data.train[j-5,]$a3)
    trains_38_a4_temp<-c(trains_38_a4_temp,data.train[j-5,]$a4)
    trains_38_a5_temp<-c(trains_38_a5_temp,data.train[j-5,]$a5)
    trains_38_b1_temp<-c(trains_38_b1_temp,data.train[j-5,]$b1)
    trains_38_b2_temp<-c(trains_38_b2_temp,data.train[j-5,]$b2)
  }
  a1<-trains_38_a1_temp[-1]
  a2<-trains_38_a2_temp[-1]
  a3<-trains_38_a3_temp[-1]
  a4<-trains_38_a4_temp[-1]
  a5<-trains_38_a5_temp[-1]
  b1<-trains_38_b1_temp[-1]
  b2<-trains_38_b2_temp[-1]
  
  trains_38  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  
  #Trains_39
  trains_39_a1_temp<-0
  trains_39_a2_temp<-0
  trains_39_a3_temp<-0
  trains_39_a4_temp<-0
  trains_39_a5_temp<-0
  trains_39_b1_temp<-0
  trains_39_b2_temp<-0
  for (j in row) {
    trains_39_a1_temp<-c(trains_39_a1_temp,data.train[j-4,]$a1)
    trains_39_a2_temp<-c(trains_39_a2_temp,data.train[j-4,]$a2)
    trains_39_a3_temp<-c(trains_39_a3_temp,data.train[j-4,]$a3)
    trains_39_a4_temp<-c(trains_39_a4_temp,data.train[j-4,]$a4)
    trains_39_a5_temp<-c(trains_39_a5_temp,data.train[j-4,]$a5)
    trains_39_b1_temp<-c(trains_39_b1_temp,data.train[j-4,]$b1)
    trains_39_b2_temp<-c(trains_39_b2_temp,data.train[j-4,]$b2)
  }
  a1<-trains_39_a1_temp[-1]
  a2<-trains_39_a2_temp[-1]
  a3<-trains_39_a3_temp[-1]
  a4<-trains_39_a4_temp[-1]
  a5<-trains_39_a5_temp[-1]
  b1<-trains_39_b1_temp[-1]
  b2<-trains_39_b2_temp[-1]
  
  trains_39  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_40
  trains_40_a1_temp<-0
  trains_40_a2_temp<-0
  trains_40_a3_temp<-0
  trains_40_a4_temp<-0
  trains_40_a5_temp<-0
  trains_40_b1_temp<-0
  trains_40_b2_temp<-0
  for (j in row) {
    trains_40_a1_temp<-c(trains_40_a1_temp,data.train[j-3,]$a1)
    trains_40_a2_temp<-c(trains_40_a2_temp,data.train[j-3,]$a2)
    trains_40_a3_temp<-c(trains_40_a3_temp,data.train[j-3,]$a3)
    trains_40_a4_temp<-c(trains_40_a4_temp,data.train[j-3,]$a4)
    trains_40_a5_temp<-c(trains_40_a5_temp,data.train[j-3,]$a5)
    trains_40_b1_temp<-c(trains_40_b1_temp,data.train[j-3,]$b1)
    trains_40_b2_temp<-c(trains_40_b2_temp,data.train[j-3,]$b2)
  }
  a1<-trains_40_a1_temp[-1]
  a2<-trains_40_a2_temp[-1]
  a3<-trains_40_a3_temp[-1]
  a4<-trains_40_a4_temp[-1]
  a5<-trains_40_a5_temp[-1]
  b1<-trains_40_b1_temp[-1]
  b2<-trains_40_b2_temp[-1]
  
  trains_40  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_41
  trains_41_a1_temp<-0
  trains_41_a2_temp<-0
  trains_41_a3_temp<-0
  trains_41_a4_temp<-0
  trains_41_a5_temp<-0
  trains_41_b1_temp<-0
  trains_41_b2_temp<-0
  for (j in row) {
    trains_41_a1_temp<-c(trains_41_a1_temp,data.train[j-2,]$a1)
    trains_41_a2_temp<-c(trains_41_a2_temp,data.train[j-2,]$a2)
    trains_41_a3_temp<-c(trains_41_a3_temp,data.train[j-2,]$a3)
    trains_41_a4_temp<-c(trains_41_a4_temp,data.train[j-2,]$a4)
    trains_41_a5_temp<-c(trains_41_a5_temp,data.train[j-2,]$a5)
    trains_41_b1_temp<-c(trains_41_b1_temp,data.train[j-2,]$b1)
    trains_41_b2_temp<-c(trains_41_b2_temp,data.train[j-2,]$b2)
  }
  a1<-trains_41_a1_temp[-1]
  a2<-trains_41_a2_temp[-1]
  a3<-trains_41_a3_temp[-1]
  a4<-trains_41_a4_temp[-1]
  a5<-trains_41_a5_temp[-1]
  b1<-trains_41_b1_temp[-1]
  b2<-trains_41_b2_temp[-1]
  
  trains_41  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Trains_42
  trains_42_a1_temp<-0
  trains_42_a2_temp<-0
  trains_42_a3_temp<-0
  trains_42_a4_temp<-0
  trains_42_a5_temp<-0
  trains_42_b1_temp<-0
  trains_42_b2_temp<-0
  for (j in row) {
    trains_42_a1_temp<-c(trains_42_a1_temp,data.train[j-1,]$a1)
    trains_42_a2_temp<-c(trains_42_a2_temp,data.train[j-1,]$a2)
    trains_42_a3_temp<-c(trains_42_a3_temp,data.train[j-1,]$a3)
    trains_42_a4_temp<-c(trains_42_a4_temp,data.train[j-1,]$a4)
    trains_42_a5_temp<-c(trains_42_a5_temp,data.train[j-1,]$a5)
    trains_42_b1_temp<-c(trains_42_b1_temp,data.train[j-1,]$b1)
    trains_42_b2_temp<-c(trains_42_b2_temp,data.train[j-1,]$b2)
  }
  a1<-trains_42_a1_temp[-1]
  a2<-trains_42_a2_temp[-1]
  a3<-trains_42_a3_temp[-1]
  a4<-trains_42_a4_temp[-1]
  a5<-trains_42_a5_temp[-1]
  b1<-trains_42_b1_temp[-1]
  b2<-trains_42_b2_temp[-1]
  
  trains_42  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  #Results
  results_a1_temp<-0
  results_a2_temp<-0
  results_a3_temp<-0
  results_a4_temp<-0
  results_a5_temp<-0
  results_b1_temp<-0
  results_b2_temp<-0
  for (j in row) {
    results_a1_temp<-c(results_a1_temp,data.train[j,]$a1)
    results_a2_temp<-c(results_a2_temp,data.train[j,]$a2)
    results_a3_temp<-c(results_a3_temp,data.train[j,]$a3)
    results_a4_temp<-c(results_a4_temp,data.train[j,]$a4)
    results_a5_temp<-c(results_a5_temp,data.train[j,]$a5)
    results_b1_temp<-c(results_b1_temp,data.train[j,]$b1)
    results_b2_temp<-c(results_b2_temp,data.train[j,]$b2)
  }
  a1<-results_a1_temp[-1]
  a2<-results_a2_temp[-1]
  a3<-results_a3_temp[-1]
  a4<-results_a4_temp[-1]
  a5<-results_a5_temp[-1]
  b1<-results_b1_temp[-1]
  b2<-results_b2_temp[-1]
  
  results  <-data.frame(a1,a2,a3,a4,a5,b1,b2)
  
  tests_1  <-tail(data,count)[1:(count-41),]
  tests_2  <-tail(data,count)[2:(count-40),]
  tests_3  <-tail(data,count)[3:(count-39),]
  tests_4  <-tail(data,count)[4:(count-38),]
  tests_5  <-tail(data,count)[5:(count-37),]
  tests_6  <-tail(data,count)[6:(count-36),]
  tests_7  <-tail(data,count)[7:(count-35),]
  tests_8  <-tail(data,count)[8:(count-34),]
  tests_9  <-tail(data,count)[9:(count-33),]
  tests_10<-tail(data,count)[10:(count-32),]
  tests_11<-tail(data,count)[11:(count-31),]
  tests_12<-tail(data,count)[12:(count-30),]
  tests_13<-tail(data,count)[13:(count-29),]
  tests_14<-tail(data,count)[14:(count-28),]
  tests_15<-tail(data,count)[15:(count-27),]
  tests_16<-tail(data,count)[16:(count-26),]
  tests_17<-tail(data,count)[17:(count-25),]
  tests_18<-tail(data,count)[18:(count-24),]
  tests_19<-tail(data,count)[19:(count-23),]
  tests_20<-tail(data,count)[20:(count-22),]
  tests_21<-tail(data,count)[21:(count-21),]
  tests_22<-tail(data,count)[22:(count-20),]
  tests_23<-tail(data,count)[23:(count-19),]
  tests_24<-tail(data,count)[24:(count-18),]
  tests_25<-tail(data,count)[25:(count-17),]
  tests_26<-tail(data,count)[26:(count-16),]
  tests_27<-tail(data,count)[27:(count-15),]
  tests_28<-tail(data,count)[28:(count-14),]
  tests_29<-tail(data,count)[29:(count-13),]
  tests_30<-tail(data,count)[30:(count-12),]
  tests_31<-tail(data,count)[31:(count-11),]
  tests_32<-tail(data,count)[32:(count-10),]
  tests_33 <-tail(data,count)[33:(count-9),]
  tests_34 <-tail(data,count)[34:(count-8),]
  tests_35 <-tail(data,count)[35:(count-7),]
  tests_36 <-tail(data,count)[36:(count-6),]
  tests_37 <-tail(data,count)[37:(count-5),]
  tests_38 <-tail(data,count)[38:(count-4),]
  tests_39 <-tail(data,count)[39:(count-3),]
  tests_40 <-tail(data,count)[40:(count-2),]
  tests_41 <-tail(data,count)[41:(count-1),]
  tests_42 <-tail(data,count-41)
  
  #A:
  a1.1<-trains_1$a1
  a2.1<-trains_1$a2
  a3.1<-trains_1$a3
  a4.1<-trains_1$a4
  a5.1<-trains_1$a5
  
  a1.2<-trains_2$a1
  a2.2<-trains_2$a2
  a3.2<-trains_2$a3
  a4.2<-trains_2$a4
  a5.2<-trains_2$a5
  
  a1.3<-trains_3$a1
  a2.3<-trains_3$a2
  a3.3<-trains_3$a3
  a4.3<-trains_3$a4
  a5.3<-trains_3$a5
  
  a1.4<-trains_4$a1
  a2.4<-trains_4$a2
  a3.4<-trains_4$a3
  a4.4<-trains_4$a4
  a5.4<-trains_4$a5
  
  a1.5<-trains_5$a1
  a2.5<-trains_5$a2
  a3.5<-trains_5$a3
  a4.5<-trains_5$a4
  a5.5<-trains_5$a5
  
  a1.6<-trains_6$a1
  a2.6<-trains_6$a2
  a3.6<-trains_6$a3
  a4.6<-trains_6$a4
  a5.6<-trains_6$a5
  
  a1.7<-trains_7$a1
  a2.7<-trains_7$a2
  a3.7<-trains_7$a3
  a4.7<-trains_7$a4
  a5.7<-trains_7$a5
  
  a1.8<-trains_8$a1
  a2.8<-trains_8$a2
  a3.8<-trains_8$a3
  a4.8<-trains_8$a4
  a5.8<-trains_8$a5
  
  a1.9<-trains_9$a1
  a2.9<-trains_9$a2
  a3.9<-trains_9$a3
  a4.9<-trains_9$a4
  a5.9<-trains_9$a5
  
  a1.10<-trains_10$a1
  a2.10<-trains_10$a2
  a3.10<-trains_10$a3
  a4.10<-trains_10$a4
  a5.10<-trains_10$a5
  
  a1.11<-trains_11$a1
  a2.11<-trains_11$a2
  a3.11<-trains_11$a3
  a4.11<-trains_11$a4
  a5.11<-trains_11$a5
  
  a1.12<-trains_12$a1
  a2.12<-trains_12$a2
  a3.12<-trains_12$a3
  a4.12<-trains_12$a4
  a5.12<-trains_12$a5
  
  a1.13<-trains_13$a1
  a2.13<-trains_13$a2
  a3.13<-trains_13$a3
  a4.13<-trains_13$a4
  a5.13<-trains_13$a5
  
  a1.14<-trains_14$a1
  a2.14<-trains_14$a2
  a3.14<-trains_14$a3
  a4.14<-trains_14$a4
  a5.14<-trains_14$a5
  
  a1.15<-trains_15$a1
  a2.15<-trains_15$a2
  a3.15<-trains_15$a3
  a4.15<-trains_15$a4
  a5.15<-trains_15$a5
  
  a1.16<-trains_16$a1
  a2.16<-trains_16$a2
  a3.16<-trains_16$a3
  a4.16<-trains_16$a4
  a5.16<-trains_16$a5
  
  a1.17<-trains_17$a1
  a2.17<-trains_17$a2
  a3.17<-trains_17$a3
  a4.17<-trains_17$a4
  a5.17<-trains_17$a5
  
  a1.18<-trains_18$a1
  a2.18<-trains_18$a2
  a3.18<-trains_18$a3
  a4.18<-trains_18$a4
  a5.18<-trains_18$a5
  
  a1.19<-trains_19$a1
  a2.19<-trains_19$a2
  a3.19<-trains_19$a3
  a4.19<-trains_19$a4
  a5.19<-trains_19$a5
  
  a1.20<-trains_20$a1
  a2.20<-trains_20$a2
  a3.20<-trains_20$a3
  a4.20<-trains_20$a4
  a5.20<-trains_20$a5
  
  a1.21<-trains_21$a1
  a2.21<-trains_21$a2
  a3.21<-trains_21$a3
  a4.21<-trains_21$a4
  a5.21<-trains_21$a5
  
  a1.22<-trains_22$a1
  a2.22<-trains_22$a2
  a3.22<-trains_22$a3
  a4.22<-trains_22$a4
  a5.22<-trains_22$a5
  
  a1.23<-trains_23$a1
  a2.23<-trains_23$a2
  a3.23<-trains_23$a3
  a4.23<-trains_23$a4
  a5.23<-trains_23$a5
  
  a1.24<-trains_24$a1
  a2.24<-trains_24$a2
  a3.24<-trains_24$a3
  a4.24<-trains_24$a4
  a5.24<-trains_24$a5
  
  a1.25<-trains_25$a1
  a2.25<-trains_25$a2
  a3.25<-trains_25$a3
  a4.25<-trains_25$a4
  a5.25<-trains_25$a5
  
  a1.26<-trains_26$a1
  a2.26<-trains_26$a2
  a3.26<-trains_26$a3
  a4.26<-trains_26$a4
  a5.26<-trains_26$a5
  
  a1.27<-trains_27$a1
  a2.27<-trains_27$a2
  a3.27<-trains_27$a3
  a4.27<-trains_27$a4
  a5.27<-trains_27$a5
  
  a1.28<-trains_28$a1
  a2.28<-trains_28$a2
  a3.28<-trains_28$a3
  a4.28<-trains_28$a4
  a5.28<-trains_28$a5
  
  a1.29<-trains_29$a1
  a2.29<-trains_29$a2
  a3.29<-trains_29$a3
  a4.29<-trains_29$a4
  a5.29<-trains_29$a5
  
  a1.30<-trains_30$a1
  a2.30<-trains_30$a2
  a3.30<-trains_30$a3
  a4.30<-trains_30$a4
  a5.30<-trains_30$a5
  
  a1.31<-trains_31$a1
  a2.31<-trains_31$a2
  a3.31<-trains_31$a3
  a4.31<-trains_31$a4
  a5.31<-trains_31$a5
  
  a1.32<-trains_32$a1
  a2.32<-trains_32$a2
  a3.32<-trains_32$a3
  a4.32<-trains_32$a4
  a5.32<-trains_32$a5
  
  a1.33<-trains_33$a1
  a2.33<-trains_33$a2
  a3.33<-trains_33$a3
  a4.33<-trains_33$a4
  a5.33<-trains_33$a5
  
  a1.34<-trains_34$a1
  a2.34<-trains_34$a2
  a3.34<-trains_34$a3
  a4.34<-trains_34$a4
  a5.34<-trains_34$a5
  
  a1.35<-trains_35$a1
  a2.35<-trains_35$a2
  a3.35<-trains_35$a3
  a4.35<-trains_35$a4
  a5.35<-trains_35$a5
  
  a1.36<-trains_36$a1
  a2.36<-trains_36$a2
  a3.36<-trains_36$a3
  a4.36<-trains_36$a4
  a5.36<-trains_36$a5
  
  a1.37<-trains_37$a1
  a2.37<-trains_37$a2
  a3.37<-trains_37$a3
  a4.37<-trains_37$a4
  a5.37<-trains_37$a5
  
  a1.38<-trains_38$a1
  a2.38<-trains_38$a2
  a3.38<-trains_38$a3
  a4.38<-trains_38$a4
  a5.38<-trains_38$a5
  
  a1.39<-trains_39$a1
  a2.39<-trains_39$a2
  a3.39<-trains_39$a3
  a4.39<-trains_39$a4
  a5.39<-trains_39$a5
  
  a1.40<-trains_40$a1
  a2.40<-trains_40$a2
  a3.40<-trains_40$a3
  a4.40<-trains_40$a4
  a5.40<-trains_40$a5
  
  a1.41<-trains_41$a1
  a2.41<-trains_41$a2
  a3.41<-trains_41$a3
  a4.41<-trains_41$a4
  a5.41<-trains_41$a5
  
  a1.42<-trains_42$a1
  a2.42<-trains_42$a2
  a3.42<-trains_42$a3
  a4.42<-trains_42$a4
  a5.42<-trains_42$a5
  
  
  resa1<-results$a1
  resa2<-results$a2
  resa3<-results$a3
  resa4<-results$a4
  resa5<-results$a5
  #B:
  b1.1<-trains_1$b1
  b2.1<-trains_1$b2
  b1.2<-trains_2$b1
  b2.2<-trains_2$b2
  b1.3<-trains_3$b1
  b2.3<-trains_3$b2
  b1.4<-trains_4$b1
  b2.4<-trains_4$b2
  b1.5<-trains_5$b1
  b2.5<-trains_5$b2
  b1.6<-trains_6$b1
  b2.6<-trains_6$b2
  b1.7<-trains_7$b1
  b2.7<-trains_7$b2
  b1.8<-trains_8$b1
  b2.8<-trains_8$b2
  b1.9<-trains_9$b1
  b2.9<-trains_9$b2
  
  b1.10<-trains_10$b1
  b2.10<-trains_10$b2
  b1.11<-trains_11$b1
  b2.11<-trains_11$b2
  b1.12<-trains_12$b1
  b2.12<-trains_12$b2
  b1.13<-trains_13$b1
  b2.13<-trains_13$b2
  b1.14<-trains_14$b1
  b2.14<-trains_14$b2
  b1.15<-trains_15$b1
  b2.15<-trains_15$b2
  b1.16<-trains_16$b1
  b2.16<-trains_16$b2
  b1.17<-trains_17$b1
  b2.17<-trains_17$b2
  b1.18<-trains_18$b1
  b2.18<-trains_18$b2
  b1.19<-trains_19$b1
  b2.19<-trains_19$b2
  
  b1.20<-trains_20$b1
  b2.20<-trains_20$b2
  b1.21<-trains_21$b1
  b2.21<-trains_21$b2
  b1.22<-trains_22$b1
  b2.22<-trains_22$b2
  b1.23<-trains_23$b1
  b2.23<-trains_23$b2
  b1.24<-trains_24$b1
  b2.24<-trains_24$b2
  b1.25<-trains_25$b1
  b2.25<-trains_25$b2
  b1.26<-trains_26$b1
  b2.26<-trains_26$b2
  b1.27<-trains_27$b1
  b2.27<-trains_27$b2
  b1.28<-trains_28$b1
  b2.28<-trains_28$b2
  b1.29<-trains_29$b1
  b2.29<-trains_29$b2
  
  b1.30<-trains_30$b1
  b2.30<-trains_30$b2
  b1.31<-trains_31$b1
  b2.31<-trains_31$b2
  b1.32<-trains_32$b1
  b2.32<-trains_32$b2
  b1.33<-trains_33$b1
  b2.33<-trains_33$b2
  b1.34<-trains_34$b1
  b2.34<-trains_34$b2
  b1.35<-trains_35$b1
  b2.35<-trains_35$b2
  b1.36<-trains_36$b1
  b2.36<-trains_36$b2
  b1.37<-trains_37$b1
  b2.37<-trains_37$b2
  b1.38<-trains_38$b1
  b2.38<-trains_38$b2
  b1.39<-trains_39$b1
  b2.39<-trains_39$b2
  
  b1.40<-trains_40$b1
  b2.40<-trains_40$b2
  b1.41<-trains_41$b1
  b2.41<-trains_41$b2
  b1.42<-trains_42$b1
  b2.42<-trains_42$b2
  
  
  resb1<-results$b1
  resb2<-results$b2
  
  
  trains.a1<-data.frame(a1.1,a1.2,a1.3,
                        a1.4,a1.5,a1.6,
                        a1.7,a1.8,a1.9,
                        a1.10,a1.11,a1.12,
                        a1.13,a1.14,a1.15,
                        a1.16,a1.17,a1.18,
                        a1.19,a1.20,a1.21,
                        a1.22,a1.23,a1.24,
                        a1.25,a1.26,a1.27,
                        a1.28,a1.29,a1.30,
                        a1.31,a1.32,a1.33,
                        a1.34,a1.35,a1.36,
                        a1.37,a1.38,a1.39,
                        a1.40,a1.41,a1.42,
                        resa1)
  trains.a2<-data.frame(a2.1,a2.2,a2.3,
                        a2.4,a2.5,a2.6,
                        a2.7,a2.8,a2.9,
                        a2.10,a2.11,a2.12,
                        a2.13,a2.14,a2.15,
                        a2.16,a2.17,a2.18,
                        a2.19,a2.20,a2.21,
                        a2.22,a2.23,a2.24,
                        a2.25,a2.26,a2.27,
                        a2.28,a2.29,a2.30,
                        a2.31,a2.32,a2.33,
                        a2.34,a2.35,a2.36,
                        a2.37,a2.38,a2.39,
                        a2.40,a2.41,a2.42,
                        resa2)
  trains.a3<-data.frame(a3.1,a3.2,a3.3,
                        a3.4,a3.5,a3.6,
                        a3.7,a3.8,a3.9,
                        a3.10,a3.11,a3.12,
                        a3.13,a3.14,a3.15,
                        a3.16,a3.17,a3.18,
                        a3.19,a3.20,a3.21,
                        a3.22,a3.23,a3.24,
                        a3.25,a3.26,a3.27,
                        a3.28,a3.29,a3.30,
                        a3.31,a3.32,a3.33,
                        a3.34,a3.35,a3.36,
                        a3.37,a3.38,a3.39,
                        a3.40,a3.41,a3.42,
                        resa3)
  trains.a4<-data.frame(a4.1,a4.2,a4.3,
                        a4.4,a4.5,a4.6,
                        a4.7,a4.8,a4.9,
                        a4.10,a4.11,a4.12,
                        a4.13,a4.14,a4.15,
                        a4.16,a4.17,a4.18,
                        a4.19,a4.20,a4.21,
                        a4.22,a4.23,a4.24,
                        a4.25,a4.26,a4.27,
                        a4.28,a4.29,a4.30,
                        a4.31,a4.32,a4.33,
                        a4.34,a4.35,a4.36,
                        a4.37,a4.38,a4.39,
                        a4.40,a4.41,a4.42,
                        resa4)
  trains.a5<-data.frame(a5.1,a5.2,a5.3,
                        a5.4,a5.5,a5.6,
                        a5.7,a5.8,a5.9,
                        a5.10,a5.11,a5.12,
                        a5.13,a5.14,a5.15,
                        a5.16,a5.17,a5.18,
                        a5.19,a5.20,a5.21,
                        a5.22,a5.23,a5.24,
                        a5.25,a5.26,a5.27,
                        a5.28,a5.29,a5.30,
                        a5.31,a5.32,a5.33,
                        a5.34,a5.35,a5.36,
                        a5.37,a5.38,a5.39,
                        a5.40,a5.41,a5.42,
                        resa5)
  trains.b1<-data.frame(b1.1,b1.2,b1.3,
                        b1.4,b1.5,b1.6,
                        b1.7,b1.8,b1.9,
                        b1.10,b1.11,b1.12,
                        b1.13,b1.14,b1.15,
                        b1.16,b1.17,b1.18,
                        b1.19,b1.20,b1.21,
                        b1.22,b1.23,b1.24,
                        b1.25,b1.26,b1.27,
                        b1.28,b1.29,b1.30,
                        b1.31,b1.32,b1.33,
                        b1.34,b1.35,b1.36,
                        b1.37,b1.38,b1.39,
                        b1.40,b1.41,b1.42,
                        resb1)
  trains.b2<-data.frame(b2.1,b2.2,b2.3,
                        b2.4,b2.5,b2.6,
                        b2.7,b2.8,b2.9,
                        b2.10,b2.11,b2.12,
                        b2.13,b2.14,b2.15,
                        b2.16,b2.17,b2.18,
                        b2.19,b2.20,b2.21,
                        b2.22,b2.23,b2.24,
                        b2.25,b2.26,b2.27,
                        b2.28,b2.29,b2.30,
                        b2.31,b2.32,b2.33,
                        b2.34,b2.35,b2.36,
                        b2.37,b2.38,b2.39,
                        b2.40,b2.41,b2.42,
                        resb2)
  mdaModel.a1<-mda(resa1~
                     #a1.1+a1.2+a1.3+
                     #a1.4+a1.5+a1.6+
                     #a1.7+a1.8+a1.9+
                     #a1.10+a1.11+a1.12+
                     #a1.13+a1.14+a1.15+
                     #a1.16+a1.17+a1.18+
                     #a1.19+a1.20+a1.21+
                     #a1.22+a1.23+a1.24+
                     #a1.25+a1.26+a1.27+
                     #a1.28+a1.29+a1.30+
                     #a1.31+a1.32+a1.33+
                     #a1.34+a1.35+a1.36+
                     a1.37+a1.38+a1.39+
                     a1.40+a1.41+a1.42,
                   data = trains.a1,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 8
  )
  mdaModel.a2<-mda(resa2~
                     #a2.1+a2.2+a2.3+
                     #a2.4+a2.5+a2.6+
                     #a2.7+a2.8+a2.9+
                     #a2.10+a2.11+a2.12+
                     #a2.13+a2.14+a2.15+
                     #a2.16+a2.17+a2.18+
                     #a2.19+a2.20+a2.21+
                     #a2.22+a2.23+a2.24+
                     #a2.25+a2.26+a2.27+
                     #a2.28+a2.29+a2.30+
                     #a2.31+a2.32+a2.33+
                     #a2.34+a2.35+a2.36+
                     a2.37+a2.38+a2.39+
                     a2.40+a2.41+a2.42,
                   data = trains.a2,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 6
  )
  mdaModel.a3<-mda(resa3~
                     #a3.1+a3.2+a3.3+
                     #a3.4+a3.5+a3.6+
                     #a3.7+a3.8+a3.9+
                     #a3.10+a3.11+a3.12+
                     #a3.13+a3.14+a3.15+
                     #a3.16+a3.17+a3.18+
                     #a3.19+a3.20+a3.21+
                     #a3.22+a3.23+a3.24+
                     #a3.25+a3.26+a3.27+
                     #a3.28+a3.29+a3.30+
                     #a3.31+a3.32+a3.33+
                     #a3.34+a3.35+a3.36+
                     a3.37+a3.38+a3.39+
                     a3.40+a3.41+a3.42,
                   data = trains.a3,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 5
  )
  mdaModel.a4<-mda(resa4~
                     #a4.1+a4.2+a4.3+
                     #a4.4+a4.5+a4.6+
                     #a4.7+a4.8+a4.9+
                     #a4.10+a4.11+a4.12+
                     #a4.13+a4.14+a4.15+
                     #a4.16+a4.17+a4.18+
                     #a4.19+a4.20+a4.21+
                     #a4.22+a4.23+a4.24+
                     #a4.25+a4.26+a4.27+
                     #a4.28+a4.29+a4.30+
                     #a4.31+a4.32+a4.33+
                     #a4.34+a4.35+a4.36+
                     a4.37+a4.38+a4.39+
                     a4.40+a4.41+a4.42,
                   data = trains.a4,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 6
  )
  mdaModel.a5<-mda(resa5~
                     #a5.1+a5.2+a5.3+
                     #a5.4+a5.5+a5.6+
                     #a5.7+a5.8+a5.9+
                     #a5.10+a5.11+a5.12+
                     #a5.13+a5.14+a5.15+
                     #a5.16+a5.17+a5.18+
                     #a5.19+a5.20+a5.21+
                     #a5.22+a5.23+a5.24+
                     #a5.25+a5.26+a5.27+
                     #a5.28+a5.29+a5.30+
                     #a5.31+a5.32+a5.33+
                     #a5.34+a5.35+a5.36+
                     a5.37+a5.38+a5.39+
                     a5.40+a5.41+a5.42,
                   data = trains.a5,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 8
  )
  mdaModel.b1<-mda(resb1~
                     #b1.1+b1.2+b1.3+
                     #b1.4+b1.5+b1.6+
                     #b1.7+b1.8+b1.9+
                     #b1.10+b1.11+b1.12+
                     #b1.13+b1.14+b1.15+
                     #b1.16+b1.17+b1.18+
                     #b1.19+b1.20+b1.21+
                     #b1.22+b1.23+b1.24+
                     #b1.25+b1.26+b1.27+
                     #b1.28+b1.29+b1.30+
                     #b1.31+b1.32+b1.33+
                     #b1.34+b1.35+b1.36+
                     b1.37+b1.38+b1.39+
                     b1.40+b1.41+b1.42,
                   data = trains.b1,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 16
  )
  mdaModel.b2<-mda(resb2~
                     #b2.1+b2.2+b2.3+
                     #b2.4+b2.5+b2.6+
                     #b2.7+b2.8+b2.9+
                     #b2.10+b2.11+b2.12+
                     #b2.13+b2.14+b2.15+
                     #b2.16+b2.17+b2.18+
                     #b2.19+b2.20+b2.21+
                     #b2.22+b2.23+b2.24+
                     #b2.25+b2.26+b2.27+
                     #b2.28+b2.29+b2.30+
                     #b2.31+b2.32+b2.33+
                     #b2.34+b2.35+b2.36+
                     b2.37+b2.38+b2.39+
                     b2.40+b2.41+b2.42,
                   data = trains.b2,
                   preProcess = c("BoxCox","center","scale","pca"),
                   subclasses = 9
  )
  
  ################################################################################
  tests.ab<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                       a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                       a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                       a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                       a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                       a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                       a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                       a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                       a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                       
                       a1.10,a2.10,a3.10,a4.10,a5.10,b1.10,b2.10,
                       a1.11,a2.11,a3.11,a4.11,a5.11,b1.11,b2.11,
                       a1.12,a2.12,a3.12,a4.12,a5.12,b1.12,b2.12,
                       a1.13,a2.13,a3.13,a4.13,a5.13,b1.13,b2.13,
                       a1.14,a2.14,a3.14,a4.14,a5.14,b1.14,b2.14,
                       a1.15,a2.15,a3.15,a4.15,a5.15,b1.15,b2.15,
                       a1.16,a2.16,a3.16,a4.16,a5.16,b1.16,b2.16,
                       a1.17,a2.17,a3.17,a4.17,a5.17,b1.17,b2.17,
                       a1.18,a2.18,a3.18,a4.18,a5.18,b1.18,b2.18,
                       a1.19,a2.19,a3.19,a4.19,a5.19,b1.19,b2.19,
                       
                       a1.20,a2.20,a3.20,a4.20,a5.20,b1.20,b2.20,
                       a1.21,a2.21,a3.21,a4.21,a5.21,b1.21,b2.21,
                       a1.22,a2.22,a3.22,a4.22,a5.22,b1.22,b2.22,
                       a1.23,a2.23,a3.23,a4.23,a5.23,b1.23,b2.23,
                       a1.24,a2.24,a3.24,a4.24,a5.24,b1.24,b2.24,
                       a1.25,a2.25,a3.25,a4.25,a5.25,b1.25,b2.25,
                       a1.26,a2.26,a3.26,a4.26,a5.26,b1.26,b2.26,
                       a1.27,a2.27,a3.27,a4.27,a5.27,b1.27,b2.27,
                       a1.28,a2.28,a3.28,a4.28,a5.28,b1.28,b2.28,
                       a1.29,a2.29,a3.29,a4.29,a5.29,b1.29,b2.29,
                       
                       a1.30,a2.30,a3.30,a4.30,a5.30, b1.30,b2.30,
                       a1.31,a2.31,a3.31,a4.31,a5.31,b1.31,b2.31,
                       a1.32,a2.32,a3.32,a4.32,a5.32,b1.32,b2.32,
                       a1.33,a2.33,a3.33,a4.33,a5.33,b1.33,b2.33,
                       a1.34,a2.34,a3.34,a4.34,a5.34,b1.34,b2.34,
                       a1.35,a2.35,a3.35,a4.35,a5.35,b1.35,b2.35,
                       a1.36,a2.36,a3.36,a4.36,a5.36,b1.36,b2.36,
                       a1.37,a2.37,a3.37,a4.37,a5.37,b1.37,b2.37,
                       a1.38,a2.38,a3.38,a4.38,a5.38,b1.38,b2.38,
                       a1.39,a2.39,a3.39,a4.39,a5.39,b1.39,b2.39,
                       
                       a1.40,a2.40,a3.40,a4.40,a5.40,b1.40,b2.40,
                       a1.41,a2.41,a3.41,a4.41,a5.41,b1.41,b2.41,
                       a1.42,a2.42,a3.42,a4.42,a5.42,b1.42,b2.42)
  test.ab<-preProcess(tests.ab,method = c("BoxCox","center","scale","pca"))
  testPredictions.a1<-as.numeric(predict(mdaModel.a1,tests.ab))
  testPredictions.a2<-as.numeric(predict(mdaModel.a2,tests.ab))
  testPredictions.a3<-as.numeric(predict(mdaModel.a3,tests.ab))
  testPredictions.a4<-as.numeric(predict(mdaModel.a4,tests.ab))
  testPredictions.a5<-as.numeric(predict(mdaModel.a5,tests.ab))
  testPredictions.b1<-as.numeric(predict(mdaModel.b1,tests.ab))
  testPredictions.b2<-as.numeric(predict(mdaModel.b2,tests.ab))
  
  
  #A1:
  a1.delta<-results$a1-testPredictions.a1
  barplot(table(a1.delta),main = "a1")
  #A2:
  
  a2.delta<-results$a2-testPredictions.a2
  barplot(table(a2.delta),main = "a2") 
  #A3:
  a3.delta<-results$a3-testPredictions.a3
  barplot(table(a3.delta),main = "a3") 
  #A4:
  a4.delta<-results$a4-testPredictions.a4
  barplot(table(a4.delta),main = "a4") 
  #A5:
  a5.delta<-results$a5-testPredictions.a5
  barplot(table(a5.delta),main = "a5") 
  #B1:
  b1.delta<-results$b1-testPredictions.b1
  barplot(table(b1.delta),main = "b1")
  #B2:
  b2.delta<-results$b2-testPredictions.b2
  barplot(table(b2.delta),main = "b2")
  
  a1.d<-tail(as.numeric(names(sort(table(a1.delta)))),1)
  a2.d<-tail(as.numeric(names(sort(table(a2.delta)))),1)
  a3.d<-tail(as.numeric(names(sort(table(a3.delta)))),1)
  a4.d<-tail(as.numeric(names(sort(table(a4.delta)))),1)
  a5.d<-tail(as.numeric(names(sort(table(a5.delta)))),1)
  b1.d<-tail(as.numeric(names(sort(table(b1.delta)))),1)
  b2.d<-tail(as.numeric(names(sort(table(b2.delta)))),1)
  
  ###################################################################################
  
  #Buil test data
  #A:
  a1.1<-tests_1$a1
  a2.1<-tests_1$a2
  a3.1<-tests_1$a3
  a4.1<-tests_1$a4
  a5.1<-tests_1$a5
  
  a1.2<-tests_2$a1
  a2.2<-tests_2$a2
  a3.2<-tests_2$a3
  a4.2<-tests_2$a4
  a5.2<-tests_2$a5
  
  a1.3<-tests_3$a1
  a2.3<-tests_3$a2
  a3.3<-tests_3$a3
  a4.3<-tests_3$a4
  a5.3<-tests_3$a5
  
  a1.4<-tests_4$a1
  a2.4<-tests_4$a2
  a3.4<-tests_4$a3
  a4.4<-tests_4$a4
  a5.4<-tests_4$a5
  
  a1.5<-tests_5$a1
  a2.5<-tests_5$a2
  a3.5<-tests_5$a3
  a4.5<-tests_5$a4
  a5.5<-tests_5$a5
  
  a1.6<-tests_6$a1
  a2.6<-tests_6$a2
  a3.6<-tests_6$a3
  a4.6<-tests_6$a4
  a5.6<-tests_6$a5
  
  a1.7<-tests_7$a1
  a2.7<-tests_7$a2
  a3.7<-tests_7$a3
  a4.7<-tests_7$a4
  a5.7<-tests_7$a5
  
  a1.8<-tests_8$a1
  a2.8<-tests_8$a2
  a3.8<-tests_8$a3
  a4.8<-tests_8$a4
  a5.8<-tests_8$a5
  
  a1.9<-tests_9$a1
  a2.9<-tests_9$a2
  a3.9<-tests_9$a3
  a4.9<-tests_9$a4
  a5.9<-tests_9$a5
  
  a1.10<-tests_10$a1
  a2.10<-tests_10$a2
  a3.10<-tests_10$a3
  a4.10<-tests_10$a4
  a5.10<-tests_10$a5
  
  a1.11<-tests_11$a1
  a2.11<-tests_11$a2
  a3.11<-tests_11$a3
  a4.11<-tests_11$a4
  a5.11<-tests_11$a5
  
  a1.12<-tests_12$a1
  a2.12<-tests_12$a2
  a3.12<-tests_12$a3
  a4.12<-tests_12$a4
  a5.12<-tests_12$a5
  
  a1.13<-tests_13$a1
  a2.13<-tests_13$a2
  a3.13<-tests_13$a3
  a4.13<-tests_13$a4
  a5.13<-tests_13$a5
  
  a1.14<-tests_14$a1
  a2.14<-tests_14$a2
  a3.14<-tests_14$a3
  a4.14<-tests_14$a4
  a5.14<-tests_14$a5
  
  a1.15<-tests_15$a1
  a2.15<-tests_15$a2
  a3.15<-tests_15$a3
  a4.15<-tests_15$a4
  a5.15<-tests_15$a5
  
  a1.16<-tests_16$a1
  a2.16<-tests_16$a2
  a3.16<-tests_16$a3
  a4.16<-tests_16$a4
  a5.16<-tests_16$a5
  
  a1.17<-tests_17$a1
  a2.17<-tests_17$a2
  a3.17<-tests_17$a3
  a4.17<-tests_17$a4
  a5.17<-tests_17$a5
  
  a1.18<-tests_18$a1
  a2.18<-tests_18$a2
  a3.18<-tests_18$a3
  a4.18<-tests_18$a4
  a5.18<-tests_18$a5
  
  a1.19<-tests_19$a1
  a2.19<-tests_19$a2
  a3.19<-tests_19$a3
  a4.19<-tests_19$a4
  a5.19<-tests_19$a5
  
  a1.20<-tests_20$a1
  a2.20<-tests_20$a2
  a3.20<-tests_20$a3
  a4.20<-tests_20$a4
  a5.20<-tests_20$a5
  
  a1.21<-tests_21$a1
  a2.21<-tests_21$a2
  a3.21<-tests_21$a3
  a4.21<-tests_21$a4
  a5.21<-tests_21$a5
  
  a1.22<-tests_22$a1
  a2.22<-tests_22$a2
  a3.22<-tests_22$a3
  a4.22<-tests_22$a4
  a5.22<-tests_22$a5
  
  a1.23<-tests_23$a1
  a2.23<-tests_23$a2
  a3.23<-tests_23$a3
  a4.23<-tests_23$a4
  a5.23<-tests_23$a5
  
  a1.24<-tests_24$a1
  a2.24<-tests_24$a2
  a3.24<-tests_24$a3
  a4.24<-tests_24$a4
  a5.24<-tests_24$a5
  
  a1.25<-tests_25$a1
  a2.25<-tests_25$a2
  a3.25<-tests_25$a3
  a4.25<-tests_25$a4
  a5.25<-tests_25$a5
  
  a1.26<-tests_26$a1
  a2.26<-tests_26$a2
  a3.26<-tests_26$a3
  a4.26<-tests_26$a4
  a5.26<-tests_26$a5
  
  a1.27<-tests_27$a1
  a2.27<-tests_27$a2
  a3.27<-tests_27$a3
  a4.27<-tests_27$a4
  a5.27<-tests_27$a5
  
  a1.28<-tests_28$a1
  a2.28<-tests_28$a2
  a3.28<-tests_28$a3
  a4.28<-tests_28$a4
  a5.28<-tests_28$a5
  
  a1.29<-tests_29$a1
  a2.29<-tests_29$a2
  a3.29<-tests_29$a3
  a4.29<-tests_29$a4
  a5.29<-tests_29$a5
  
  a1.30<-tests_30$a1
  a2.30<-tests_30$a2
  a3.30<-tests_30$a3
  a4.30<-tests_30$a4
  a5.30<-tests_30$a5
  
  a1.31<-tests_31$a1
  a2.31<-tests_31$a2
  a3.31<-tests_31$a3
  a4.31<-tests_31$a4
  a5.31<-tests_31$a5
  
  a1.32<-tests_32$a1
  a2.32<-tests_32$a2
  a3.32<-tests_32$a3
  a4.32<-tests_32$a4
  a5.32<-tests_32$a5
  
  a1.33<-tests_33$a1
  a2.33<-tests_33$a2
  a3.33<-tests_33$a3
  a4.33<-tests_33$a4
  a5.33<-tests_33$a5
  
  a1.34<-tests_34$a1
  a2.34<-tests_34$a2
  a3.34<-tests_34$a3
  a4.34<-tests_34$a4
  a5.34<-tests_34$a5
  
  a1.35<-tests_35$a1
  a2.35<-tests_35$a2
  a3.35<-tests_35$a3
  a4.35<-tests_35$a4
  a5.35<-tests_35$a5
  
  a1.36<-tests_36$a1
  a2.36<-tests_36$a2
  a3.36<-tests_36$a3
  a4.36<-tests_36$a4
  a5.36<-tests_36$a5
  
  a1.37<-tests_37$a1
  a2.37<-tests_37$a2
  a3.37<-tests_37$a3
  a4.37<-tests_37$a4
  a5.37<-tests_37$a5
  
  a1.38<-tests_38$a1
  a2.38<-tests_38$a2
  a3.38<-tests_38$a3
  a4.38<-tests_38$a4
  a5.38<-tests_38$a5
  
  a1.39<-tests_39$a1
  a2.39<-tests_39$a2
  a3.39<-tests_39$a3
  a4.39<-tests_39$a4
  a5.39<-tests_39$a5
  
  a1.40<-tests_40$a1
  a2.40<-tests_40$a2
  a3.40<-tests_40$a3
  a4.40<-tests_40$a4
  a5.40<-tests_40$a5
  
  a1.41<-tests_41$a1
  a2.41<-tests_41$a2
  a3.41<-tests_41$a3
  a4.41<-tests_41$a4
  a5.41<-tests_41$a5
  
  a1.42<-tests_42$a1
  a2.42<-tests_42$a2
  a3.42<-tests_42$a3
  a4.42<-tests_42$a4
  a5.42<-tests_42$a5
  
  #B:
  b1.1<-tests_1$b1
  b2.1<-tests_1$b2
  b1.2<-tests_2$b1
  b2.2<-tests_2$b2
  b1.3<-tests_3$b1
  b2.3<-tests_3$b2
  b1.4<-tests_4$b1
  b2.4<-tests_4$b2
  b1.5<-tests_5$b1
  b2.5<-tests_5$b2
  b1.6<-tests_6$b1
  b2.6<-tests_6$b2
  b1.7<-tests_7$b1
  b2.7<-tests_7$b2
  b1.8<-tests_8$b1
  b2.8<-tests_8$b2
  b1.9<-tests_9$b1
  b2.9<-tests_9$b2
  
  b1.10<-tests_10$b1
  b2.10<-tests_10$b2
  b1.11<-tests_11$b1
  b2.11<-tests_11$b2
  b1.12<-tests_12$b1
  b2.12<-tests_12$b2
  b1.13<-tests_13$b1
  b2.13<-tests_13$b2
  b1.14<-tests_14$b1
  b2.14<-tests_14$b2
  b1.15<-tests_15$b1
  b2.15<-tests_15$b2
  b1.16<-tests_16$b1
  b2.16<-tests_16$b2
  b1.17<-tests_17$b1
  b2.17<-tests_17$b2
  b1.18<-tests_18$b1
  b2.18<-tests_18$b2
  b1.19<-tests_19$b1
  b2.19<-tests_19$b2
  
  b1.20<-tests_20$b1
  b2.20<-tests_20$b2
  b1.21<-tests_21$b1
  b2.21<-tests_21$b2
  b1.22<-tests_22$b1
  b2.22<-tests_22$b2
  b1.23<-tests_23$b1
  b2.23<-tests_23$b2
  b1.24<-tests_24$b1
  b2.24<-tests_24$b2
  b1.25<-tests_25$b1
  b2.25<-tests_25$b2
  b1.26<-tests_26$b1
  b2.26<-tests_26$b2
  b1.27<-tests_27$b1
  b2.27<-tests_27$b2
  b1.28<-tests_28$b1
  b2.28<-tests_28$b2
  b1.29<-tests_29$b1
  b2.29<-tests_29$b2
  
  b1.30<-tests_30$b1
  b2.30<-tests_30$b2
  b1.31<-tests_31$b1
  b2.31<-tests_31$b2
  b1.32<-tests_32$b1
  b2.32<-tests_32$b2
  b1.33<-tests_33$b1
  b2.33<-tests_33$b2
  b1.34<-tests_34$b1
  b2.34<-tests_34$b2
  b1.35<-tests_35$b1
  b2.35<-tests_35$b2
  b1.36<-tests_36$b1
  b2.36<-tests_36$b2
  b1.37<-tests_37$b1
  b2.37<-tests_37$b2
  b1.38<-tests_38$b1
  b2.38<-tests_38$b2
  b1.39<-tests_39$b1
  b2.39<-tests_39$b2
  
  b1.40<-tests_40$b1
  b2.40<-tests_40$b2
  b1.41<-tests_41$b1
  b2.41<-tests_41$b2
  b1.42<-tests_42$b1
  b2.42<-tests_42$b2
  
  tests.ab<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                       a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                       a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                       a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                       a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                       a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                       a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                       a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                       a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                       
                       a1.10,a2.10,a3.10,a4.10,a5.10,b1.10,b2.10,
                       a1.11,a2.11,a3.11,a4.11,a5.11,b1.11,b2.11,
                       a1.12,a2.12,a3.12,a4.12,a5.12,b1.12,b2.12,
                       a1.13,a2.13,a3.13,a4.13,a5.13,b1.13,b2.13,
                       a1.14,a2.14,a3.14,a4.14,a5.14,b1.14,b2.14,
                       a1.15,a2.15,a3.15,a4.15,a5.15,b1.15,b2.15,
                       a1.16,a2.16,a3.16,a4.16,a5.16,b1.16,b2.16,
                       a1.17,a2.17,a3.17,a4.17,a5.17,b1.17,b2.17,
                       a1.18,a2.18,a3.18,a4.18,a5.18,b1.18,b2.18,
                       a1.19,a2.19,a3.19,a4.19,a5.19,b1.19,b2.19,
                       
                       a1.20,a2.20,a3.20,a4.20,a5.20,b1.20,b2.20,
                       a1.21,a2.21,a3.21,a4.21,a5.21,b1.21,b2.21,
                       a1.22,a2.22,a3.22,a4.22,a5.22,b1.22,b2.22,
                       a1.23,a2.23,a3.23,a4.23,a5.23,b1.23,b2.23,
                       a1.24,a2.24,a3.24,a4.24,a5.24,b1.24,b2.24,
                       a1.25,a2.25,a3.25,a4.25,a5.25,b1.25,b2.25,
                       a1.26,a2.26,a3.26,a4.26,a5.26,b1.26,b2.26,
                       a1.27,a2.27,a3.27,a4.27,a5.27,b1.27,b2.27,
                       a1.28,a2.28,a3.28,a4.28,a5.28,b1.28,b2.28,
                       a1.29,a2.29,a3.29,a4.29,a5.29,b1.29,b2.29,
                       
                       a1.30,a2.30,a3.30,a4.30,a5.30, b1.30,b2.30,
                       a1.31,a2.31,a3.31,a4.31,a5.31,b1.31,b2.31,
                       a1.32,a2.32,a3.32,a4.32,a5.32,b1.32,b2.32,
                       a1.33,a2.33,a3.33,a4.33,a5.33,b1.33,b2.33,
                       a1.34,a2.34,a3.34,a4.34,a5.34,b1.34,b2.34,
                       a1.35,a2.35,a3.35,a4.35,a5.35,b1.35,b2.35,
                       a1.36,a2.36,a3.36,a4.36,a5.36,b1.36,b2.36,
                       a1.37,a2.37,a3.37,a4.37,a5.37,b1.37,b2.37,
                       a1.38,a2.38,a3.38,a4.38,a5.38,b1.38,b2.38,
                       a1.39,a2.39,a3.39,a4.39,a5.39,b1.39,b2.39,
                       
                       a1.40,a2.40,a3.40,a4.40,a5.40,b1.40,b2.40,
                       a1.41,a2.41,a3.41,a4.41,a5.41,b1.41,b2.41,
                       a1.42,a2.42,a3.42,a4.42,a5.42,b1.42,b2.42)
  test.ab<-preProcess(tests.ab,method = c("BoxCox","center","scale","pca"))
  testPredictions.a1<-as.numeric(predict(mdaModel.a1,tests.ab))
  testPredictions.a2<-as.numeric(predict(mdaModel.a2,tests.ab))
  testPredictions.a3<-as.numeric(predict(mdaModel.a3,tests.ab))
  testPredictions.a4<-as.numeric(predict(mdaModel.a4,tests.ab))
  testPredictions.a5<-as.numeric(predict(mdaModel.a5,tests.ab))
  testPredictions.b1<-as.numeric(predict(mdaModel.b1,tests.ab))
  testPredictions.b2<-as.numeric(predict(mdaModel.b2,tests.ab))
  
  a1.Predictions<-testPredictions.a1+a1.d
  a2.Predictions<-testPredictions.a2+a2.d
  a3.Predictions<-testPredictions.a3+a3.d
  a4.Predictions<-testPredictions.a4+a4.d
  a5.Predictions<-testPredictions.a5+a5.d
  b1.Predictions<-testPredictions.b1+b1.d
  b2.Predictions<-testPredictions.b2+b2.d
  
  
  dlt.p.table(dlt,a1.Predictions,
              a2.Predictions,a3.Predictions,a4.Predictions,
              a5.Predictions,b1.Predictions,b2.Predictions)
  
  #  return(c(
  #    tail(round(testPredictions.a1),1),
  #    tail(round(testPredictions.a2),1),
  #    tail(round(testPredictions.a3),1),
  #    tail(round(testPredictions.a4),1),
  #    tail(round(testPredictions.a5),1),
  #    tail(round(testPredictions.b1),1),
  #    tail(round(testPredictions.b2),1)
  #  ))
  
  a1<-round(a1.Predictions)
  a2<-round(a2.Predictions)
  a3<-round(a3.Predictions)
  a4<-round(a4.Predictions)
  a5<-round(a5.Predictions)
  b1<-round(b1.Predictions)
  b2<-round(b2.Predictions)
  
  result.ab<-data.frame(a1,a2,a3,a4,a5,b1,b2)
  return(result.ab)
  
}


