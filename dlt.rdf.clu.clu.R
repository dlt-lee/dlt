dlt.rdf.clu.clu <- function(data){
  library(randomForest)
  
  pre.m.a1<-matrix(data$pre.a1,ncol = 16,byrow = TRUE)
  pre.m.a2<-matrix(data$pre.a2,ncol = 16,byrow = TRUE)
  pre.m.a3<-matrix(data$pre.a3,ncol = 16,byrow = TRUE)
  pre.m.a4<-matrix(data$pre.a4,ncol = 16,byrow = TRUE)
  pre.m.a5<-matrix(data$pre.a5,ncol = 16,byrow = TRUE)
  pre.m.b1<-matrix(data$pre.b1,ncol = 16,byrow = TRUE)
  pre.m.b2<-matrix(data$pre.b2,ncol = 16,byrow = TRUE)
  
  
  pre.a1.1<-pre.m.a1[,1]
  pre.a1.2<-pre.m.a1[,2]
  pre.a1.3<-pre.m.a1[,3]
  pre.a1.4<-pre.m.a1[,4]
  pre.a1.5<-pre.m.a1[,5]
  pre.a1.6<-pre.m.a1[,6]
  pre.a1.7<-pre.m.a1[,7]
  pre.a1.8<-pre.m.a1[,8]
  pre.a1.9<-pre.m.a1[,9]
  pre.a1.10<-pre.m.a1[,10]
  pre.a1.11<-pre.m.a1[,11]
  pre.a1.12<-pre.m.a1[,12]
  pre.a1.13<-pre.m.a1[,13]
  pre.a1.14<-pre.m.a1[,14]
  pre.a1.15<-pre.m.a1[,15]
  pre.a1.16<-pre.m.a1[,16]
  exp.a1<-data$exp.a1[1:90]
  trains.a1<-data.frame(pre.a1.1,pre.a1.2,pre.a1.3,pre.a1.4,pre.a1.5,pre.a1.6,
                        pre.a1.7,pre.a1.8,pre.a1.9,pre.a1.10,pre.a1.11,pre.a1.12,
                        pre.a1.13,pre.a1.14,pre.a1.15,pre.a1.16,exp.a1)
  
  pre.a2.1<-pre.m.a2[,1]
  pre.a2.2<-pre.m.a2[,2]
  pre.a2.3<-pre.m.a2[,3]
  pre.a2.4<-pre.m.a2[,4]
  pre.a2.5<-pre.m.a2[,5]
  pre.a2.6<-pre.m.a2[,6]
  pre.a2.7<-pre.m.a2[,7]
  pre.a2.8<-pre.m.a2[,8]
  pre.a2.9<-pre.m.a2[,9]
  pre.a2.10<-pre.m.a2[,10]
  pre.a2.11<-pre.m.a2[,11]
  pre.a2.12<-pre.m.a2[,12]
  pre.a2.13<-pre.m.a2[,13]
  pre.a2.14<-pre.m.a2[,14]
  pre.a2.15<-pre.m.a2[,15]
  pre.a2.16<-pre.m.a2[,16]
  exp.a2<-data$exp.a2[1:90]
  trains.a2<-data.frame(pre.a2.1,pre.a2.2,pre.a2.3,pre.a2.4,pre.a2.5,pre.a2.6,
                        pre.a2.7,pre.a2.8,pre.a2.9,pre.a2.10,pre.a2.11,pre.a2.12,
                        pre.a2.13,pre.a2.14,pre.a2.15,pre.a2.16,exp.a2)
  
  pre.a3.1<-pre.m.a3[,1]
  pre.a3.2<-pre.m.a3[,2]
  pre.a3.3<-pre.m.a3[,3]
  pre.a3.4<-pre.m.a3[,4]
  pre.a3.5<-pre.m.a3[,5]
  pre.a3.6<-pre.m.a3[,6]
  pre.a3.7<-pre.m.a3[,7]
  pre.a3.8<-pre.m.a3[,8]
  pre.a3.9<-pre.m.a3[,9]
  pre.a3.10<-pre.m.a3[,10]
  pre.a3.11<-pre.m.a3[,11]
  pre.a3.12<-pre.m.a3[,12]
  pre.a3.13<-pre.m.a3[,13]
  pre.a3.14<-pre.m.a3[,14]
  pre.a3.15<-pre.m.a3[,15]
  pre.a3.16<-pre.m.a3[,16]
  exp.a3<-data$exp.a3[1:90]
  trains.a3<-data.frame(pre.a3.1,pre.a3.2,pre.a3.3,pre.a3.4,pre.a3.5,pre.a3.6,
                        pre.a3.7,pre.a3.8,pre.a3.9,pre.a3.10,pre.a3.11,pre.a3.12,
                        pre.a3.13,pre.a3.14,pre.a3.15,pre.a3.16,exp.a3)
  
  pre.a4.1<-pre.m.a4[,1]
  pre.a4.2<-pre.m.a4[,2]
  pre.a4.3<-pre.m.a4[,3]
  pre.a4.4<-pre.m.a4[,4]
  pre.a4.5<-pre.m.a4[,5]
  pre.a4.6<-pre.m.a4[,6]
  pre.a4.7<-pre.m.a4[,7]
  pre.a4.8<-pre.m.a4[,8]
  pre.a4.9<-pre.m.a4[,9]
  pre.a4.10<-pre.m.a4[,10]
  pre.a4.11<-pre.m.a4[,11]
  pre.a4.12<-pre.m.a4[,12]
  pre.a4.13<-pre.m.a4[,13]
  pre.a4.14<-pre.m.a4[,14]
  pre.a4.15<-pre.m.a4[,15]
  pre.a4.16<-pre.m.a4[,16]
  exp.a4<-data$exp.a4[1:90]
  trains.a4<-data.frame(pre.a4.1,pre.a4.2,pre.a4.3,pre.a4.4,pre.a4.5,pre.a4.6,
                        pre.a4.7,pre.a4.8,pre.a4.9,pre.a4.10,pre.a4.11,pre.a4.12,
                        pre.a4.13,pre.a4.14,pre.a4.15,pre.a4.16,exp.a4)
  
  pre.a5.1<-pre.m.a5[,1]
  pre.a5.2<-pre.m.a5[,2]
  pre.a5.3<-pre.m.a5[,3]
  pre.a5.4<-pre.m.a5[,4]
  pre.a5.5<-pre.m.a5[,5]
  pre.a5.6<-pre.m.a5[,6]
  pre.a5.7<-pre.m.a5[,7]
  pre.a5.8<-pre.m.a5[,8]
  pre.a5.9<-pre.m.a5[,9]
  pre.a5.10<-pre.m.a5[,10]
  pre.a5.11<-pre.m.a5[,11]
  pre.a5.12<-pre.m.a5[,12]
  pre.a5.13<-pre.m.a5[,13]
  pre.a5.14<-pre.m.a5[,14]
  pre.a5.15<-pre.m.a5[,15]
  pre.a5.16<-pre.m.a5[,16]
  exp.a5<-data$exp.a5[1:90]
  trains.a5<-data.frame(pre.a5.1,pre.a5.2,pre.a5.3,pre.a5.4,pre.a5.5,pre.a5.6,
                        pre.a5.7,pre.a5.8,pre.a5.9,pre.a5.10,pre.a5.11,pre.a5.12,
                        pre.a5.13,pre.a5.14,pre.a5.15,pre.a5.16,exp.a5)
  
  pre.b1.1<-pre.m.b1[,1]
  pre.b1.2<-pre.m.b1[,2]
  pre.b1.3<-pre.m.b1[,3]
  pre.b1.4<-pre.m.b1[,4]
  pre.b1.5<-pre.m.b1[,5]
  pre.b1.6<-pre.m.b1[,6]
  pre.b1.7<-pre.m.b1[,7]
  pre.b1.8<-pre.m.b1[,8]
  pre.b1.9<-pre.m.b1[,9]
  pre.b1.10<-pre.m.b1[,10]
  pre.b1.11<-pre.m.b1[,11]
  pre.b1.12<-pre.m.b1[,12]
  pre.b1.13<-pre.m.b1[,13]
  pre.b1.14<-pre.m.b1[,14]
  pre.b1.15<-pre.m.b1[,15]
  pre.b1.16<-pre.m.b1[,16]
  exp.b1<-data$exp.b1[1:90]
  trains.b1<-data.frame(pre.b1.1,pre.b1.2,pre.b1.3,pre.b1.4,pre.b1.5,pre.b1.6,
                        pre.b1.7,pre.b1.8,pre.b1.9,pre.b1.10,pre.b1.11,pre.b1.12,
                        pre.b1.13,pre.b1.14,pre.b1.15,pre.b1.16,exp.b1)
  
  pre.b2.1<-pre.m.b2[,1]
  pre.b2.2<-pre.m.b2[,2]
  pre.b2.3<-pre.m.b2[,3]
  pre.b2.4<-pre.m.b2[,4]
  pre.b2.5<-pre.m.b2[,5]
  pre.b2.6<-pre.m.b2[,6]
  pre.b2.7<-pre.m.b2[,7]
  pre.b2.8<-pre.m.b2[,8]
  pre.b2.9<-pre.m.b2[,9]
  pre.b2.10<-pre.m.b2[,10]
  pre.b2.11<-pre.m.b2[,11]
  pre.b2.12<-pre.m.b2[,12]
  pre.b2.13<-pre.m.b2[,13]
  pre.b2.14<-pre.m.b2[,14]
  pre.b2.15<-pre.m.b2[,15]
  pre.b2.16<-pre.m.b2[,16]
  exp.b2<-data$exp.b2[1:90]
  trains.b2<-data.frame(pre.b2.1,pre.b2.2,pre.b2.3,pre.b2.4,pre.b2.5,pre.b2.6,
                        pre.b2.7,pre.b2.8,pre.b2.9,pre.b2.10,pre.b2.11,pre.b2.12,
                        pre.b2.13,pre.b2.14,pre.b2.15,pre.b2.16,exp.b2)
  
  rf.a1 = randomForest(exp.a1 ~ pre.a1.1+pre.a1.2+pre.a1.3+pre.a1.4+pre.a1.5+
                         pre.a1.6+pre.a1.7+pre.a1.8+pre.a1.9+pre.a1.10+
                         pre.a1.11+pre.a1.12+pre.a1.13+pre.a1.14+pre.a1.15+
                         pre.a1.16,data = trains.a1,
                       importance = T
                       #,ntrees=2000
                       )
  rf.a2 = randomForest(exp.a2 ~ pre.a2.1+pre.a2.2+pre.a2.3+pre.a2.4+pre.a2.5+
                         pre.a2.6+pre.a2.7+pre.a2.8+pre.a2.9+pre.a2.10+
                         pre.a2.11+pre.a2.12+pre.a2.13+pre.a2.14+pre.a2.15+
                         pre.a2.16,data = trains.a2,
                       importance = T
                       #,ntrees=2000
                       )
  rf.a3 = randomForest(exp.a3 ~ pre.a3.1+pre.a3.2+pre.a1.3+pre.a3.4+pre.a3.5+
                         pre.a3.6+pre.a3.7+pre.a3.8+pre.a3.9+pre.a3.10+
                         pre.a3.11+pre.a3.12+pre.a3.13+pre.a3.14+pre.a3.15+
                         pre.a3.16,data = trains.a3,
                       importance = T
                       #,ntrees=2000
                       )
  rf.a4 = randomForest(exp.a4 ~ pre.a4.1+pre.a4.2+pre.a4.3+pre.a4.4+pre.a4.5+
                         pre.a4.6+pre.a4.7+pre.a4.8+pre.a4.9+pre.a4.10+
                         pre.a4.11+pre.a4.12+pre.a4.13+pre.a4.14+pre.a4.15+
                         pre.a4.16,data = trains.a4,
                       importance = T
                       #,ntrees=2000
                       )
  rf.a5 = randomForest(exp.a5 ~ pre.a5.1+pre.a5.2+pre.a5.3+pre.a5.4+pre.a5.5+
                         pre.a5.6+pre.a5.7+pre.a5.8+pre.a5.9+pre.a5.10+
                         pre.a5.11+pre.a5.12+pre.a5.13+pre.a5.14+pre.a5.15+
                         pre.a5.16,data = trains.a5,
                       importance = T
                       #,ntrees=2000
                       )
  rf.b1 = randomForest(exp.b1 ~ pre.b1.1+pre.b1.2+pre.b1.3+pre.b1.4+pre.b1.5+
                         pre.b1.6+pre.b1.7+pre.b1.8+pre.b1.9+pre.b1.10+
                         pre.b1.11+pre.b1.12+pre.b1.13+pre.b1.14+pre.b1.15+
                         pre.b1.16,data = trains.b1,
                       importance = T
                       #,ntrees=2000
                       )
  rf.b2 = randomForest(exp.b2 ~ pre.b2.1+pre.b2.2+pre.b2.3+pre.b2.4+pre.b2.5+
                         pre.b2.6+pre.b2.7+pre.b2.8+pre.b2.9+pre.b2.10+
                         pre.b2.11+pre.b2.12+pre.b2.13+pre.b2.14+pre.b2.15+
                         pre.b2.16,data = trains.b2,
                       importance = T
                       #,ntrees=2000
                       )
  
  data<-tail(dlt,300)
  data.tr<-dlt.data.filter(data)
  pre.final.data<-dlt.xgboost.clu(data,data.tr)
  
  pre.a1.1<-pre.final.data$a1.clu[1]
  pre.a1.2<-pre.final.data$a1.clu[2]
  pre.a1.3<-pre.final.data$a1.clu[3]
  pre.a1.4<-pre.final.data$a1.clu[4]
  pre.a1.5<-pre.final.data$a1.clu[5]
  pre.a1.6<-pre.final.data$a1.clu[6]
  pre.a1.7<-pre.final.data$a1.clu[7]
  pre.a1.8<-pre.final.data$a1.clu[8]
  pre.a1.9<-pre.final.data$a1.clu[9]
  pre.a1.10<-pre.final.data$a1.clu[10]
  pre.a1.11<-pre.final.data$a1.clu[11]
  pre.a1.12<-pre.final.data$a1.clu[12]
  pre.a1.13<-pre.final.data$a1.clu[13]
  pre.a1.14<-pre.final.data$a1.clu[14]
  pre.a1.15<-pre.final.data$a1.clu[15]
  pre.a1.16<-pre.final.data$a1.clu[16]
  test.a1<-data.frame(pre.a1.1,pre.a1.2,pre.a1.3,pre.a1.4,pre.a1.5,pre.a1.6,
                     pre.a1.7,pre.a1.8,pre.a1.9,pre.a1.10,pre.a1.11,pre.a1.12,
                     pre.a1.13,pre.a1.14,pre.a1.15,pre.a1.16)
  
  pre.a2.1<-pre.final.data$a2.clu[1]
  pre.a2.2<-pre.final.data$a2.clu[2]
  pre.a2.3<-pre.final.data$a2.clu[3]
  pre.a2.4<-pre.final.data$a2.clu[4]
  pre.a2.5<-pre.final.data$a2.clu[5]
  pre.a2.6<-pre.final.data$a2.clu[6]
  pre.a2.7<-pre.final.data$a2.clu[7]
  pre.a2.8<-pre.final.data$a2.clu[8]
  pre.a2.9<-pre.final.data$a2.clu[9]
  pre.a2.10<-pre.final.data$a2.clu[10]
  pre.a2.11<-pre.final.data$a2.clu[11]
  pre.a2.12<-pre.final.data$a2.clu[12]
  pre.a2.13<-pre.final.data$a2.clu[13]
  pre.a2.14<-pre.final.data$a2.clu[14]
  pre.a2.15<-pre.final.data$a2.clu[15]
  pre.a2.16<-pre.final.data$a2.clu[16]
  test.a2<-data.frame(pre.a2.1,pre.a2.2,pre.a2.3,pre.a2.4,pre.a2.5,pre.a2.6,
                      pre.a2.7,pre.a2.8,pre.a2.9,pre.a2.10,pre.a2.11,pre.a2.12,
                      pre.a2.13,pre.a2.14,pre.a2.15,pre.a2.16)
  
  pre.a3.1<-pre.final.data$a3.clu[1]
  pre.a3.2<-pre.final.data$a3.clu[2]
  pre.a3.3<-pre.final.data$a3.clu[3]
  pre.a3.4<-pre.final.data$a3.clu[4]
  pre.a3.5<-pre.final.data$a3.clu[5]
  pre.a3.6<-pre.final.data$a3.clu[6]
  pre.a3.7<-pre.final.data$a3.clu[7]
  pre.a3.8<-pre.final.data$a3.clu[8]
  pre.a3.9<-pre.final.data$a3.clu[9]
  pre.a3.10<-pre.final.data$a3.clu[10]
  pre.a3.11<-pre.final.data$a3.clu[11]
  pre.a3.12<-pre.final.data$a3.clu[12]
  pre.a3.13<-pre.final.data$a3.clu[13]
  pre.a3.14<-pre.final.data$a3.clu[14]
  pre.a3.15<-pre.final.data$a3.clu[15]
  pre.a3.16<-pre.final.data$a3.clu[16]
  test.a3<-data.frame(pre.a3.1,pre.a3.2,pre.a3.3,pre.a3.4,pre.a3.5,pre.a3.6,
                      pre.a3.7,pre.a3.8,pre.a3.9,pre.a3.10,pre.a3.11,pre.a3.12,
                      pre.a3.13,pre.a3.14,pre.a3.15,pre.a3.16)
  
  pre.a4.1<-pre.final.data$a4.clu[1]
  pre.a4.2<-pre.final.data$a4.clu[2]
  pre.a4.3<-pre.final.data$a4.clu[3]
  pre.a4.4<-pre.final.data$a4.clu[4]
  pre.a4.5<-pre.final.data$a4.clu[5]
  pre.a4.6<-pre.final.data$a4.clu[6]
  pre.a4.7<-pre.final.data$a4.clu[7]
  pre.a4.8<-pre.final.data$a4.clu[8]
  pre.a4.9<-pre.final.data$a4.clu[9]
  pre.a4.10<-pre.final.data$a4.clu[10]
  pre.a4.11<-pre.final.data$a4.clu[11]
  pre.a4.12<-pre.final.data$a4.clu[12]
  pre.a4.13<-pre.final.data$a4.clu[13]
  pre.a4.14<-pre.final.data$a4.clu[14]
  pre.a4.15<-pre.final.data$a4.clu[15]
  pre.a4.16<-pre.final.data$a4.clu[16]
  test.a4<-data.frame(pre.a4.1,pre.a4.2,pre.a4.3,pre.a4.4,pre.a4.5,pre.a4.6,
                      pre.a4.7,pre.a4.8,pre.a4.9,pre.a4.10,pre.a4.11,pre.a4.12,
                      pre.a4.13,pre.a4.14,pre.a4.15,pre.a4.16)
  
  pre.a5.1<-pre.final.data$a5.clu[1]
  pre.a5.2<-pre.final.data$a5.clu[2]
  pre.a5.3<-pre.final.data$a5.clu[3]
  pre.a5.4<-pre.final.data$a5.clu[4]
  pre.a5.5<-pre.final.data$a5.clu[5]
  pre.a5.6<-pre.final.data$a5.clu[6]
  pre.a5.7<-pre.final.data$a5.clu[7]
  pre.a5.8<-pre.final.data$a5.clu[8]
  pre.a5.9<-pre.final.data$a5.clu[9]
  pre.a5.10<-pre.final.data$a5.clu[10]
  pre.a5.11<-pre.final.data$a5.clu[11]
  pre.a5.12<-pre.final.data$a5.clu[12]
  pre.a5.13<-pre.final.data$a5.clu[13]
  pre.a5.14<-pre.final.data$a5.clu[14]
  pre.a5.15<-pre.final.data$a5.clu[15]
  pre.a5.16<-pre.final.data$a5.clu[16]
  test.a5<-data.frame(pre.a5.1,pre.a5.2,pre.a5.3,pre.a5.4,pre.a5.5,pre.a5.6,
                      pre.a5.7,pre.a5.8,pre.a5.9,pre.a5.10,pre.a5.11,pre.a5.12,
                      pre.a5.13,pre.a5.14,pre.a5.15,pre.a5.16)
  
  pre.b1.1<-pre.final.data$b1.clu[1]
  pre.b1.2<-pre.final.data$b1.clu[2]
  pre.b1.3<-pre.final.data$b1.clu[3]
  pre.b1.4<-pre.final.data$b1.clu[4]
  pre.b1.5<-pre.final.data$b1.clu[5]
  pre.b1.6<-pre.final.data$b1.clu[6]
  pre.b1.7<-pre.final.data$b1.clu[7]
  pre.b1.8<-pre.final.data$b1.clu[8]
  pre.b1.9<-pre.final.data$b1.clu[9]
  pre.b1.10<-pre.final.data$b1.clu[10]
  pre.b1.11<-pre.final.data$b1.clu[11]
  pre.b1.12<-pre.final.data$b1.clu[12]
  pre.b1.13<-pre.final.data$b1.clu[13]
  pre.b1.14<-pre.final.data$b1.clu[14]
  pre.b1.15<-pre.final.data$b1.clu[15]
  pre.b1.16<-pre.final.data$b1.clu[16]
  test.b1<-data.frame(pre.b1.1,pre.b1.2,pre.b1.3,pre.b1.4,pre.b1.5,pre.b1.6,
                      pre.b1.7,pre.b1.8,pre.b1.9,pre.b1.10,pre.b1.11,pre.b1.12,
                      pre.b1.13,pre.b1.14,pre.b1.15,pre.b1.16)
  
  pre.b2.1<-pre.final.data$b2.clu[1]
  pre.b2.2<-pre.final.data$b2.clu[2]
  pre.b2.3<-pre.final.data$b2.clu[3]
  pre.b2.4<-pre.final.data$b2.clu[4]
  pre.b2.5<-pre.final.data$b2.clu[5]
  pre.b2.6<-pre.final.data$b2.clu[6]
  pre.b2.7<-pre.final.data$b2.clu[7]
  pre.b2.8<-pre.final.data$b2.clu[8]
  pre.b2.9<-pre.final.data$b2.clu[9]
  pre.b2.10<-pre.final.data$b2.clu[10]
  pre.b2.11<-pre.final.data$b2.clu[11]
  pre.b2.12<-pre.final.data$b2.clu[12]
  pre.b2.13<-pre.final.data$b2.clu[13]
  pre.b2.14<-pre.final.data$b2.clu[14]
  pre.b2.15<-pre.final.data$b2.clu[15]
  pre.b2.16<-pre.final.data$b2.clu[16]
  test.b2<-data.frame(pre.b2.1,pre.b2.2,pre.b2.3,pre.b2.4,pre.b2.5,pre.b2.6,
                      pre.b2.7,pre.b2.8,pre.b2.9,pre.b2.10,pre.b2.11,pre.b2.12,
                      pre.b2.13,pre.b2.14,pre.b2.15,pre.b2.16)
  
  testPredictions.a1<-predict(rf.a1,test.a1)
  testPredictions.a2<-predict(rf.a2,test.a2)
  testPredictions.a3<-predict(rf.a3,test.a3)
  testPredictions.a4<-predict(rf.a4,test.a4)
  testPredictions.a5<-predict(rf.a5,test.a5)
  testPredictions.b1<-predict(rf.b1,test.b1)
  testPredictions.b2<-predict(rf.b2,test.b2)
  
  return(c(round(testPredictions.a1),round(testPredictions.a2),
           round(testPredictions.a3),round(testPredictions.a4),
           round(testPredictions.a5),
           round(testPredictions.b1),round(testPredictions.b2)))
  
}















