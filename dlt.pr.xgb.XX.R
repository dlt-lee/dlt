data<-dlt
count<-dim(dlt)[1]
seed<-1
dlt.pr.xgb.XX <- function(data,count,seed) {
  library(xgboost)
  library(caret)
  
  trains_1  <-tail(data,count)[1:(count-20),]
  trains_2  <-tail(data,count)[2:(count-19),]
  trains_3  <-tail(data,count)[3:(count-18),]
  trains_4  <-tail(data,count)[4:(count-17),]
  trains_5  <-tail(data,count)[5:(count-16),]
  trains_6  <-tail(data,count)[6:(count-15),]
  trains_7  <-tail(data,count)[7:(count-14),]
  trains_8  <-tail(data,count)[8:(count-13),]
  trains_9  <-tail(data,count)[9:(count-12),]
  trains_10<-tail(data,count)[10:(count-11),]
  trains_11<-tail(data,count)[11:(count-10),]
  trains_12 <-tail(data,count)[12:(count-9),]
  trains_13 <-tail(data,count)[13:(count-8),]
  trains_14 <-tail(data,count)[14:(count-7),]
  trains_15 <-tail(data,count)[15:(count-6),]
  trains_16 <-tail(data,count)[16:(count-5),]
  trains_17 <-tail(data,count)[17:(count-4),]
  trains_18 <-tail(data,count)[18:(count-3),]
  trains_19 <-tail(data,count)[19:(count-2),]
  trains_20 <-tail(data,count)[20:(count-1),]
  results<-tail(data,(count-20))
  tests_1  <-tail(data,count)[1:(count-19),]
  tests_2  <-tail(data,count)[2:(count-18),]
  tests_3  <-tail(data,count)[3:(count-17),]
  tests_4  <-tail(data,count)[4:(count-16),]
  tests_5  <-tail(data,count)[5:(count-15),]
  tests_6  <-tail(data,count)[6:(count-14),]
  tests_7  <-tail(data,count)[7:(count-13),]
  tests_8  <-tail(data,count)[8:(count-12),]
  tests_9  <-tail(data,count)[9:(count-11),]
  tests_10<-tail(data,count)[10:(count-10),]
  tests_11 <-tail(data,count)[11:(count-9),]
  tests_12 <-tail(data,count)[12:(count-8),]
  tests_13 <-tail(data,count)[13:(count-7),]
  tests_14 <-tail(data,count)[14:(count-6),]
  tests_15 <-tail(data,count)[15:(count-5),]
  tests_16 <-tail(data,count)[16:(count-4),]
  tests_17 <-tail(data,count)[17:(count-3),]
  tests_18 <-tail(data,count)[18:(count-2),]
  tests_19 <-tail(data,count)[19:(count-1),]
  tests_20 <-tail(data,count-19)
  
  #A:
  trn1<-trains_1$n
  trn2<-trains_2$n
  trn3<-trains_3$n
  
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
  b1.4<-trains_1$b1
  b2.4<-trains_1$b2
  b1.5<-trains_2$b1
  b2.5<-trains_2$b2
  b1.6<-trains_3$b1
  b2.6<-trains_3$b2
  b1.7<-trains_1$b1
  b2.7<-trains_1$b2
  b1.8<-trains_1$b1
  b2.8<-trains_1$b2
  b1.9<-trains_2$b1
  b2.9<-trains_2$b2
  
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
  
  resb1<-results$b1
  resb2<-results$b2
  
  
  trains.a1<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resa1)
  trains.a2<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resa2)
  trains.a3<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resa3)
  trains.a4<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resa4)
  trains.a5<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resa5)
  trains.b1<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resb1)
  trains.b2<-data.frame(trn1,trn2,trn3,
                        a1.1,a2.1,a3.1,a4.1,a5.1,
                        a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        a1.4,a2.4,a3.4,a4.4,a5.4,
                        a1.5,a2.5,a3.5,a4.5,a5.5,
                        a1.6,a2.6,a3.6,a4.6,a5.6,
                        a1.7,a2.7,a3.7,a4.7,a5.7,
                        a1.8,a2.8,a3.8,a4.8,a5.8,
                        a1.9,a2.9,a3.9,a4.9,a5.9,
                        
                        a1.10,a2.10,a3.10,a4.10,a5.10,
                        a1.11,a2.11,a3.11,a4.11,a5.11,
                        a1.12,a2.12,a3.12,a4.12,a5.12,
                        a1.13,a2.13,a3.13,a4.13,a5.13,
                        a1.14,a2.14,a3.14,a4.14,a5.14,
                        a1.15,a2.15,a3.15,a4.15,a5.15,
                        a1.16,a2.16,a3.16,a4.16,a5.16,
                        a1.17,a2.17,a3.17,a4.17,a5.17,
                        a1.18,a2.18,a3.18,a4.18,a5.18,
                        a1.19,a2.19,a3.19,a4.19,a5.19,
                        
                        a1.20,a2.20,a3.20,a4.20,a5.20,
                        
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        
                        b1.10,b2.10,
                        b1.11,b2.11,
                        b1.12,b2.12,
                        b1.13,b2.13,
                        b1.14,b2.14,
                        b1.15,b2.15,
                        b1.16,b2.16,
                        b1.17,b2.17,
                        b1.18,b2.18,
                        b1.19,b2.19,
                        
                        b1.20,b2.20,
                        
                        resb2)
  
  set.seed(seed)
  repeatedSplits.a1<-createDataPartition(trains.a1$resa1,p = .80)
  repeatedSplits.a2<-createDataPartition(trains.a2$resa2,p = .80)
  repeatedSplits.a3<-createDataPartition(trains.a3$resa3,p = .80)
  repeatedSplits.a4<-createDataPartition(trains.a4$resa4,p = .80)
  repeatedSplits.a5<-createDataPartition(trains.a5$resa5,p = .80)
  repeatedSplits.b1<-createDataPartition(trains.b1$resb1,p = .80)
  repeatedSplits.b2<-createDataPartition(trains.b2$resb2,p = .80)
  
  train.a1<-trains.a1[repeatedSplits.a1$Resample1,]
  train.a2<-trains.a2[repeatedSplits.a2$Resample1,]
  train.a3<-trains.a3[repeatedSplits.a3$Resample1,]
  train.a4<-trains.a4[repeatedSplits.a4$Resample1,]
  train.a5<-trains.a5[repeatedSplits.a5$Resample1,]
  train.b1<-trains.b1[repeatedSplits.b1$Resample1,]
  train.b2<-trains.b2[repeatedSplits.b2$Resample1,]
  
  train.T.a1<-Matrix(as.matrix(train.a1[,4:143]),sparse=T)
  train.T.a2<-Matrix(as.matrix(train.a2[,4:143]),sparse=T)
  train.T.a3<-Matrix(as.matrix(train.a3[,4:143]),sparse=T)
  train.T.a4<-Matrix(as.matrix(train.a4[,4:143]),sparse=T)
  train.T.a5<-Matrix(as.matrix(train.a5[,4:143]),sparse=T)
  train.T.b1<-Matrix(as.matrix(train.b1[,4:143]),sparse=T)
  train.T.b2<-Matrix(as.matrix(train.b2[,4:143]),sparse=T)
  
  n=300
  bst.a1 <- xgboost(data = train.T.a1,label = train.a1$resa1,nrounds = n)
  bst.a2 <- xgboost(data = train.T.a2,label = train.a2$resa2,nrounds = n)
  bst.a3 <- xgboost(data = train.T.a3,label = train.a3$resa3,nrounds = n)
  bst.a4 <- xgboost(data = train.T.a4,label = train.a4$resa4,nrounds = n)
  bst.a5 <- xgboost(data = train.T.a5,label = train.a5$resa5,nrounds = n)
  bst.b1 <- xgboost(data = train.T.b1,label = train.b1$resb1,nrounds = n)
  bst.b2 <- xgboost(data = train.T.b2,label = train.b2$resb2,nrounds = n)
  
  result.a1<-trains.a1[-repeatedSplits.a1$Resample1,]
  result.a2<-trains.a2[-repeatedSplits.a2$Resample1,]
  result.a3<-trains.a3[-repeatedSplits.a3$Resample1,]
  result.a4<-trains.a4[-repeatedSplits.a4$Resample1,]
  result.a5<-trains.a5[-repeatedSplits.a5$Resample1,]
  result.b1<-trains.b1[-repeatedSplits.b1$Resample1,]
  result.b2<-trains.b2[-repeatedSplits.b2$Resample1,]
  
  result.T.a1<-Matrix(as.matrix(result.a1[,4:143]),sparse=T)
  result.T.a2<-Matrix(as.matrix(result.a2[,4:143]),sparse=T)
  result.T.a3<-Matrix(as.matrix(result.a3[,4:143]),sparse=T)
  result.T.a4<-Matrix(as.matrix(result.a4[,4:143]),sparse=T)
  result.T.a5<-Matrix(as.matrix(result.a5[,4:143]),sparse=T)
  result.T.b1<-Matrix(as.matrix(result.b1[,4:143]),sparse=T)
  result.T.b2<-Matrix(as.matrix(result.b2[,4:143]),sparse=T)
  
  testPredictions.a1 <- predict(object = bst.a1,newdata = result.T.a1)
  testPredictions.a2 <- predict(object = bst.a2,newdata = result.T.a2)
  testPredictions.a3 <- predict(object = bst.a3,newdata = result.T.a3)
  testPredictions.a4 <- predict(object = bst.a4,newdata = result.T.a4)
  testPredictions.a5 <- predict(object = bst.a5,newdata = result.T.a5)
  testPredictions.b1 <- predict(object = bst.b1,newdata = result.T.b1)
  testPredictions.b2 <- predict(object = bst.b2,newdata = result.T.b2)
  
  a1.Predictions<-round(testPredictions.a1)
  a2.Predictions<-round(testPredictions.a2)
  a3.Predictions<-round(testPredictions.a3)
  a4.Predictions<-round(testPredictions.a4)
  a5.Predictions<-round(testPredictions.a5)
  b1.Predictions<-round(testPredictions.b1)
  b2.Predictions<-round(testPredictions.b2)
  
  
  rows<-min(length(testPredictions.a1),
            length(testPredictions.a2),
            length(testPredictions.a3),
            length(testPredictions.a4),
            length(testPredictions.a5),
            length(testPredictions.b1),
            length(testPredictions.b2))
  
  barplot(result.a1$resa1-a1.Predictions,main = "a1")
  barplot(result.a2$resa2-a2.Predictions,main = "a2")
  barplot(result.a3$resa3-a3.Predictions,main = "a3")
  barplot(result.a4$resa4-a4.Predictions,main = "a4")
  barplot(result.a5$resa5-a5.Predictions,main = "a5")
  barplot(result.b1$resb1-b1.Predictions,main = "b1")
  barplot(result.b2$resb2-b2.Predictions,main = "b2")
  
  
  #Buil test data
  #A:
  tsn1<-tests_1$n
  tsn2<-tests_2$n
  tsn3<-tests_3$n
  
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
  
  tests.ab<-data.frame(tsn1,tsn2,tsn3,
                       a1.1,a2.1,a3.1,a4.1,a5.1,
                       a1.2,a2.2,a3.2,a4.2,a5.2,
                       a1.3,a2.3,a3.3,a4.3,a5.3,
                       a1.4,a2.4,a3.4,a4.4,a5.4,
                       a1.5,a2.5,a3.5,a4.5,a5.5,
                       a1.6,a2.6,a3.6,a4.6,a5.6,
                       a1.7,a2.7,a3.7,a4.7,a5.7,
                       a1.8,a2.8,a3.8,a4.8,a5.8,
                       a1.9,a2.9,a3.9,a4.9,a5.9,
                       
                       a1.10,a2.10,a3.10,a4.10,a5.10,
                       a1.11,a2.11,a3.11,a4.11,a5.11,
                       a1.12,a2.12,a3.12,a4.12,a5.12,
                       a1.13,a2.13,a3.13,a4.13,a5.13,
                       a1.14,a2.14,a3.14,a4.14,a5.14,
                       a1.15,a2.15,a3.15,a4.15,a5.15,
                       a1.16,a2.16,a3.16,a4.16,a5.16,
                       a1.17,a2.17,a3.17,a4.17,a5.17,
                       a1.18,a2.18,a3.18,a4.18,a5.18,
                       a1.19,a2.19,a3.19,a4.19,a5.19,
                       
                       a1.20,a2.20,a3.20,a4.20,a5.20,
                       
                       b1.1,b2.1,
                       b1.2,b2.2,
                       b1.3,b2.3,
                       b1.4,b2.4,
                       b1.5,b2.5,
                       b1.6,b2.6,
                       b1.7,b2.7,
                       b1.8,b2.8,
                       b1.9,b2.9,
                       
                       b1.10,b2.10,
                       b1.11,b2.11,
                       b1.12,b2.12,
                       b1.13,b2.13,
                       b1.14,b2.14,
                       b1.15,b2.15,
                       b1.16,b2.16,
                       b1.17,b2.17,
                       b1.18,b2.18,
                       b1.19,b2.19,
                       
                       b1.20,b2.20)
  
  tests.T.ab<-Matrix(as.matrix(tests.ab[,4:143]),sparse=T)
  
  testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.ab)
  testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.ab)
  testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.ab)
  testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.ab)
  testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.ab)
  testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.ab)
  testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.ab)
  
  dlt.p.table(dlt,
              round(testPredictions.a1),round(testPredictions.a2),
              round(testPredictions.a3),round(testPredictions.a4),
              round(testPredictions.a5),
              round(testPredictions.b1),round(testPredictions.b2))
  
  print(c(mean(result.a1$resa1-a1.Predictions),
          mean(result.a2$resa2-a2.Predictions),
          mean(result.a3$resa3-a3.Predictions),
          mean(result.a4$resa4-a4.Predictions),
          mean(result.a5$resa5-a5.Predictions),
          mean(result.b1$resb1-b1.Predictions),
          mean(result.b2$resb2-b2.Predictions)))
  
  return(c(
    tail(round(testPredictions.a1),1),
    tail(round(testPredictions.a2),1),
    tail(round(testPredictions.a3),1),
    tail(round(testPredictions.a4),1),
    tail(round(testPredictions.a5),1),
    tail(round(testPredictions.b1),1),
    tail(round(testPredictions.b2),1)
  ))
  
  
  
  
}








