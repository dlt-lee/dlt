data<-dlt
count<-dim(dlt)[1]
dlt.pr.xgb.IX <- function(data,count,seed) {
  library(xgboost)
  library(caret)
  
  trains_1 <-tail(data,count)[1:(count-9),]
  trains_2 <-tail(data,count)[2:(count-8),]
  trains_3 <-tail(data,count)[3:(count-7),]
  trains_4 <-tail(data,count)[4:(count-6),]
  trains_5 <-tail(data,count)[5:(count-5),]
  trains_6 <-tail(data,count)[6:(count-4),]
  trains_7 <-tail(data,count)[7:(count-3),]
  trains_8 <-tail(data,count)[8:(count-2),]
  trains_9 <-tail(data,count)[9:(count-1),]
  results<-tail(data,(count-9))
  tests_1<-tail(data,count)[1:(count-9),]
  tests_2<-tail(data,count)[2:(count-8),]
  tests_3<-tail(data,count)[3:(count-7),]
  tests_4<-tail(data,count)[4:(count-6),]
  tests_5<-tail(data,count)[5:(count-5),]
  tests_6<-tail(data,count)[6:(count-4),]
  tests_7<-tail(data,count)[7:(count-3),]
  tests_8<-tail(data,count)[8:(count-2),]
  tests_9<-tail(data,count)[9:(count-1),]
  
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
  
  a1.4<-trains_1$a1
  a2.4<-trains_1$a2
  a3.4<-trains_1$a3
  a4.4<-trains_1$a4
  a5.4<-trains_1$a5
  
  a1.5<-trains_1$a1
  a2.5<-trains_1$a2
  a3.5<-trains_1$a3
  a4.5<-trains_1$a4
  a5.5<-trains_1$a5
  
  a1.6<-trains_1$a1
  a2.6<-trains_1$a2
  a3.6<-trains_1$a3
  a4.6<-trains_1$a4
  a5.6<-trains_1$a5
  
  a1.7<-trains_1$a1
  a2.7<-trains_1$a2
  a3.7<-trains_1$a3
  a4.7<-trains_1$a4
  a5.7<-trains_1$a5
  
  a1.8<-trains_1$a1
  a2.8<-trains_1$a2
  a3.8<-trains_1$a3
  a4.8<-trains_1$a4
  a5.8<-trains_1$a5
  
  a1.9<-trains_1$a1
  a2.9<-trains_1$a2
  a3.9<-trains_1$a3
  a4.9<-trains_1$a4
  a5.9<-trains_1$a5
  
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
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
                        b1.1,b2.1,
                        b1.2,b2.2,
                        b1.3,b2.3,
                        b1.4,b2.4,
                        b1.5,b2.5,
                        b1.6,b2.6,
                        b1.7,b2.7,
                        b1.8,b2.8,
                        b1.9,b2.9,
                        resb2)
  
  set.seed(seed)
  repeatedSplits.a1<-createDataPartition(trains.a1$resa1,p = .80,times = 3)
  repeatedSplits.a2<-createDataPartition(trains.a2$resa2,p = .80,times = 3)
  repeatedSplits.a3<-createDataPartition(trains.a3$resa3,p = .80,times = 3)
  repeatedSplits.a4<-createDataPartition(trains.a4$resa4,p = .80,times = 3)
  repeatedSplits.a5<-createDataPartition(trains.a5$resa5,p = .80,times = 3)
  repeatedSplits.b1<-createDataPartition(trains.b1$resb1,p = .80,times = 3)
  repeatedSplits.b2<-createDataPartition(trains.b2$resb2,p = .80,times = 3)
  
  trains.a1<-trains.a1[repeatedSplits.a1$Resample1,]
  trains.a2<-trains.a2[repeatedSplits.a2$Resample1,]
  trains.a3<-trains.a3[repeatedSplits.a3$Resample1,]
  trains.a4<-trains.a4[repeatedSplits.a4$Resample1,]
  trains.a5<-trains.a5[repeatedSplits.a5$Resample1,]
  trains.b1<-trains.b1[repeatedSplits.b1$Resample1,]
  trains.b2<-trains.b2[repeatedSplits.b2$Resample1,]
  
  trains.T.a1<-Matrix(as.matrix(trains.a1[,4:66]),sparse=T)
  trains.T.a2<-Matrix(as.matrix(trains.a2[,4:66]),sparse=T)
  trains.T.a3<-Matrix(as.matrix(trains.a3[,4:66]),sparse=T)
  trains.T.a4<-Matrix(as.matrix(trains.a4[,4:66]),sparse=T)
  trains.T.a5<-Matrix(as.matrix(trains.a5[,4:66]),sparse=T)
  trains.T.b1<-Matrix(as.matrix(trains.b1[,4:66]),sparse=T)
  trains.T.b2<-Matrix(as.matrix(trains.b2[,4:66]),sparse=T)
  
  n=300
  bst.a1 <- xgboost(data = trains.T.a1,label = trains.a1$resa1,nrounds = n)
  bst.a2 <- xgboost(data = trains.T.a2,label = trains.a2$resa2,nrounds = n)
  bst.a3 <- xgboost(data = trains.T.a3,label = trains.a3$resa3,nrounds = n)
  bst.a4 <- xgboost(data = trains.T.a4,label = trains.a4$resa4,nrounds = n)
  bst.a5 <- xgboost(data = trains.T.a5,label = trains.a5$resa5,nrounds = n)
  bst.b1 <- xgboost(data = trains.T.b1,label = trains.b1$resb1,nrounds = n)
  bst.b2 <- xgboost(data = trains.T.b2,label = trains.b2$resb2,nrounds = n)
  
  result.a1<-trains.a1[-repeatedSplits.a1$Resample1,]
  result.a2<-trains.a2[-repeatedSplits.a2$Resample1,]
  result.a3<-trains.a3[-repeatedSplits.a3$Resample1,]
  result.a4<-trains.a4[-repeatedSplits.a4$Resample1,]
  result.a5<-trains.a5[-repeatedSplits.a5$Resample1,]
  result.b1<-trains.b1[-repeatedSplits.b1$Resample1,]
  result.b2<-trains.b2[-repeatedSplits.b2$Resample1,]
  
  result.T.a1<-Matrix(as.matrix(result.a1[,4:66]),sparse=T)
  result.T.a2<-Matrix(as.matrix(result.a2[,4:66]),sparse=T)
  result.T.a3<-Matrix(as.matrix(result.a3[,4:66]),sparse=T)
  result.T.a4<-Matrix(as.matrix(result.a4[,4:66]),sparse=T)
  result.T.a5<-Matrix(as.matrix(result.a5[,4:66]),sparse=T)
  result.T.b1<-Matrix(as.matrix(result.b1[,4:66]),sparse=T)
  result.T.b2<-Matrix(as.matrix(result.b2[,4:66]),sparse=T)
  
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
  
  
  rows<-length(result.a1)
  n.a<-0
  n.b<-0
  n.c<-0
  for (i in 1:rows) {
    temp.a<-0
    temp.b<-0
    temp.c<-0
    if (result.a1[i,]$resa1==a1.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (result.a2[i,]$resa2==a2.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (result.a3[i,]$resa3==a3.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (result.a4[i,]$resa4==a4.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (result.a5[i,]$resa5==a5.Predictions[i]) {
      temp.a<-temp.a+1
    }
    n.a<-c(n.a,temp.a)
    if (result.b1[i,]$resb1==b1.Predictions[i]) {
      temp.b<-temp.b+1
    }
    if (result.b2[i,]$resb2==b2.Predictions[i]) {
      temp.b<-temp.b+1
    }
    n.a<-c(n.a,temp.a)
    n.b<-c(n.b,temp.b)
    if (temp.a==5&temp.b==2) {
      temp.c<-1
    }
    else if(temp.a==5&temp.b==1) {
      temp.c<-2
    }
    else if (temp.a==5|(temp.a==4&temp.b==2)) {
      temp.c<-3
    }
    else if ((temp.a==4&temp.b==1)|(temp.a==3&temp.b==2)) {
      temp.c<-4
    }
    else if (temp.a==4|(temp.a==3&temp.b==1)|(temp.a==2&temp.b==2)) {
      temp.c<-5
    }
    else if (temp.a==3|(temp.a==1&temp.b==2)|(temp.a==2&temp.b==1)|temp.b==2) {
      temp.c<-6
    }
    n.c<-c(n.c,temp.c)
  }
  table(n.a)
  table(n.b)
  print(table(n.c))
  
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
  
  a1.4<-tests_1$a1
  a2.4<-tests_1$a2
  a3.4<-tests_1$a3
  a4.4<-tests_1$a4
  a5.4<-tests_1$a5
  
  a1.5<-tests_2$a1
  a2.5<-tests_2$a2
  a3.5<-tests_2$a3
  a4.5<-tests_2$a4
  a5.5<-tests_2$a5
  
  a1.6<-tests_3$a1
  a2.6<-tests_3$a2
  a3.6<-tests_3$a3
  a4.6<-tests_3$a4
  a5.6<-tests_3$a5
  
  a1.7<-tests_1$a1
  a2.7<-tests_1$a2
  a3.7<-tests_1$a3
  a4.7<-tests_1$a4
  a5.7<-tests_1$a5
  
  a1.8<-tests_2$a1
  a2.8<-tests_2$a2
  a3.8<-tests_2$a3
  a4.8<-tests_2$a4
  a5.8<-tests_2$a5
  
  a1.9<-tests_3$a1
  a2.9<-tests_3$a2
  a3.9<-tests_3$a3
  a4.9<-tests_3$a4
  a5.9<-tests_3$a5
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
  
  tests.ab<-data.frame(trn1,trn2,trn3,
                       a1.1,a2.1,a3.1,a4.1,a5.1,
                       a1.2,a2.2,a3.2,a4.2,a5.2,
                       a1.3,a2.3,a3.3,a4.3,a5.3,
                       a1.4,a2.4,a3.4,a4.4,a5.4,
                       a1.5,a2.5,a3.5,a4.5,a5.5,
                       a1.6,a2.6,a3.6,a4.6,a5.6,
                       a1.7,a2.7,a3.7,a4.7,a5.7,
                       a1.8,a2.8,a3.8,a4.8,a5.8,
                       a1.9,a2.9,a3.9,a4.9,a5.9,
                       b1.1,b2.1,
                       b1.2,b2.2,
                       b1.3,b2.3,
                       b1.4,b2.4,
                       b1.5,b2.5,
                       b1.6,b2.6,
                       b1.7,b2.7,
                       b1.8,b2.8,
                       b1.9,b2.9)
  
  tests.T.ab<-Matrix(as.matrix(tests.ab[,4:66]),sparse=T)
  
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
  
  c(
    tail(round(testPredictions.a1),1),
    tail(round(testPredictions.a2),1),
    tail(round(testPredictions.a3),1),
    tail(round(testPredictions.a4),1),
    tail(round(testPredictions.a5),1),
    tail(round(testPredictions.b1),1),
    tail(round(testPredictions.b2),1))
    
    
    
}








