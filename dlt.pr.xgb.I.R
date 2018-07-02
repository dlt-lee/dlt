data<-dlt
count<-dim(dlt)[1]
dlt.pr.xgb.I <- function(data,count,seed) {
  library(xgboost)
  library(caret)
  
  trains_1 <-tail(data,count)[1:(count-3),]
  trains_2 <-tail(data,count)[2:(count-2),]
  trains_3 <-tail(data,count)[3:(count-1),]
  results<-tail(data,(count-3))
  tests_1<-tail(data,count)[1:(count-2),]
  tests_2<-tail(data,count)[2:(count-1),]
  tests_3<-tail(data,(count-2))
  
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
  resb1<-results$b1
  resb2<-results$b2
  
  
  trains.a1<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
                        resa1)
  trains.a2<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
                        resa2)
  trains.a3<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
                        resa3)
  trains.a4<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
                        resa4)
  trains.a5<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
                        resa5)
  trains.b1<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
                        resb1)
  trains.b2<-data.frame(trn1,trn2,trn3,
                        #a1.1,a2.1,a3.1,a4.1,a5.1,
                        #a1.2,a2.2,a3.2,a4.2,a5.2,
                        a1.3,a2.3,a3.3,a4.3,a5.3,
                        #b1.1,b2.1,
                        #b1.2,b2.2,
                        b1.3,b2.3,
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
  
  train.T.a1<-Matrix(as.matrix(train.a1[,4:10]),sparse=T)
  train.T.a2<-Matrix(as.matrix(train.a2[,4:10]),sparse=T)
  train.T.a3<-Matrix(as.matrix(train.a3[,4:10]),sparse=T)
  train.T.a4<-Matrix(as.matrix(train.a4[,4:10]),sparse=T)
  train.T.a5<-Matrix(as.matrix(train.a5[,4:10]),sparse=T)
  train.T.b1<-Matrix(as.matrix(train.b1[,4:10]),sparse=T)
  train.T.b2<-Matrix(as.matrix(train.b2[,4:10]),sparse=T)
  
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
  
  result.T.a1<-Matrix(as.matrix(result.a1[,4:10]),sparse=T)
  result.T.a2<-Matrix(as.matrix(result.a2[,4:10]),sparse=T)
  result.T.a3<-Matrix(as.matrix(result.a3[,4:10]),sparse=T)
  result.T.a4<-Matrix(as.matrix(result.a4[,4:10]),sparse=T)
  result.T.a5<-Matrix(as.matrix(result.a5[,4:10]),sparse=T)
  result.T.b1<-Matrix(as.matrix(result.b1[,4:10]),sparse=T)
  result.T.b2<-Matrix(as.matrix(result.b2[,4:10]),sparse=T)
  
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
  #B:
  b1.1<-tests_1$b1
  b2.1<-tests_1$b2
  b1.2<-tests_2$b1
  b2.2<-tests_2$b2
  b1.3<-tests_3$b1
  b2.3<-tests_3$b2
  
  tests.ab<-data.frame(tsn1,tsn2,tsn3,
                       #a1.1,a2.1,a3.1,a4.1,a5.1,
                       #a1.2,a2.2,a3.2,a4.2,a5.2,
                       a1.3,a2.3,a3.3,a4.3,a5.3,
                       #b1.1,b2.1,
                       #b1.2,b2.2,
                       b1.3,b2.3)
  
  tests.T.ab<-Matrix(as.matrix(tests.ab[,4:10]),sparse=T)
  
  testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.ab)
  testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.ab)
  testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.ab)
  testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.ab)
  testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.ab)
  testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.ab)
  testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.ab)
  
 
  
  print(c(mean(result.a1$resa1-a1.Predictions),
          mean(result.a2$resa2-a2.Predictions),
          mean(result.a3$resa3-a3.Predictions),
          mean(result.a4$resa4-a4.Predictions),
          mean(result.a5$resa5-a5.Predictions),
          mean(result.b1$resb1-b1.Predictions),
          mean(result.b2$resb2-b2.Predictions)))
  
  return(c(
    tail(round(testPredictions.a1)+as.integer(names(sort(-table(result.a1$resa1-a1.Predictions))[1])),1),
    tail(round(testPredictions.a2)+as.integer(names(sort(-table(result.a2$resa2-a2.Predictions))[1])),1),
    tail(round(testPredictions.a3)+as.integer(names(sort(-table(result.a3$resa3-a3.Predictions))[1])),1),
    tail(round(testPredictions.a4)+as.integer(names(sort(-table(result.a4$resa4-a4.Predictions))[1])),1),
    tail(round(testPredictions.a5)+as.integer(names(sort(-table(result.a5$resa5-a5.Predictions))[1])),1),
    tail(round(testPredictions.b1)+as.integer(names(sort(-table(result.b1$resb1-b1.Predictions))[1])),1),
    tail(round(testPredictions.b2)+as.integer(names(sort(-table(result.b2$resb2-b2.Predictions))[1])),1)
  ))
  
}









