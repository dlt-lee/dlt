dlt.DRF <- function(data,count) {
  library(randomForest)
  
  trains <-tail(data,(count))[1:(count-1),]
  results<-tail(data,(count-1))
  tests<-tail(data,1)
  #A:
  testset.a<-tests[,4:8]
  #A1:
  r.a1<-results[,4]
  trainset.a1<-cbind(trains[,4:8],r.a1)
  rf.a1 = randomForest(r.a1 ~ a1+a2+a3+a4+a5,data = trainset.a1,importance = T,ntrees=1000)
  p.a1 = predict(rf.a1,testset.a)
  #A2:
  r.a2<-results[,5]
  trainset.a2<-cbind(trains[,4:8],r.a2)
  rf.a2 = randomForest(r.a2 ~ a1+a2+a3+a4+a5,data = trainset.a2,importance = T,ntrees=1000)
  p.a2 = predict(rf.a2,testset.a)
  #A3:
  r.a3<-results[,6]
  trainset.a3<-cbind(trains[,4:8],r.a3)
  rf.a3 = randomForest(r.a3 ~ a1+a2+a3+a4+a5,data = trainset.a3,importance = T,ntrees=1000)
  p.a3 = predict(rf.a3,testset.a)
  #A4:
  r.a4<-results[,7]
  trainset.a4<-cbind(trains[,4:8],r.a4)
  rf.a4 = randomForest(r.a4 ~ a1+a2+a3+a4+a5,data = trainset.a4,importance = T,ntrees=1000)
  p.a4 = predict(rf.a4,testset.a)
  #A5:
  r.a5<-results[,8]
  trainset.a5<-cbind(trains[,4:8],r.a5)
  rf.a5 = randomForest(r.a5 ~ a1+a2+a3+a4+a5,data = trainset.a5,importance = T,ntrees=1000)
  p.a5 = predict(rf.a5,testset.a)
  
  
  trains.b <-tail(data,count)[1:(count-1),]
  results.b<-tail(data,(count-1))
  tests<-tail(dlt,1)
  #B:
  testset.b<-tests[,9:10]
  #B1:
  r.b1<-results.b[,9]
  trainset.b1<-cbind(trains.b[,9:10],r.b1)
  rf.b1 = randomForest(r.b1 ~ b1+b2,data = trainset.b1,importance = T,ntrees=1000)
  p.b1 = predict(rf.b1,testset.b)
  #B2:
  r.b2<-results.b[,10]
  trainset.b2<-cbind(trains.b[,9:10],r.b2)
  rf.b2 = randomForest(r.b2 ~ b1+b2,data = trainset.b2,importance = T,ntrees=1000)
  p.b2 = predict(rf.b2,testset.b)
  
  result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
  barplot(result,main = "DRF")
  return(result)
  
  
}


