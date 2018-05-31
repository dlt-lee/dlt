dlt.DRFII <- function(data,count) {
  library(randomForest)
  trains_1 <-tail(data,count)[1:(count-2),]
  trains_2 <-tail(data,count)[2:(count-1),]
  results<-tail(data,count)[3:count,]
  tests_1<-tail(data,2)[1,]
  tests_2<-tail(data,2)[2,]
  #A:
  trn1<-trains_1$n
  trn2<-trains_2$n
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
  resa1<-results$a1
  resa2<-results$a2
  resa3<-results$a3
  resa4<-results$a4
  resa5<-results$a5
  trains.a1<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
  trains.a2<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
  trains.a3<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
  trains.a4<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
  trains.a5<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
  #B:
  b1.1<-trains_1$b1
  b2.1<-trains_1$b2
  b1.2<-trains_2$b1
  b2.2<-trains_2$b2
  resb1<-results$b1
  resb2<-results$b2
  trains.b1<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
  trains.b2<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
  #Build DRForest model
  fit_drf.a11 = randomForest(resa1 ~ a1.2+a1.1,data = trains.a1,importance = T,ntrees=1000)
  fit_drf.a22 = randomForest(resa2 ~ a2.2+a2.1,data = trains.a2,importance = T,ntrees=1000)
  fit_drf.a33 = randomForest(resa3 ~ a3.2+a3.1,data = trains.a3,importance = T,ntrees=1000)
  fit_drf.a44 = randomForest(resa4 ~ a4.2+a4.1,data = trains.a4,importance = T,ntrees=1000)
  fit_drf.a55 = randomForest(resa5 ~ a5.2+a5.1,data = trains.a5,importance = T,ntrees=1000)
  fit_drf.b11 = randomForest(resb1 ~ b1.2+b1.1,data = trains.b1,importance = T,ntrees=1000)
  fit_drf.b22 = randomForest(resb2 ~ b2.2+b1.1,data = trains.b1,importance = T,ntrees=1000)
  #Buil test data
  #A:
  tsn1<-tests_1$n
  tsn2<-tests_2$n
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
  tests.a11<-data.frame(tsn1,tsn2,a1.2,a1.1)
  tests.a22<-data.frame(tsn1,tsn2,a2.2,a2.1)
  tests.a33<-data.frame(tsn1,tsn2,a3.2,a3.1)
  tests.a44<-data.frame(tsn1,tsn2,a4.2,a4.1)
  tests.a55<-data.frame(tsn1,tsn2,a5.2,a5.1)
  #B:
  b1.1<-tests_1$b1
  b2.1<-tests_1$b2
  b1.2<-tests_2$b1
  b2.2<-tests_2$b2
  tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
  tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
  #randomForest test suit
  p.a11 = predict(fit_drf.a11,tests.a11)
  p.a22 = predict(fit_drf.a22,tests.a22)
  p.a33 = predict(fit_drf.a33,tests.a33)
  p.a44 = predict(fit_drf.a44,tests.a44)
  p.a55 = predict(fit_drf.a55,tests.a55)
  p.b11 = predict(fit_drf.b11,tests.b11)
  p.b22 = predict(fit_drf.b22,tests.b22)
  
  result<-c(p.a11,p.a22,p.a33,p.a44,p.a55,p.b11,p.b22)
  barplot(result,main = "DRFII")
  return(result)
  
  
}


