dlt.KNNFII <- function(data,count) {
  library(caret)
  trains_1 <-tail(data,count)[1:(count-2),]
  trains_2 <-tail(data,count)[2:(count-1),]
  results<-tail(data,(count-2))
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
  trainY.a1<-results$a1
  trainY.a2<-results$a2
  trainY.a3<-results$a3
  trainY.a4<-results$a4
  trainY.a5<-results$a5
  trains.a1<-data.frame(trn1,trn2,a1.2,a1.1,trainY.a1)
  trains.a2<-data.frame(trn1,trn2,a2.2,a2.1,trainY.a2)
  trains.a3<-data.frame(trn1,trn2,a3.2,a3.1,trainY.a3)
  trains.a4<-data.frame(trn1,trn2,a4.2,a4.1,trainY.a4)
  trains.a5<-data.frame(trn1,trn2,a5.2,a5.1,trainY.a5)
  #B:
  b1.1<-trains_1$b1
  b2.1<-trains_1$b2
  b1.2<-trains_2$b1
  b2.2<-trains_2$b2
  trainY.b1<-results$b1
  trainY.b2<-results$b2
  trains.b1<-data.frame(trn1,trn2,b1.2,b1.1,trainY.b1)
  trains.b2<-data.frame(trn1,trn2,b2.2,b2.1,trainY.b2)
  #build KNN model
  fit_knn.a11 <- knnreg(trains.a1[3:4], trains.a1$trainY.a1, k = 5)
  fit_knn.a22 <- knnreg(trains.a2[3:4], trains.a2$trainY.a2, k = 5)
  fit_knn.a33 <- knnreg(trains.a3[3:4], trains.a3$trainY.a3, k = 5)
  fit_knn.a44 <- knnreg(trains.a4[3:4], trains.a4$trainY.a4, k = 5)
  fit_knn.a55 <- knnreg(trains.a5[3:4], trains.a5$trainY.a5, k = 5)
  fit_knn.b11 <- knnreg(trains.b1[3:4], trains.b1$trainY.b1, k = 5)
  fit_knn.b22 <- knnreg(trains.b1[3:4], trains.b1$trainY.b1, k = 5)
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
  b2.1<-tests_1$a2
  b1.2<-tests_2$b1
  b2.2<-tests_2$b2
  tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
  tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
  #KNN predict test suit
  p.a11 <- predict(fit_knn.a11, tests.a11[3:4])
  p.a22 <- predict(fit_knn.a22, tests.a22[3:4])
  p.a33 <- predict(fit_knn.a33, tests.a33[3:4])
  p.a44 <- predict(fit_knn.a44, tests.a44[3:4])
  p.a55 <- predict(fit_knn.a55, tests.a55[3:4])
  p.b11 <- predict(fit_knn.b11, tests.b11[3:4])
  p.b22 <- predict(fit_knn.b22, tests.b22[3:4])
  
  
  result<-c(p.a11,p.a22,p.a33,p.a44,p.a55,p.b11,p.b22)
  barplot(result,main = "KNNFII")
  return(result)
  
  
  
}


