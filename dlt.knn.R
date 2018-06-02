library(caret)
#Build data
trains <-tail(dlt,251)[1:250,]
results<-tail(dlt,250)
tests<-tail(dlt,1)
trainX.a<-trains[,4:8]
trainX.b<-trains[,9:10]
trainY.a1<-results$a1
trainY.a2<-results$a2
trainY.a3<-results$a3
trainY.a4<-results$a4
trainY.a5<-results$a5
trainY.b1<-results$b1
trainY.b2<-results$b2
testX.a<-tests[,4:8]
testX.b<-tests[,9:10]

#build KNN model
fit_knn.a1 <- knnreg(trainX.a, trainY.a1, k = 10)
fit_knn.a2 <- knnreg(trainX.a, trainY.a2, k = 10)
fit_knn.a3 <- knnreg(trainX.a, trainY.a3, k = 10)
fit_knn.a4 <- knnreg(trainX.a, trainY.a4, k = 10)
fit_knn.a5 <- knnreg(trainX.a, trainY.a5, k = 10)
fit_knn.b1 <- knnreg(trainX.b, trainY.b1, k = 10)
fit_knn.b2 <- knnreg(trainX.b, trainY.b2, k = 10)
#KNNpredict test suit
p.a1 <- predict(fit_knn.a1, testX.a)
p.a2 <- predict(fit_knn.a2, testX.a)
p.a3 <- predict(fit_knn.a3, testX.a)
p.a4 <- predict(fit_knn.a4, testX.a)
p.a5 <- predict(fit_knn.a5, testX.a)
p.b1 <- predict(fit_knn.b1, testX.b)
p.b2 <- predict(fit_knn.b2, testX.b)


result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
result
plot(result)

