data<-dlt
count<-dim(dlt)[1]
#dlt.caret_knn <- function(data,count) {
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
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa1)
trains.a2<-data.frame(trn1,trn2,trn3,
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa2)
trains.a3<-data.frame(trn1,trn2,trn3,
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa3)
trains.a4<-data.frame(trn1,trn2,trn3,
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa4)
trains.a5<-data.frame(trn1,trn2,trn3,
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa5)
trains.b1<-data.frame(trn1,trn2,trn3,
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resb1)
trains.b2<-data.frame(trn1,trn2,trn3,
                      a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resb2)
solTrainXrans<-trains.a1[,4:25]
#knnDescr<-solTrainXrans[,-nearZeroVar(solTrainXrans)]
set.seed(100)
n.t<-15
n.r<-5
n.n<-10
knnTune.a1<-train(resa1~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.a1,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.a1,main="knn.a1"))
knnTune.a2<-train(resa2~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.a2,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.a2,main="knn.a2"))
knnTune.a3<-train(resa3~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.a3,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.a3,main="knn.a3"))
knnTune.a4<-train(resa4~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.a4,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.a4,main="knn.a4"))
knnTune.a5<-train(resa5~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.a5,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.a5,main="knn.a5"))
knnTune.b1<-train(resb1~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.b1,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.b1,main="knn.b1"))
knnTune.b2<-train(resb2~
                    a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    b1.1+b2.1+b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.b2,
                  method="knn",
                  PreProc=c("BoxCox","center","scale","pca"),
                  tuneGrid=data.frame(.k=1:25),
                  trControl=trainControl(method = "repeatedcv",
                                         repeats = n.r,
                                         number = n.n))
print(plot(knnTune.b2,main="knn.b2"))


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
b2.1<-tests_1$a2
b1.2<-tests_2$b1
b2.2<-tests_2$b2
b1.3<-tests_3$b1
b2.3<-tests_3$b2

tests.ab<-data.frame(tsn1,tsn2,tsn3,
                     a1.1,a2.1,a3.1,a4.1,a5.1,
                     a1.2,a2.2,a3.2,a4.2,a5.2,
                     a1.3,a2.3,a3.3,a4.3,a5.3,
                     b1.1,b2.1,
                     b1.2,b2.2,
                     b1.3,b2.3)
test.ab<-preProcess(tests.ab,method = c("BoxCox","center","scale","pca"))

testPredictions.a1<-predict(knnTune.a1,tests.ab)
testPredictions.a2<-predict(knnTune.a2,tests.ab)
testPredictions.a3<-predict(knnTune.a3,tests.ab)
testPredictions.a4<-predict(knnTune.a4,tests.ab)
testPredictions.a5<-predict(knnTune.a5,tests.ab)
testPredictions.b1<-predict(knnTune.b1,tests.ab)
testPredictions.b2<-predict(knnTune.b2,tests.ab)

print(c(tail(testPredictions.a1,1),
        tail(testPredictions.a2,1),
        tail(testPredictions.a3,1),  
        tail(testPredictions.a4,1),
        tail(testPredictions.a5,1),
        tail(testPredictions.b1,1),
        tail(testPredictions.b2,1)))

a1<-testPredictions.a1
a2<-testPredictions.a2
a3<-testPredictions.a3
a4<-testPredictions.a4
a5<-testPredictions.a5
b1<-testPredictions.b1
b2<-testPredictions.b2

result.data<-data.frame(a1,a2,a3,a4,a5,b1,b2)
result.model<-list(knnTune.a1,
                   knnTune.a2,
                   knnTune.a3,
                   knnTune.a4,
                   knnTune.a5,
                   knnTune.b1,
                   knnTune.b2)
return(list(knnTune.a1,
            knnTune.a2,
            knnTune.a3,
            knnTune.a4,
            knnTune.a5,
            knnTune.b1,
            knnTune.b2,
            testPredictions.a1,
            testPredictions.a2,
            testPredictions.a3,
            testPredictions.a4,
            testPredictions.a5,
            testPredictions.b1,
            testPredictions.b2))

#}



