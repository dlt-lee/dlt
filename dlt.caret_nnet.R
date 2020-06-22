data<-dlt
count<-dim(dlt)[1]
#dlt.caret_nnet <- function(data,count) {
  library(nnet)
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
  
  #tooHigh<-findCorrelation(cor(trains.a1[,4:24]),cutoff = 0.75)
  #trains.a1<-trains.a1[,-tooHigh]
  #trains.a1.pre<-preProcess(trains.a1[],method = c("BoxCox","center","scale","pca"))
  
  n.t<-15
  n.r<-5
  n.n<-10
  ctrl<-trainControl(method = "repeatedcv",repeats = n.r,number = n.n)
  #set.seed(100)
  #cvsplites.a1<-createFolds(trains.a1,,k = 10,returnTrain = TRUE)
  nnetGrid<-expand.grid(.decay=c(0,0.01,.1),
                        .size=seq(1,27,by = 2),
                        .bag=FALSE)
  date()
  set.seed(100)
  avnnetTune.a1<-train(resa1~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.a1,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.a1[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.a1,main="nnet.a1")
  date()
  avnnetTune.a2<-train(resa2~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.a2,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.a2[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.a2,main="nnet.a2")
  date()
  avnnetTune.a3<-train(resa3~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.a3,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.a3[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.a3,main="nnet.a3")
  date()
  avnnetTune.a4<-train(resa4~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.a4,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.a4[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.a4,main="nnet.a4")
  date()
  avnnetTune.a5<-train(resa5~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.a5,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.a5[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.a5,main="nnet.a5")
  date()
  avnnetTune.b1<-train(resb1~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.b1,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.b1[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.b1,main="nnet.b1")
  date()
  avnnetTune.b2<-train(resb2~a1.1+a2.1+a3.1+a4.1+a5.1+
                         a1.2+a2.2+a3.2+a4.2+a5.2+
                         a1.3+a2.3+a3.3+a4.3+a5.3+
                         b1.1+b2.1+b2.2+b2.2+b2.3+b2.3,
                       data = trains.b2,
                       method="avNNet",
                       tuneGrid=nnetGrid,
                       trControl=ctrl,
                       PreProc=c("BoxCox","center","scale","pca"),
                       linout=TRUE,
                       trace=FALSE,
                       MAXNWts=10*(ncol(trains.b2[,4:24])+1)+10+1,
                       maxit=500)
  plot(avnnetTune.b2,main="nnet.b2")
  date()
  
  
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
  
  testPredictions.a1<-predict(avnnetTune.a1,tests.ab)
  testPredictions.a2<-predict(avnnetTune.a2,tests.ab)
  testPredictions.a3<-predict(avnnetTune.a3,tests.ab)
  testPredictions.a4<-predict(avnnetTune.a4,tests.ab)
  testPredictions.a5<-predict(avnnetTune.a5,tests.ab)
  testPredictions.b1<-predict(avnnetTune.b1,tests.ab)
  testPredictions.b2<-predict(avnnetTune.b2,tests.ab)
  
  date()
  #################################################################
  #verification
  dlt.p.table(dlt,
              ceiling(testPredictions.a1),ceiling(testPredictions.a2),
              ceiling(testPredictions.a3),ceiling(testPredictions.a4),
              ceiling(testPredictions.a5),
              ceiling(testPredictions.b1),ceiling(testPredictions.b2)
  )
  dlt.p.table(dlt,
              floor(testPredictions.a1),floor(testPredictions.a2),
              floor(testPredictions.a3),floor(testPredictions.a4),
              floor(testPredictions.a5),
              floor(testPredictions.b1),floor(testPredictions.b2))
  dlt.p.table(dlt,
              trunc(testPredictions.a1),trunc(testPredictions.a2),
              trunc(testPredictions.a3),trunc(testPredictions.a4),
              trunc(testPredictions.a5),
              trunc(testPredictions.b1),trunc(testPredictions.b2))
  dlt.p.table(dlt,
              round(testPredictions.a1),round(testPredictions.a2),
              round(testPredictions.a3),round(testPredictions.a4),
              round(testPredictions.a5),
              round(testPredictions.b1),round(testPredictions.b2))
  ################################################################
  
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
  result.model<-c(avnnetTune.a1,
                  avnnetTune.a2,
                  avnnetTune.a3,
                  avnnetTune.a4,
                  avnnetTune.a5,
                  avnnetTune.b1,
                  avnnetTune.b2)
  return(list(avnnetTune.a1,
              avnnetTune.a2,
              avnnetTune.a3,
              avnnetTune.a4,
              avnnetTune.a5,
              avnnetTune.b1,
              avnnetTune.b2,
              testPredictions.a1,
              testPredictions.a2,
              testPredictions.a3,
              testPredictions.a4,
              testPredictions.a5,
              testPredictions.b1,
              testPredictions.b2))
                       
                       
                       
                       
                       
#}

