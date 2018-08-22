data<-dlt
count<-dim(dlt)[1]

#dlt.class.rf <- function(data,count) {
  library(randomForest)
  
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
  resa1<-dlt.d2c(results$a1,1)
  resa2<-dlt.d2c(results$a2,1)
  resa3<-dlt.d2c(results$a3,1)
  resa4<-dlt.d2c(results$a4,1)
  resa5<-dlt.d2c(results$a5,1)
  #B:
  b1.1<-trains_1$b1
  b2.1<-trains_1$b2
  b1.2<-trains_2$b1
  b2.2<-trains_2$b2
  b1.3<-trains_3$b1
  b2.3<-trains_3$b2
  resb1<-dlt.d2c(results$b1,2)
  resb2<-dlt.d2c(results$b2,2)
  
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
  
  rf.model.a1<-randomForest(resa1~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.a1,
                            ntree = 2000)
  rf.model.a2<-randomForest(resa2~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.a2,
                            ntree = 2000)
  
  rf.model.a3<-randomForest(resa3~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.a3,
                            ntree = 2000)
  
  rf.model.a4<-randomForest(resa4~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.a4,
                            ntree = 2000)
  
  rf.model.a5<-randomForest(resa5~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.a5,
                            ntree = 2000)
  
  rf.model.b1<-randomForest(resb1~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.b1,
                            ntree = 2000)
  
  rf.model.b2<-randomForest(resb2~
                              a1.1+a2.1+a3.1+a4.1+a5.1+
                              a1.2+a2.2+a3.2+a4.2+a5.2+
                              a1.3+a2.3+a3.3+a4.3+a5.3+
                              b1.1+b2.1+
                              b1.2+b2.2+
                              b1.3+b2.3,
                            data = trains.b2,
                            ntree = 2000)
  
  
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
                       a1.1,a2.1,a3.1,a4.1,a5.1,
                       a1.2,a2.2,a3.2,a4.2,a5.2,
                       a1.3,a2.3,a3.3,a4.3,a5.3,
                       b1.1,b2.1,
                       b1.2,b2.2,
                       b1.3,b2.3)
  
  rfTestPred.a1<-predict(rf.model.a1,tests.ab,type = "prob")
  rfTestPred.a2<-predict(rf.model.a2,tests.ab,type = "prob")
  rfTestPred.a3<-predict(rf.model.a3,tests.ab,type = "prob")
  rfTestPred.a4<-predict(rf.model.a4,tests.ab,type = "prob")
  rfTestPred.a5<-predict(rf.model.a5,tests.ab,type = "prob")
  rfTestPred.b1<-predict(rf.model.b1,tests.ab,type = "prob")
  rfTestPred.b2<-predict(rf.model.b2,tests.ab,type = "prob")
  
  #tests.ab$RFprob.a101<-rfTestPred.a1[,"01"]
  #tests.ab$RFprob.a102<-rfTestPred.a1[,"02"]
  #tests.ab$RFprob.a103<-rfTestPred.a1[,"03"]
  #tests.ab$RFprob.a104<-rfTestPred.a1[,"04"]
  #tests.ab$RFprob.a105<-rfTestPred.a1[,"05"]
  #tests.ab$RFprob.a106<-rfTestPred.a1[,"06"]
  #tests.ab$RFprob.a107<-rfTestPred.a1[,"07"]
  #tests.ab$RFprob.a108<-rfTestPred.a1[,"08"]
  #tests.ab$RFprob.a109<-rfTestPred.a1[,"09"]
  #tests.ab$RFprob.a110<-rfTestPred.a1[,"10"]
  #tests.ab$RFprob.a111<-rfTestPred.a1[,"11"]
  #tests.ab$RFprob.a112<-rfTestPred.a1[,"12"]
  #tests.ab$RFprob.a113<-rfTestPred.a1[,"13"]
  #tests.ab$RFprob.a114<-rfTestPred.a1[,"14"]
  #tests.ab$RFprob.a115<-rfTestPred.a1[,"15"]
  #tests.ab$RFprob.a116<-rfTestPred.a1[,"16"]
  #tests.ab$RFprob.a117<-rfTestPred.a1[,"17"]
  #tests.ab$RFprob.a118<-rfTestPred.a1[,"18"]
  #tests.ab$RFprob.a119<-rfTestPred.a1[,"19"]
  #tests.ab$RFprob.a120<-rfTestPred.a1[,"20"]
  #tests.ab$RFprob.a121<-rfTestPred.a1[,"21"]
  #tests.ab$RFprob.a122<-rfTestPred.a1[,"22"]
  #tests.ab$RFprob.a123<-rfTestPred.a1[,"23"]
  #tests.ab$RFprob.a124<-rfTestPred.a1[,"24"]
  
  #tests.ab$RFclass.a1<-predict(rf.model.a1,tests.ab)
  #tests.ab$RFclass.a2<-predict(rf.model.a2,tests.ab)
  #tests.ab$RFclass.a3<-predict(rf.model.a3,tests.ab)
  #tests.ab$RFclass.a4<-predict(rf.model.a4,tests.ab)
  #tests.ab$RFclass.a5<-predict(rf.model.a5,tests.ab)
  #tests.ab$RFclass.b1<-predict(rf.model.b1,tests.ab)
  #tests.ab$RFclass.b2<-predict(rf.model.b2,tests.ab)
  
  barplot(tail(rfTestPred.a1,1),main = "a1")
  barplot(tail(rfTestPred.a2,1),main = "a2")
  barplot(tail(rfTestPred.a3,1),main = "a3")
  barplot(tail(rfTestPred.a4,1),main = "a4")
  barplot(tail(rfTestPred.a5,1),main = "a5")
  barplot(tail(rfTestPred.b1,1),main = "b1")
  barplot(tail(rfTestPred.b2,1),main = "b2")
  
  dn.a1<-names(tail(rfTestPred.a1,1)[1,])
  pn.a1<-tail(rfTestPred.a1,1)[1,]
  r.a1<-data.frame(dn.a1,pn.a1)
  
  dn.a2<-names(tail(rfTestPred.a2,1)[1,])
  pn.a2<-tail(rfTestPred.a2,1)[1,]
  r.a2<-data.frame(dn.a2,pn.a2)
  
  dn.a3<-names(tail(rfTestPred.a3,1)[1,])
  pn.a3<-tail(rfTestPred.a3,1)[1,]
  r.a3<-data.frame(dn.a3,pn.a3)
  
  dn.a4<-names(tail(rfTestPred.a4,1)[1,])
  pn.a4<-tail(rfTestPred.a4,1)[1,]
  r.a4<-data.frame(dn.a4,pn.a4)
  
  dn.a5<-names(tail(rfTestPred.a5,1)[1,])
  pn.a5<-tail(rfTestPred.a5,1)[1,]
  r.a5<-data.frame(dn.a5,pn.a5)
  
  dn.b1<-names(tail(rfTestPred.b1,1)[1,])
  pn.b1<-tail(rfTestPred.b1,1)[1,]
  r.b1<-data.frame(dn.b1,pn.b1)
  
  dn.b2<-names(tail(rfTestPred.b2,1)[1,])
  pn.b2<-tail(rfTestPred.b2,1)[1,]
  r.b2<-data.frame(dn.b2,pn.b2)
  
  c(
    r.a1[which(dn.a1=="13"),2],
    r.a2[which(dn.a2=="14"),2],
    r.a3[which(dn.a3=="20"),2],
    r.a4[which(dn.a4=="28"),2],
    r.a5[which(dn.a5=="34"),2],
    r.b1[which(dn.b1=="06"),2],
    r.b2[which(dn.b2=="10"),2]
  )
  
  
  tail(rfTestPred.a1,1)
  tail(rfTestPred.a2,1)
  tail(rfTestPred.a3,1)
  tail(rfTestPred.a4,1)
  tail(rfTestPred.a5,1)
  tail(rfTestPred.b1,1)
  tail(rfTestPred.b2,1)
#}



