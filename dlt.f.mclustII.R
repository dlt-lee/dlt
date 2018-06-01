library(mclust)
dlt.f.mclustII<-function(mytable) {
  #Build data
  trains_1 <-tail(mytable,202)[1:200,]
  trains_2 <-tail(mytable,202)[2:201,]
  results<-tail(mytable,200)
  tests_1<-tail(mytable,2)[1,]
  tests_2<-tail(mytable,2)[2,]
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
  trains.a11<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
  trains.a22<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
  trains.a33<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
  trains.a44<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
  trains.a55<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
  #B:
  b1.1<-trains_1$b1
  b2.1<-trains_1$b2
  b1.2<-trains_2$b1
  b2.2<-trains_2$b2
  resb1<-results$b1
  resb2<-results$b2
  trains.b11<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
  trains.b22<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
  #Build gMclust model
  fit_mc.a11<-Mclust(trains.a11[3:4],Gparallel = TRUE)
  fit_mc.a22<-Mclust(trains.a22[3:4],parallel = TRUE)
  fit_mc.a33<-Mclust(trains.a33[3:4],parallel = TRUE)
  fit_mc.a44<-Mclust(trains.a44[3:4],parallel = TRUE)
  fit_mc.a55<-Mclust(trains.a55[3:4],parallel = TRUE)
  fit_mc.b11<-Mclust(trains.b11[3:4],parallel = TRUE)
  fit_mc.b22<-Mclust(trains.b22[3:4],parallel = TRUE)
  #summary(fit_mc.a1,parametwrs = TRUE)
  #Build test data
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
  #Mclust testing suit
  p.a11<-predict(fit_mc.a11, tests.a11[3:4])
  p.a22<-predict(fit_mc.a22, tests.a22[3:4])
  p.a33<-predict(fit_mc.a33, tests.a33[3:4])
  p.a44<-predict(fit_mc.a44, tests.a44[3:4])
  p.a55<-predict(fit_mc.a55, tests.a55[3:4])
  p.b11<-predict(fit_mc.b11, tests.b11[3:4])
  p.b22<-predict(fit_mc.b22, tests.b22[3:4])
  p.a11<-table(fit_mc.a11$classification,trains.a11$resa1)[p.a11$classification,]
  barplot(p.a11,main = "a11")
  p.a22<-table(fit_mc.a22$classification,trains.a22$resa2)[p.a22$classification,]
  barplot(p.a22,main = "a22")
  p.a33<-table(fit_mc.a33$classification,trains.a33$resa3)[p.a33$classification,]
  barplot(p.a33,main = "a33")
  p.a44<-table(fit_mc.a44$classification,trains.a44$resa4)[p.a44$classification,]
  barplot(p.a44,main = "a44")
  p.a55<-table(fit_mc.a55$classification,trains.a55$resa5)[p.a55$classification,]
  barplot(p.a55,main = "a55")
  p.b11<-table(fit_mc.b11$classification,trains.b11$resb1)[p.b11$classification,]
  barplot(p.b11,main = "b11")
  p.b22<-table(fit_mc.b22$classification,trains.b22$resb2)[p.b22$classification,]
  barplot(p.b22,main = "b22")
  
  sort(p.a11,decreasing = TRUE)
  nm.a11<-as.data.frame.integer(names(p.a11))[,1]
  p.a11<-as.data.frame(p.a11)$p.a11
  p.a11<-data.frame(nm.a11,p.a11)
  
  sort(p.a22,decreasing = TRUE)
  nm.a22<-as.data.frame.integer(names(p.a22))[,1]
  p.a22<-as.data.frame(p.a22)$p.a22
  p.a22<-data.frame(nm.a22,p.a22)
  
  sort(p.a33,decreasing = TRUE)
  nm.a33<-as.data.frame.integer(names(p.a33))[,1]
  p.a33<-as.data.frame(p.a33)$p.a33
  p.a33<-data.frame(nm.a33,p.a33)
  
  sort(p.a44,decreasing = TRUE)
  nm.a44<-as.data.frame.integer(names(p.a44))[,1]
  p.a44<-as.data.frame(p.a44)$p.a44
  p.a44<-data.frame(nm.a44,p.a44)
  
  sort(p.a55,decreasing = TRUE)
  nm.a55<-as.data.frame.integer(names(p.a55))[,1]
  p.a55<-as.data.frame(p.a55)$p.a55
  p.a55<-data.frame(nm.a55,p.a55)
  
  sort(p.b11,decreasing = TRUE)
  nm.b11<-as.data.frame.integer(names(p.b11))[,1]
  p.b11<-as.data.frame(p.b11)$p.b11
  p.b11<-data.frame(nm.b11,p.b11)
  
  sort(p.b22,decreasing = TRUE)
  nm.b22<-as.data.frame.integer(names(p.b22))[,1]
  p.b22<-as.data.frame(p.b22)$p.b22
  p.b22<-data.frame(nm.b22,p.b22)
  
  p<-c(p.a11,p.a22,p.a33,p.a44,p.a55,p.b11,p.b22)
  return(p)
  
  
}



