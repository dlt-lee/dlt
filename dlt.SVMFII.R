dlt.SVMFII <- function(data,count) {
  library(e1071)
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
  #build SVM model
  fit_svm.a11=svm(resa1~a1.2+a1.1,data = trains.a1)
  fit_svm.a22=svm(resa2~a2.2+a2.1,data = trains.a2)
  fit_svm.a33=svm(resa3~a3.2+a3.1,data = trains.a3)
  fit_svm.a44=svm(resa4~a4.2+a4.1,data = trains.a4)
  fit_svm.a55=svm(resa5~a5.2+a5.1,data = trains.a5)
  fit_svm.b11=svm(resb1~b1.2+b1.1,data = trains.b1)
  fit_svm.b22=svm(resb2~b2.2+b2.1,data = trains.b2)
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
  #SVM predict test suit
  p.a11 <- predict(object = fit_svm.a11,newdata = tests.a11[3:4])
  p.a22 <- predict(object = fit_svm.a22,newdata = tests.a22[3:4])
  p.a33 <- predict(object = fit_svm.a33,newdata = tests.a33[3:4])
  p.a44 <- predict(object = fit_svm.a44,newdata = tests.a44[3:4])
  p.a55 <- predict(object = fit_svm.a55,newdata = tests.a55[3:4])
  p.b11 <- predict(object = fit_svm.b11,newdata = tests.b11[3:4])
  p.b22 <- predict(object = fit_svm.b22,newdata = tests.b22[3:4])
  
  
  result<-c(p.a11,p.a22,p.a33,p.a44,p.a55,p.b11,p.b22)
  barplot(result,main = "SVMFII")
  return(result)
  
  
  
}



