dlt.SVMF <- function(data,count) {
  library(e1071)
  #Build data
  tests<-tail(data,1)
  
  trains.a <-tail(data,count)[1:(count-1),]
  results.a<-tail(data,(count-1))
  
  trn<-trains.a$n
  a1<-trains.a$a1
  a2<-trains.a$a2
  a3<-trains.a$a3
  a4<-trains.a$a4
  a5<-trains.a$a5
  
  resa1<-results.a$a1
  resa2<-results.a$a2
  resa3<-results.a$a3
  resa4<-results.a$a4
  resa5<-results.a$a5
  
  trains.a1<-data.frame(a1,a2,a3,a4,a5,resa1)
  trains.a2<-data.frame(a1,a2,a3,a4,a5,resa2)
  trains.a3<-data.frame(a1,a2,a3,a4,a5,resa3)
  trains.a4<-data.frame(a1,a2,a3,a4,a5,resa4)
  trains.a5<-data.frame(a1,a2,a3,a4,a5,resa5)
  
  trains.b<-tail(data,count)[1:(count-1),]
  results.b<-tail(data,(count-1))
  
  b1<-trains.b$b1
  b2<-trains.b$b2
  
  resb1<-results.b$b1
  resb2<-results.b$b2
  
  trains.b1<-data.frame(b1,b2,resb1)
  trains.b2<-data.frame(b1,b2,resb2)
  
  fit_svm.a1=svm(resa1~a1+a2+a3+a4+a5,data = trains.a1)
  fit_svm.a2=svm(resa2~a1+a2+a3+a4+a5,data = trains.a2)
  fit_svm.a3=svm(resa3~a1+a2+a3+a4+a5,data = trains.a3)
  fit_svm.a4=svm(resa4~a1+a2+a3+a4+a5,data = trains.a4)
  fit_svm.a5=svm(resa5~a1+a2+a3+a4+a5,data = trains.a5)
  fit_svm.b1=svm(resb1~b1+b2,data = trains.b1)
  fit_svm.b2=svm(resb2~b1+b2,data = trains.b2)
  p.a1 <- predict(object = fit_svm.a1,newdata = tests[4:8])
  p.a2 <- predict(object = fit_svm.a2,newdata = tests[4:8])
  p.a3 <- predict(object = fit_svm.a3,newdata = tests[4:8])
  p.a4 <- predict(object = fit_svm.a4,newdata = tests[4:8])
  p.a5 <- predict(object = fit_svm.a5,newdata = tests[4:8])
  p.b1 <- predict(object = fit_svm.b1,newdata = tests[9:10])
  p.b2 <- predict(object = fit_svm.b2,newdata = tests[9:10])
  
  #compareTable <- table (trains_1.a$a1, predict(fit_svm)) 
  
  result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
  barplot(result,main = "SVMF")
  return(result)
  
  
  
}


