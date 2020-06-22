dlt.GLMFII <- function(data,count) {
  library(glmnet)
  library(doParallel)
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
  trains.a1<-data.frame(a1.2,a1.1,resa1)
  trains.a2<-data.frame(a2.2,a2.1,resa2)
  trains.a3<-data.frame(a3.2,a3.1,resa3)
  trains.a4<-data.frame(a4.2,a4.1,resa4)
  trains.a5<-data.frame(a5.2,a5.1,resa5)
  #B:
  b1.1<-trains_1$b1
  b2.1<-trains_1$b2
  b1.2<-trains_2$b1
  b2.2<-trains_2$b2
  resb1<-results$b1
  resb2<-results$b2
  trains.b1<-data.frame(b1.2,b1.1,resb1)
  trains.b2<-data.frame(b2.2,b2.1,resb2)
  #Build gGLM model
  fit_glm.a11 = glmnet(as.matrix(trains.a1[,1:2]), trains.a1$resa1, family="gaussian", nlambda=50, alpha=1)
  fit_glm.a22 = glmnet(as.matrix(trains.a2[,1:2]), trains.a2$resa2, family="gaussian", nlambda=50, alpha=1)
  fit_glm.a33 = glmnet(as.matrix(trains.a3[,1:2]), trains.a3$resa3, family="gaussian", nlambda=50, alpha=1)
  fit_glm.a44 = glmnet(as.matrix(trains.a4[,1:2]), trains.a4$resa4, family="gaussian", nlambda=50, alpha=1)
  fit_glm.a55 = glmnet(as.matrix(trains.a5[,1:2]), trains.a5$resa5, family="gaussian", nlambda=50, alpha=1)
  fit_glm.b11 = glmnet(as.matrix(trains.b1[,1:2]), trains.b1$resb1, family="gaussian", nlambda=50, alpha=1)
  fit_glm.b22 = glmnet(as.matrix(trains.b2[,1:2]), trains.b2$resb2, family="gaussian", nlambda=50, alpha=1)
  #plot(fit_glm.a11, xvar="lambda", label=TRUE)
  #plot(fit_glm.a22, xvar="lambda", label=TRUE)
  #plot(fit_glm.a33, xvar="lambda", label=TRUE)
  #plot(fit_glm.a44, xvar="lambda", label=TRUE)
  #plot(fit_glm.a55, xvar="lambda", label=TRUE)
  #plot(fit_glm.b11, xvar="lambda", label=TRUE)
  #plot(fit_glm.b22, xvar="lambda", label=TRUE)
  cvfit_glm.a11 = cv.glmnet(as.matrix(trains.a1[,1:2]), trains.a1$resa1, family = "gaussian", type.measure = "mse",parallel = TRUE)
  cvfit_glm.a22 = cv.glmnet(as.matrix(trains.a2[,1:2]), trains.a2$resa2, family = "gaussian", type.measure = "mse",parallel = TRUE)
  cvfit_glm.a33 = cv.glmnet(as.matrix(trains.a3[,1:2]), trains.a3$resa3, family = "gaussian", type.measure = "mse",parallel = TRUE)
  cvfit_glm.a44 = cv.glmnet(as.matrix(trains.a4[,1:2]), trains.a4$resa4, family = "gaussian", type.measure = "mse",parallel = TRUE)
  cvfit_glm.a55 = cv.glmnet(as.matrix(trains.a5[,1:2]), trains.a5$resa5, family = "gaussian", type.measure = "mse",parallel = TRUE)
  cvfit_glm.b11 = cv.glmnet(as.matrix(trains.b1[,1:2]), trains.b1$resb1, family = "gaussian", type.measure = "mse",parallel = TRUE)
  cvfit_glm.b22 = cv.glmnet(as.matrix(trains.b2[,1:2]), trains.b2$resb2, family = "gaussian", type.measure = "mse",parallel = TRUE)
  #plot(cvfit_glm.a11)
  #plot(cvfit_glm.a22)
  #plot(cvfit_glm.a33)
  #plot(cvfit_glm.a44)
  #plot(cvfit_glm.a55)
  #plot(cvfit_glm.b11)
  #plot(cvfit_glm.b22)
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
  #GLM predict suit
  p.a1<-predict(cvfit_glm.a11, newx=as.matrix(tests.a11[3:4]),s="lambda.1se")
  p.a2<-predict(cvfit_glm.a22, newx=as.matrix(tests.a22[3:4]),s="lambda.1se")
  p.a3<-predict(cvfit_glm.a33, newx=as.matrix(tests.a33[3:4]),s="lambda.1se")
  p.a4<-predict(cvfit_glm.a44, newx=as.matrix(tests.a44[3:4]),s="lambda.1se")
  p.a5<-predict(cvfit_glm.a55, newx=as.matrix(tests.a55[3:4]),s="lambda.1se")
  p.b1<-predict(cvfit_glm.b11, newx=as.matrix(tests.b11[3:4]),s="lambda.1se")
  p.b2<-predict(cvfit_glm.b22, newx=as.matrix(tests.b22[3:4]),s="lambda.1se")
  
  result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
  barplot(result,main = "GLMFII")
  return(result)
  
  
}


