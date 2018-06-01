library(glmnet)
library(doParallel)
#Build data
tests<-tail(dlt,1)
trains.a <-tail(dlt,251)[1:250,]
results.a<-tail(dlt,250)
#A:
trn<-trains$n
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
#B:
trains.b<-tail(dlt,126)[1:125,]
results.b<-tail(dlt,125)
b1<-trains.b$b1
b2<-trains.b$b2
resb1<-results.b$b1
resb2<-results.b$b2
trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)
#Build GLM model
fit_glm.a1 = glmnet(as.matrix(trains.a1[,1:5]), trains.a1$resa1, family="gaussian", nlambda=50, alpha=1)
fit_glm.a2 = glmnet(as.matrix(trains.a2[,1:5]), trains.a2$resa2, family="gaussian", nlambda=50, alpha=1)
fit_glm.a3 = glmnet(as.matrix(trains.a3[,1:5]), trains.a3$resa3, family="gaussian", nlambda=50, alpha=1)
fit_glm.a4 = glmnet(as.matrix(trains.a4[,1:5]), trains.a4$resa4, family="gaussian", nlambda=50, alpha=1)
fit_glm.a5 = glmnet(as.matrix(trains.a5[,1:5]), trains.a5$resa5, family="gaussian", nlambda=50, alpha=1)
fit_glm.b1 = glmnet(as.matrix(trains.b1[,1:2]), trains.b1$resb1, family="gaussian", nlambda=50, alpha=1)
fit_glm.b2 = glmnet(as.matrix(trains.b2[,1:2]), trains.b2$resb2, family="gaussian", nlambda=50, alpha=1)
plot(fit_glm.a1, xvar="lambda", label=TRUE)
plot(fit_glm.a2, xvar="lambda", label=TRUE)
plot(fit_glm.a3, xvar="lambda", label=TRUE)
plot(fit_glm.a4, xvar="lambda", label=TRUE)
plot(fit_glm.a5, xvar="lambda", label=TRUE)
plot(fit_glm.b1, xvar="lambda", label=TRUE)
plot(fit_glm.b2, xvar="lambda", label=TRUE)
cvfit_glm.a1 = cv.glmnet(as.matrix(trains.a1[,1:5]), trains.a1$resa1, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a2 = cv.glmnet(as.matrix(trains.a2[,1:5]), trains.a2$resa2, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a3 = cv.glmnet(as.matrix(trains.a3[,1:5]), trains.a3$resa3, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a4 = cv.glmnet(as.matrix(trains.a4[,1:5]), trains.a4$resa4, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a5 = cv.glmnet(as.matrix(trains.a5[,1:5]), trains.a5$resa5, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.b1 = cv.glmnet(as.matrix(trains.b1[,1:2]), trains.b1$resb1, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.b2 = cv.glmnet(as.matrix(trains.b2[,1:2]), trains.b2$resb2, family = "gaussian", type.measure = "mse",parallel = TRUE)
plot(cvfit_glm.a1)
plot(cvfit_glm.a2)
plot(cvfit_glm.a3)
plot(cvfit_glm.a4)
plot(cvfit_glm.a5)
plot(cvfit_glm.b1)
plot(cvfit_glm.b2)
#GLM predict suit
p.a1<-predict(cvfit_glm.a1, newx=as.matrix(tests[4:8]),s="lambda.1se")
p.a2<-predict(cvfit_glm.a2, newx=as.matrix(tests[4:8]),s="lambda.1se")
p.a3<-predict(cvfit_glm.a3, newx=as.matrix(tests[4:8]),s="lambda.1se")
p.a4<-predict(cvfit_glm.a4, newx=as.matrix(tests[4:8]),s="lambda.1se")
p.a5<-predict(cvfit_glm.a5, newx=as.matrix(tests[4:8]),s="lambda.1se")
p.b1<-predict(cvfit_glm.b1, newx=as.matrix(tests[9:10]),s="lambda.1se")
p.b2<-predict(cvfit_glm.b2, newx=as.matrix(tests[9:10]),s="lambda.1se")

result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
result
plot(result)


