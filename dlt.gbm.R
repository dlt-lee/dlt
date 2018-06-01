library(gbm)
#Build data
tests<-tail(dlt,1)

trains.a <-tail(dlt,251)[1:250,]
results.a<-tail(dlt,250)

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


trains.b <-tail(dlt,126)[1:125,]
results.b<-tail(dlt,125)

b1<-trains.b$b1
b2<-trains.b$b2

resb1<-results.b$b1
resb2<-results.b$b2

trains.a1<-data.frame(a1,a2,a3,a4,a5,resa1)
trains.a2<-data.frame(a1,a2,a3,a4,a5,resa2)
trains.a3<-data.frame(a1,a2,a3,a4,a5,resa3)
trains.a4<-data.frame(a1,a2,a3,a4,a5,resa4)
trains.a5<-data.frame(a1,a2,a3,a4,a5,resa5)

trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)

#Build GBM model
fit_gdm.a1<-gbm(resa1~a1+a2+a3+a4+a5,data = trains.a1,distribution="gaussian",n.trees=50000,cv.folds = 5,shrinkage=0.005)
#best.iter.a1<-gbm.perf(fit_gdm.a1,method="OOB")
best.iter.a1<-gbm.perf(fit_gdm.a1,method="cv")
fit_gdm.a2<-gbm(resa2~a1+a2+a3+a4+a5,data = trains.a2,distribution="gaussian",n.trees=50000,cv.folds = 5,shrinkage=0.005)
#best.iter.a2<-gbm.perf(fit_gdm.a2,method="OOB")
best.iter.a2<-gbm.perf(fit_gdm.a2,method="cv")
fit_gdm.a3<-gbm(resa3~a1+a2+a3+a4+a5,data = trains.a3,distribution="gaussian",n.trees=50000,cv.folds = 5,shrinkage=0.005)
#best.iter.a3<-gbm.perf(fit_gdm.a3,method="OOB")
best.iter.a3<-gbm.perf(fit_gdm.a3,method="cv")
fit_gdm.a4<-gbm(resa4~a1+a2+a3+a4+a5,data = trains.a4,distribution="gaussian",n.trees=50000,cv.folds = 5,shrinkage=0.005)
#best.iter.a4<-gbm.perf(fit_gdm.a4,method="OOB")
best.iter.a4<-gbm.perf(fit_gdm.a4,method="cv")
fit_gdm.a5<-gbm(resa5~a1+a2+a3+a4+a5,data = trains.a5,distribution="gaussian",n.trees=50000,cv.folds = 5,shrinkage=0.005)
#best.iter.a5<-gbm.perf(fit_gdm.a5,method="OOB")
best.iter.a5<-gbm.perf(fit_gdm.a5,method="cv")
fit_gdm.b1<-gbm(resb1~b1+b2,data = trains.b1,distribution="gaussian",n.trees=5000,cv.folds = 5,shrinkage=0.005)
#best.iter.b1<-gbm.perf(fit_gdm.b1,method="OOB")
best.iter.b1<-gbm.perf(fit_gdm.b1,method="cv")
fit_gdm.b2<-gbm(resb2~b1+b2,data = trains.b2,distribution="gaussian",n.trees=5000,cv.folds = 5,shrinkage=0.005)
#best.iter.b2<-gbm.perf(fit_gdm.b2,method="OOB")
best.iter.b2<-gbm.perf(fit_gdm.b2,method="cv")

#GBM tesing suit
p.a1 <- predict(fit_gdm.a1, tests[4:8],best.iter.a1)
p.a2 <- predict(fit_gdm.a2, tests[4:8],best.iter.a2)
p.a3 <- predict(fit_gdm.a3, tests[4:8],best.iter.a3)
p.a4 <- predict(fit_gdm.a4, tests[4:8],best.iter.a4)
p.a5 <- predict(fit_gdm.a5, tests[4:8],best.iter.a5)
p.b1 <- predict(fit_gdm.b1, tests[9:10],best.iter.b1)
p.b2 <- predict(fit_gdm.b2, tests[9:10],best.iter.b2)


result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
result
plot(result)

p.a1<-table(fit_gdm.a1$cv.fitted,trains.a1$resa1)
barplot(p.a1,main = "a1")





