library(randomForest)
#Buil training data
n<-300
a1<-tail(dlt,n+1)$a1[1:n]
a5<-tail(dlt,n+1)$a5[1:n]
sa<-tail(dlt,n+1)$sa[1:n]
b1<-tail(dlt,n+1)$b1[1:n]
b2<-tail(dlt,n+1)$b2[1:n]
sb<-tail(dlt,n+1)$sb[1:n]
mn<-(sa+sb)/7
a1.r<-tail(dlt,n)$a1
a2.r<-tail(dlt,n)$a2
a3.r<-tail(dlt,n)$a3
a4.r<-tail(dlt,n)$a4
a5.r<-tail(dlt,n)$a5
b1.r<-tail(dlt,n)$b1
b2.r<-tail(dlt,n)$b2

data.train.a1<-data.frame(a1,b1,mn,sa,a5,sb,b2,a1.r)
data.train.a2<-data.frame(a1,b1,mn,sa,a5,sb,b2,a2.r)
data.train.a3<-data.frame(a1,b1,mn,sa,a5,sb,b2,a3.r)
data.train.a4<-data.frame(a1,b1,mn,sa,a5,sb,b2,a4.r)
data.train.a5<-data.frame(a1,b1,mn,sa,a5,sb,b2,a5.r)
data.train.b1<-data.frame(a1,b1,mn,sa,a5,sb,b2,b1.r)
data.train.b2<-data.frame(a1,b1,mn,sa,a5,sb,b2,b2.r)
xgb<-Matrix(as.matrix(data.train.a1[,1:7]),sparse=T)


#RandomForest training
rf.a1 = randomForest(a1.r ~ a1+b1+mn+sa+sb+a5+b2,data = data.train.a1,importance = T,ntrees=1000)
rf.a2 = randomForest(a2.r ~ a1+b1+mn+sa+sb+a5+b2,data = data.train.a2,importance = T,ntrees=1000)
rf.a3 = randomForest(a3.r ~ a1+b1+mn+sa+sb+a5+b2,data = data.train.a3,importance = T,ntrees=1000)
rf.a4 = randomForest(a4.r ~ a1+b1+mn+sa+sb+a5+b2,data = data.train.a4,importance = T,ntrees=1000)
rf.a5 = randomForest(a5.r ~ a1+b1+mn+sa+sb+a5+b2,data = data.train.a5,importance = T,ntrees=1000)

#XGBoost training
bst.a1 <- xgboost(data = xgb,label = data.train.a1$a1.r,nrounds = n,print_every_n = 300L)
bst.a2 <- xgboost(data = xgb,label = data.train.a2$a2.r,nrounds = n,print_every_n = 300L)
bst.a3 <- xgboost(data = xgb,label = data.train.a3$a3.r,nrounds = n,print_every_n = 300L)
bst.a4 <- xgboost(data = xgb,label = data.train.a4$a4.r,nrounds = n,print_every_n = 300L)
bst.a5 <- xgboost(data = xgb,label = data.train.a5$a5.r,nrounds = n,print_every_n = 300L)
bst.b1 <- xgboost(data = xgb,label = data.train.b1$b1.r,nrounds = n,print_every_n = 300L)
bst.b2 <- xgboost(data = xgb,label = data.train.b2$b2.r,nrounds = n,print_every_n = 300L)

#Build test data
a1<-tail(dlt,1)$a1
a5<-tail(dlt,1)$a5
sa<-tail(dlt,1)$sa
b1<-tail(dlt,1)$b1
b2<-tail(dlt,1)$b2
sb<-tail(dlt,1)$sb
mn<-(sa+sb)/7
data.test<-data.frame(a1,b1,mn,sa,a5,sb,b2)
xgb.test<-Matrix(as.matrix(data.test),sparse=T)

#RandomForest predict
p.a1 = predict(rf.a1,data.test)
p.a2 = predict(rf.a2,data.test)
p.a3 = predict(rf.a3,data.test)
p.a4 = predict(rf.a4,data.test)
p.a5 = predict(rf.a5,data.test)
p.b1 = predict(rf.b1,data.test)
p.b2 = predict(rf.b2,data.test)

result.rd<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)

#XGBoost
p.a1 <- predict(object = bst.a1,newdata = t(xgb.test))
p.a2 <- predict(object = bst.a2,newdata = t(xgb.test))
p.a3 <- predict(object = bst.a3,newdata = t(xgb.test))
p.a4 <- predict(object = bst.a4,newdata = t(xgb.test))
p.a5 <- predict(object = bst.a5,newdata = t(xgb.test))
p.b1 <- predict(object = bst.b1,newdata = t(xgb.test))
p.b2 <- predict(object = bst.b2,newdata = t(xgb.test))

#Result
result.a<-c(mean(p.a1),mean(p.a2),mean(p.a3),mean(p.a4),mean(p.a5))
result.xgb<-c(sort(result.a),mean(p.b1),mean(p.b2))

floor(result.rd)
floor(result.xgb)




