library(xgboost)
#Build data
trains.a <-tail(dlt,320)[1:319,]
results<-tail(dlt,319)
tests.a<-tail(dlt,1)
#trains.a
#Convert
#a_dlt<-Matrix::sparse.model.matrix( ~ a1+a2+a3+a4+a5 -1, data = trains.a)
#t_dlt<-Matrix::sparse.model.matrix(~ a1+a2+a3+a4+a5 -1, data = tests.a)
#a_dlt<-dlt.convert(trains.a$n,trains.a[,4:8],1)[,2:36]
#t_dlt<-dlt.convert(tests.a$n,tests.a[,4:8],1)[,2:36]
#a_dlt<-dlt.convert(trains.a$n,trains.a[,4:8],3)[,2:31]
a_dlt<-Matrix(as.matrix(trains.a[,4:8]),sparse=T)
#t_dlt<-dlt.convert(tests.a$n,tests.a[,4:8],3)[,2:31]
t_dlt<-Matrix(as.matrix(tests.a[,4:8]),sparse=T)
n=300
#A1:
bst.a1 <- xgboost(data = a_dlt,label = results$a1,nrounds = n)
p.a1 <- predict(object = bst.a1,newdata = t(t_dlt))
barplot(p.a1)
#A2:
bst.a2 <- xgboost(data = a_dlt,label = results$a2,nrounds = n)
p.a2 <- predict(object = bst.a2,newdata = t(t_dlt))
barplot(p.a2)
#A3:
bst.a3 <- xgboost(data = a_dlt,label = results$a3,nrounds = n)
p.a3 <- predict(object = bst.a3,newdata = t(t_dlt))
barplot(p.a3)
#A4:
bst.a4 <- xgboost(data = a_dlt,label = results$a4,nrounds = n)
p.a4 <- predict(object = bst.a4,newdata = t(t_dlt))
barplot(p.a4)
#A5:
bst.a5 <- xgboost(data = a_dlt,label = results$a5,nrounds = n)
p.a5 <- predict(object = bst.a5,newdata = t(t_dlt))
barplot(p.a5)

#Build Data
trains.b <-tail(dlt,151)[1:150,]
results<-tail(dlt,150)
tests.b<-tail(dlt,1)
#Convert
#b_dlt<-Matrix::sparse.model.matrix( ~ b1+b2 -1, data = trains.b)
#t_dlt<-Matrix::sparse.model.matrix(~ b1+b2 -1, data = tests.b)
b_dlt<-dlt.convert(trains.b$n,trains.b[,9:10],2)[,2:13]
t_dlt<-dlt.convert(tests.b$n,tests.b[,9:10],2)[,2:13]
#b_dlt<-dlt.convert(trains.b$n,trains.b[,9:10],3)[,2:13]
#b_dlt<-Matrix(as.matrix(trains.b[,9:10]),sparse=T)
#t_dlt<-dlt.convert(tests.b$n,tests.b[,9:10],3)[,2:13]
#t_dlt<-Matrix(as.matrix(tests.b[,9:10]),sparse=T)
m<-200
#B1:
bst.b1 <- xgboost(data = b_dlt,label = results$b1,nrounds = m)
p.b1 <- predict(object = bst.b1,newdata = t(t_dlt))
barplot(p.b1)
#B2:
bst.b2 <- xgboost(data = b_dlt,label = results$b2,nrounds = m)
p.b2 <- predict(object = bst.b2,newdata = t(t_dlt))
barplot(p.b2)

p.a1<-mean(p.a1)
p.a2<-mean(p.a2)
p.a3<-mean(p.a3)
p.a4<-mean(p.a4)
p.a5<-mean(p.a5)
result.a<-c(p.a1,p.a2,p.a3,p.a4,p.a5)
p.b1
p.b2
result<-c(sort(result.a),mean(p.b1),mean(p.b2))
result
plot(result)



