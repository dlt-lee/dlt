data<-dlt
count<-dim(dlt)[1]
#dlt.xgboost <- function(data,count) {
library(xgboost)

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
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa1)
trains.a2<-data.frame(trn1,trn2,trn3,
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa2)
trains.a3<-data.frame(trn1,trn2,trn3,
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa3)
trains.a4<-data.frame(trn1,trn2,trn3,
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa4)
trains.a5<-data.frame(trn1,trn2,trn3,
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resa5)
trains.b1<-data.frame(trn1,trn2,trn3,
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resb1)
trains.b2<-data.frame(trn1,trn2,trn3,
                      #a1.1,a2.1,a3.1,a4.1,a5.1,
                      a1.2,a2.2,a3.2,a4.2,a5.2,
                      a1.3,a2.3,a3.3,a4.3,a5.3,
                      #b1.1,b2.1,
                      b1.2,b2.2,
                      b1.3,b2.3,
                      resb2)

trains.T.a1<-Matrix(as.matrix(trains.a1[,4:17]),sparse=T)
trains.T.a2<-Matrix(as.matrix(trains.a2[,4:17]),sparse=T)
trains.T.a3<-Matrix(as.matrix(trains.a3[,4:17]),sparse=T)
trains.T.a4<-Matrix(as.matrix(trains.a4[,4:17]),sparse=T)
trains.T.a5<-Matrix(as.matrix(trains.a5[,4:17]),sparse=T)
trains.T.b1<-Matrix(as.matrix(trains.b1[,4:17]),sparse=T)
trains.T.b2<-Matrix(as.matrix(trains.b2[,4:17]),sparse=T)
n=300
#A:
bst.a1 <- xgboost(data = trains.T.a1,label = trains.a1$resa1,nrounds = n)
bst.a2 <- xgboost(data = trains.T.a1,label = trains.a2$resa2,nrounds = n)
bst.a3 <- xgboost(data = trains.T.a1,label = trains.a3$resa3,nrounds = n)
bst.a4 <- xgboost(data = trains.T.a1,label = trains.a4$resa4,nrounds = n)
bst.a5 <- xgboost(data = trains.T.a1,label = trains.a5$resa5,nrounds = n)
bst.b1 <- xgboost(data = trains.T.a1,label = trains.b1$resb1,nrounds = n)
bst.b2 <- xgboost(data = trains.T.a1,label = trains.b2$resb2,nrounds = n)


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
                     #a1.1,a2.1,a3.1,a4.1,a5.1,
                     a1.2,a2.2,a3.2,a4.2,a5.2,
                     a1.3,a2.3,a3.3,a4.3,a5.3,
                    #b1.1,b2.1,
                     b1.2,b2.2,
                     b1.3,b2.3)

tests.T.ab<-Matrix(as.matrix(tests.ab[,4:17]),sparse=T)

testPredictions.a1 <- predict(object = bst.a1,newdata = tests.T.ab)
testPredictions.a2 <- predict(object = bst.a2,newdata = tests.T.ab)
testPredictions.a3 <- predict(object = bst.a3,newdata = tests.T.ab)
testPredictions.a4 <- predict(object = bst.a4,newdata = tests.T.ab)
testPredictions.a5 <- predict(object = bst.a5,newdata = tests.T.ab)
testPredictions.b1 <- predict(object = bst.b1,newdata = tests.T.ab)
testPredictions.b2 <- predict(object = bst.b2,newdata = tests.T.ab)

####################################################################

rows<-length(testPredictions.a1)-1
data.f.ab<-tail(dlt,rows)


#A1:
a1.Predictions<-head(testPredictions.a1,rows)
a1.Predictions<-floor(a1.Predictions)
a1.delta<-data.f.ab$a1-(a1.Predictions)
barplot(table(a1.delta),main = "a1")
#A2:
a2.Predictions<-head(testPredictions.a2,rows)
a2.Predictions<-floor(a2.Predictions)
a2.delta<-data.f.ab$a2-a2.Predictions
barplot(table(a2.delta),main = "a2") 
#A3:
a3.Predictions<-head(testPredictions.a3,rows)
a3.Predictions<-floor(a3.Predictions)
a3.delta<-data.f.ab$a3-a3.Predictions
barplot(table(a3.delta),main = "a3") 
#A4:
a4.Predictions<-head(testPredictions.a4,rows)
a4.Predictions<-floor(a4.Predictions)
a4.delta<-data.f.ab$a4-(a4.Predictions)
barplot(table(a4.delta),main = "a4") 
#A5:
a5.Predictions<-head(testPredictions.a5,rows)
a5.Predictions<-floor(a5.Predictions)
a5.delta<-data.f.ab$a5-(a5.Predictions)
barplot(table(a5.delta),main = "a5") 
#B1:
b1.Predictions<-head(testPredictions.b1,rows)
b1.Predictions<-floor(b1.Predictions)
b1.delta<-data.f.ab$b1-(b1.Predictions)
barplot(table(b1.delta),main = "b1")
#B2:
b2.Predictions<-head(testPredictions.b2,rows)
b2.Predictions<-floor(b2.Predictions)
b2.delta<-data.f.ab$b2-(b2.Predictions)
barplot(table(b2.delta),main = "b2")


barplot(tail(a1.delta,40),main = "a1")
barplot(tail(a2.delta,40),main = "a2")
barplot(tail(a3.delta,40),main = "a3")
barplot(tail(a4.delta,40),main = "a4")
barplot(tail(a5.delta,40),main = "a5")
barplot(tail(b1.delta,40),main = "b1")
barplot(tail(b2.delta,40),main = "b2")

###################################################################################


n.a<-0
n.b<-0
n.c<-0
for (i in 1:rows) {
  temp.a<-0
  temp.b<-0
  temp.c<-0
  if (data.f.ab[i,]$a1==a1.Predictions[i]) {
    temp.a<-temp.a+1
  }
  if (data.f.ab[i,]$a2==a2.Predictions[i]) {
    temp.a<-temp.a+1
  }
  if (data.f.ab[i,]$a3==a3.Predictions[i]) {
    temp.a<-temp.a+1
  }
  if (data.f.ab[i,]$a4==a4.Predictions[i]) {
    temp.a<-temp.a+1
  }
  if (data.f.ab[i,]$a5==a5.Predictions[i]) {
    temp.a<-temp.a+1
  }
  n.a<-c(n.a,temp.a)
  if (data.f.ab[i,]$b1==b1.Predictions[i]) {
    temp.b<-temp.b+1
  }
  if (data.f.ab[i,]$b2==b2.Predictions[i]) {
    temp.b<-temp.b+1
  }
  n.a<-c(n.a,temp.a)
  n.b<-c(n.b,temp.b)
  if (temp.a==5&temp.b==2) {
    temp.c<-1
  }
  else if(temp.a==5&temp.b==1) {
    temp.c<-2
  }
  else if (temp.a==5|(temp.a==4&temp.b==2)) {
    temp.c<-3
  }
  else if ((temp.a==4&temp.b==1)|(temp.a==3&temp.b==2)) {
    temp.c<-4
  }
  else if (temp.a==4|(temp.a==3&temp.b==1)|(temp.a==2&temp.b==2)) {
    temp.c<-5
  }
  else if (temp.a==3|(temp.a==1&temp.b==2)|(temp.a==2&temp.b==1)|temp.b==2) {
    temp.c<-6
  }
  n.c<-c(n.c,temp.c)
}
table(n.a)
table(n.b)
table(n.c)

print(c(tail(floor(testPredictions.a1),1),
        tail(floor(testPredictions.a2),1),
        tail(floor(testPredictions.a3),1),
        tail(floor(testPredictions.a4),1),
        tail(floor(testPredictions.a5),1),
        tail(floor(testPredictions.b1),1),
        tail(floor(testPredictions.b2),1)
))


