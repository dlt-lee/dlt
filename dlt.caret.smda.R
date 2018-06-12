data<-dlt
count<-dim(dlt)[1]
#dlt.mda <- function(data,count) {
library(caret)
library(sparseLDA)
library(mda)
library(rda)



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

set.seed(100)

ctrl<-trainControl(summaryFunction = twoClassSummary,
                   classProbs = TRUE)

smdaFit.a1<-train(resa1~
                    #a1.1+a2.1+a3.1+a4.1+a5.1+
                    a1.2+a2.2+a3.2+a4.2+a5.2+
                    a1.3+a2.3+a3.3+a4.3+a5.3+
                    #b1.1+b2.1+
                    b1.2+b2.2+
                    b1.3+b2.3,
                  data = trains.a1,
                 method = "smda",
                 metric = "ROC",
                 tuneGrid = expand.grid(.subclasses = 1:14),
                 trControl = ctrl
                 )
mdaModel.a2<-smda(resa2~
                   #a1.1+a2.1+a3.1+a4.1+a5.1+
                   a1.2+a2.2+a3.2+a4.2+a5.2+
                   a1.3+a2.3+a3.3+a4.3+a5.3+
                   #b1.1+b2.1+
                   b1.2+b2.2+
                   b1.3+b2.3,
                 data = trains.a2,
                 preProcess = c("BoxCox","center","scale","pca"),
                 #range(dlt$a1)-1
                 subclasses = 18
)
mdaModel.a3<-mda(resa3~
                   #a1.1+a2.1+a3.1+a4.1+a5.1+
                   a1.2+a2.2+a3.2+a4.2+a5.2+
                   a1.3+a2.3+a3.3+a4.3+a5.3+
                   #b1.1+b2.1+
                   b1.2+b2.2+
                   b1.3+b2.3,
                 data = trains.a3,
                 preProcess = c("BoxCox","center","scale","pca"),
                 #range(dlt$a1)-1
                 subclasses = 16
)
mdaModel.a4<-mda(resa4~
                   #a1.1+a2.1+a3.1+a4.1+a5.1+
                   a1.2+a2.2+a3.2+a4.2+a5.2+
                   a1.3+a2.3+a3.3+a4.3+a5.3+
                   #b1.1+b2.1+
                   b1.2+b2.2+
                   b1.3+b2.3,
                 data = trains.a4,
                 preProcess = c("BoxCox","center","scale","pca"),
                 #range(dlt$a1)-1
                 subclasses = 18
)
mdaModel.a5<-mda(resa5~
                   #a1.1+a2.1+a3.1+a4.1+a5.1+
                   a1.2+a2.2+a3.2+a4.2+a5.2+
                   a1.3+a2.3+a3.3+a4.3+a5.3+
                   #b1.1+b2.1+
                   b1.2+b2.2+
                   b1.3+b2.3,
                 data = trains.a5,
                 preProcess = c("BoxCox","center","scale","pca"),
                 #range(dlt$a1)-1
                 subclasses = 22
)
mdaModel.b1<-mda(resb1~
                   #a1.1+a2.1+a3.1+a4.1+a5.1+
                   a1.2+a2.2+a3.2+a4.2+a5.2+
                   a1.3+a2.3+a3.3+a4.3+a5.3+
                   #b1.1+b2.1+
                   b1.2+b2.2+
                   b1.3+b2.3,
                 data = trains.b1,
                 preProcess = c("BoxCox","center","scale","pca"),
                 #range(dlt$a1)-1
                 subclasses = 47
)
mdaModel.b2<-mda(resb2~
                   #a1.1+a2.1+a3.1+a4.1+a5.1+
                   a1.2+a2.2+a3.2+a4.2+a5.2+
                   a1.3+a2.3+a3.3+a4.3+a5.3+
                   #b1.1+b2.1+
                   b1.2+b2.2+
                   b1.3+b2.3,
                 data = trains.b2,
                 preProcess = c("BoxCox","center","scale","pca"),
                 #range(dlt$a1)-1
                 subclasses = 47
)


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
                     a1.1,a2.1,a3.1,a4.1,a5.1,
                     a1.2,a2.2,a3.2,a4.2,a5.2,
                     a1.3,a2.3,a3.3,a4.3,a5.3,
                     b1.1,b2.1,
                     b2.2,b2.2,
                     b2.3,b2.3)
test.ab<-preProcess(tests.ab,method = c("BoxCox","center","scale","pca"))
testPredictions.a1<-as.numeric(predict(mdaModel.a1,tests.ab))
testPredictions.a2<-as.numeric(predict(mdaModel.a2,tests.ab))
testPredictions.a3<-as.numeric(predict(mdaModel.a3,tests.ab))
testPredictions.a4<-as.numeric(predict(mdaModel.a4,tests.ab))
testPredictions.a5<-as.numeric(predict(mdaModel.a5,tests.ab))
testPredictions.b1<-as.numeric(predict(mdaModel.b1,tests.ab))
testPredictions.b2<-as.numeric(predict(mdaModel.b2,tests.ab))

print(c(tail(testPredictions.a1,1),
        tail(testPredictions.a2,1),
        tail(testPredictions.a3,1),  
        tail(testPredictions.a4,1),
        tail(testPredictions.a5,1),
        tail(testPredictions.b1,1),
        tail(testPredictions.b2,1)))

################################################################################
rows<-length(testPredictions.a1)-1
data.f.ab<-tail(dlt,rows)
#A1:
a1.Predictions<-head(testPredictions.a1,rows)
#a1.Predictions<-floor(a1.Predictions)
a1.delta<-data.f.ab$a1-(a1.Predictions)
barplot(table(a1.delta),main = "a1")
#A2:
a2.Predictions<-head(testPredictions.a2,rows)
#a2.Predictions<-floor(a2.Predictions)
a2.delta<-data.f.ab$a2-a2.Predictions
barplot(table(a2.delta),main = "a2") 
#A3:
a3.Predictions<-head(testPredictions.a3,rows)
#a3.Predictions<-floor(a3.Predictions)
a3.delta<-data.f.ab$a3-a3.Predictions
barplot(table(a3.delta),main = "a3") 
#A4:
a4.Predictions<-head(testPredictions.a4,rows)
#a4.Predictions<-floor(a4.Predictions)
a4.delta<-data.f.ab$a4-(a4.Predictions)
barplot(table(a4.delta),main = "a4") 
#A5:
a5.Predictions<-head(testPredictions.a5,rows)
#a5.Predictions<-floor(a5.Predictions)
a5.delta<-data.f.ab$a5-(a5.Predictions)
barplot(table(a5.delta),main = "a5") 
#B1:
b1.Predictions<-head(testPredictions.b1,rows)
#b1.Predictions<-floor(b1.Predictions)
b1.delta<-data.f.ab$b1-(b1.Predictions)
barplot(table(b1.delta),main = "b1")
#B2:
b2.Predictions<-head(testPredictions.b2,rows)
#b2.Predictions<-floor(b2.Predictions)
b2.delta<-data.f.ab$b2-(b2.Predictions)
barplot(table(b2.delta),main = "b2")

a1.Predictions<-a1.Predictions
a2.Predictions<-a2.Predictions+1
a3.Predictions<-a3.Predictions+2
a4.Predictions<-a4.Predictions+5
a5.Predictions<-a5.Predictions+12
b1.Predictions<-b1.Predictions
b2.Predictions<-b2.Predictions+1

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


print(c(tail(testPredictions.a1,1),
        tail(testPredictions.a2+1,1),
        tail(testPredictions.a3+2,1),  
        tail(testPredictions.a4+5,1),
        tail(testPredictions.a5+12,1),
        tail(testPredictions.b1,1),
        tail(testPredictions.b2+1,1)))


#}