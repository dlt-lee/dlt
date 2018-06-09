library(caret)
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

#SVM
#a1.Predictions<-a1.Predictions-3
#a2.Predictions<-a2.Predictions
#a3.Predictions<-a3.Predictions
#a4.Predictions<-a4.Predictions-1
#a5.Predictions<-a5.Predictions+4
#b1.Predictions<-b1.Predictions-1
#b2.Predictions<-b2.Predictions+2

c(tail(a1.Predictions,1),
  tail(a2.Predictions,1),
  tail(a3.Predictions,1),
  tail(a4.Predictions,1),
  tail(a5.Predictions,1),
  tail(b1.Predictions,1),
  tail(b2.Predictions,1)
)

#GLM
#a1.Predictions<-a1.Predictions-1
#a2.Predictions<-a2.Predictions-3
#a3.Predictions<-a3.Predictions+3
#a4.Predictions<-a4.Predictions+7
#b2.Predictions<-b2.Predictions+2

c(tail(a1.Predictions,1),
  tail(a2.Predictions,1),
  tail(a3.Predictions,1),
  tail(a4.Predictions,1),
  tail(a5.Predictions,1),
  tail(b1.Predictions,1),
  tail(b2.Predictions,1)
)

#NNet
#a1.Predictions<-a1.Predictions-3
#a2.Predictions<-a2.Predictions-3
#a3.Predictions<-a3.Predictions-1
#a4.Predictions<-a4.Predictions+3
#a5.Predictions<-a5.Predictions+5
#b1.Predictions<-b1.Predictions-2
#b2.Predictions<-b2.Predictions+3

c(tail(a1.Predictions,1),
  tail(a2.Predictions,1),
  tail(a3.Predictions,1),
  tail(a4.Predictions,1),
  tail(a5.Predictions,1),
  tail(b1.Predictions,1),
  tail(b2.Predictions,1)
)

#party
#a1.Predictions<-a1.Predictions-1
#a2.Predictions<-a2.Predictions-1
#a3.Predictions<-a3.Predictions-1
#a4.Predictions<-a4.Predictions
#a5.Predictions<-a5.Predictions+5
#b1.Predictions<-b1.Predictions-1
#b2.Predictions<-b2.Predictions+3

c(tail(a1.Predictions,1),
  tail(a2.Predictions,1),
  tail(a3.Predictions,1),
  tail(a4.Predictions,1),
  tail(a5.Predictions,1),
  tail(b1.Predictions,1),
  tail(b2.Predictions,1))

#MDA
#a1.Predictions<-a1.Predictions
#a2.Predictions<-a2.Predictions+1
#a3.Predictions<-a3.Predictions+2
#a4.Predictions<-a4.Predictions+5
#a5.Predictions<-a5.Predictions+12
#b1.Predictions<-b1.Predictions
#b2.Predictions<-b2.Predictions+1

c(tail(a1.Predictions,1),
  tail(a2.Predictions,1),
  tail(a3.Predictions,1),
  tail(a4.Predictions,1),
  tail(a5.Predictions,1),
  tail(b1.Predictions,1),
  tail(b2.Predictions,1)
)

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

c(tail(floor(testPredictions.a1),1),
  tail(floor(testPredictions.a2),1),
  tail(floor(testPredictions.a3),1),
  tail(floor(testPredictions.a4),1),
  tail(floor(testPredictions.a5),1),
  tail(floor(testPredictions.b1),1),
  tail(floor(testPredictions.b2),1)
)

Resampling<-resamples(list(svmFit.a1,logisticReg.a1))
summary(Resampling)
modelDifferences<-diff(Resampling)
summary(modelDifferences)

summary(a1.delta)
#Extend a Numerical Range by a Small Percentage
axisRange<-extendrange(c(data.f.ab$a1,a1.Predictions))
plot(data.f.ab$a1,a1.Predictions,
     ylim = axisRange,
     xlim = axisRange)
plot(a1.Predictions,a1.delta,ylab = "residual")
abline(h=0,col="darkgrey",lty=2)

R2(a1.Predictions,data.f.ab$a1)
RMSE(a1.Predictions,data.f.ab$a1)

#Correlation, Variance and Covariance (Matrices)
cor(a1.Predictions,data.f.ab$a1)
cor(a1.Predictions,data.f.ab$a1,method = "spearman")




