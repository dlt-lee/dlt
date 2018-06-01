library(randomForest)
rows<-dim(dlt)[1]
p.data<-matrix(0,nrow = rows-200,ncol = 15)
j<-0
for (i in rows:201) {
  dlt.data<-tail(dlt,i)[1:201,]
  p<-dlt.f.mclust(dlt.data)
  
  j<-j+1
  
  n.a1<-p$nm.a1
  p.a1<-p$p.a1
  p.a1<-data.frame(n.a1,p.a1)
  
  n.a2<-p$nm.a2
  p.a2<-p$p.a2
  p.a2<-data.frame(n.a2,p.a2)
  
  n.a3<-p$nm.a3
  p.a3<-p$p.a3
  p.a3<-data.frame(n.a3,p.a3)
  
  n.a4<-p$nm.a4
  p.a4<-p$p.a4
  p.a4<-data.frame(n.a4,p.a4)
  
  n.a5<-p$nm.a5
  p.a5<-p$p.a5
  p.a5<-data.frame(n.a5,p.a5)
  
  n.b1<-p$nm.b1
  p.b1<-p$p.b1
  p.b1<-data.frame(n.b1,p.b1)
  
  n.b2<-p$nm.b2
  p.b2<-p$p.b2
  p.b2<-data.frame(n.b2,p.b2)
  
  dlt.n<-tail(dlt.data,1)$n
  dlt.a1<-tail(dlt.data,1)$a1
  dlt.a2<-tail(dlt.data,1)$a2
  dlt.a3<-tail(dlt.data,1)$a3
  dlt.a4<-tail(dlt.data,1)$a4
  dlt.a5<-tail(dlt.data,1)$a5
  dlt.b1<-tail(dlt.data,1)$b1
  dlt.b2<-tail(dlt.data,1)$b2
  
  p.data[j, 1]<-dlt.n
  
  p.data[j, 2]<-p.a1[n.a1==dlt.a1,]$p.a1
  p.data[j, 4]<-p.a2[n.a2==dlt.a2,]$p.a2
  p.data[j, 6]<-p.a3[n.a3==dlt.a3,]$p.a3
  p.data[j, 8]<-p.a4[n.a4==dlt.a4,]$p.a4
  p.data[j,10]<-p.a5[n.a5==dlt.a5,]$p.a5
  p.data[j,12]<-p.b1[n.b1==dlt.b1,]$p.b1
  p.data[j,14]<-p.b2[n.b2==dlt.b2,]$p.b2
  
  p.data[j, 3]<-p.a1[n.a1==dlt.a1,]$p.a1-max(p.a1$p.a1)
  p.data[j, 5]<-p.a2[n.a2==dlt.a2,]$p.a2-max(p.a2$p.a2)
  p.data[j, 7]<-p.a3[n.a3==dlt.a3,]$p.a3-max(p.a3$p.a3)
  p.data[j, 9]<-p.a4[n.a4==dlt.a4,]$p.a4-max(p.a4$p.a4)
  p.data[j,11]<-p.a5[n.a5==dlt.a5,]$p.a5-max(p.a5$p.a5)
  p.data[j,13]<-p.b1[n.b1==dlt.b1,]$p.b1-max(p.b1$p.b1)
  p.data[j,15]<-p.b2[n.b2==dlt.b2,]$p.b2-max(p.b2$p.b2)
  
}

n<-p.data[,1]
p.a1<-p.data[, 2]
d.a1<-p.data[, 3]
p.a2<-p.data[, 4]
d.a2<-p.data[, 5]
p.a3<-p.data[, 6]
d.a3<-p.data[, 7]
p.a4<-p.data[, 8]
d.a4<-p.data[, 9]
p.a5<-p.data[,10]
d.a5<-p.data[,11]
p.b1<-p.data[,12]
d.b1<-p.data[,13]
p.b2<-p.data[,14]
d.b2<-p.data[,15]

pd.data<-data.frame(n,p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2,d.a1,d.a2,d.a3,d.a4,d.a5,d.b1,d.b2)

##################################################################################################

trains_1 <-tail(pd.data,rows-200)[1:(rows-202),]
trains_2 <-tail(pd.data,rows-200)[2:(rows-201),]
results<-tail(pd.data,rows-200)[3:(rows-200),]
tests_1<-tail(pd.data,2)[1,]
tests_2<-tail(pd.data,2)[2,]
#A:
trn1<-trains_1$n
trn2<-trains_2$n
a1.1<-trains_1$p.a1
a2.1<-trains_1$p.a2
a3.1<-trains_1$p.a3
a4.1<-trains_1$p.a4
a5.1<-trains_1$p.a5
a1.2<-trains_2$p.a1
a2.2<-trains_2$p.a2
a3.2<-trains_2$p.a3
a4.2<-trains_2$p.a4
a5.2<-trains_2$p.a5
resa1<-results$p.a1
resa2<-results$p.a2
resa3<-results$p.a3
resa4<-results$p.a4
resa5<-results$p.a5
trains.a1<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
trains.a2<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
trains.a3<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
trains.a4<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
trains.a5<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
#B:
b1.1<-trains_1$p.b1
b2.1<-trains_1$p.b2
b1.2<-trains_2$p.b1
b2.2<-trains_2$p.b2
resb1<-results$p.b1
resb2<-results$p.b2
trains.b1<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
trains.b2<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
#Build DRForest model
fit_drf.a11 = randomForest(resa1 ~ a1.2+a1.1,data = trains.a1,importance = T)
fit_drf.a22 = randomForest(resa2 ~ a2.2+a2.1,data = trains.a2,importance = T)
fit_drf.a33 = randomForest(resa3 ~ a3.2+a3.1,data = trains.a3,importance = T)
fit_drf.a44 = randomForest(resa4 ~ a4.2+a4.1,data = trains.a4,importance = T)
fit_drf.a55 = randomForest(resa5 ~ a5.2+a5.1,data = trains.a5,importance = T)
fit_drf.b11 = randomForest(resb1 ~ b1.2+b1.1,data = trains.b1,importance = T)
fit_drf.b22 = randomForest(resb2 ~ b2.2+b1.1,data = trains.b1,importance = T)
#Buil test data
#A:
tsn1<-tests_1$n
tsn2<-tests_2$n
a1.1<-tests_1$p.a1
a2.1<-tests_1$p.a2
a3.1<-tests_1$p.a3
a4.1<-tests_1$p.a4
a5.1<-tests_1$p.a5
a1.2<-tests_2$p.a1
a2.2<-tests_2$p.a2
a3.2<-tests_2$p.a3
a4.2<-tests_2$p.a4
a5.2<-tests_2$p.a5
tests.a11<-data.frame(tsn1,tsn2,a1.2,a1.1)
tests.a22<-data.frame(tsn1,tsn2,a2.2,a2.1)
tests.a33<-data.frame(tsn1,tsn2,a3.2,a3.1)
tests.a44<-data.frame(tsn1,tsn2,a4.2,a4.1)
tests.a55<-data.frame(tsn1,tsn2,a5.2,a5.1)
#B:
b1.1<-tests_1$p.b1
b2.1<-tests_1$p.b2
b1.2<-tests_2$p.b1
b2.2<-tests_2$p.b2
tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
#DRForest tesing suit
p.c.a11 = predict(fit_drf.a11,tests.a11)
p.c.a22 = predict(fit_drf.a22,tests.a22)
p.c.a33 = predict(fit_drf.a33,tests.a33)
p.c.a44 = predict(fit_drf.a44,tests.a44)
p.c.a55 = predict(fit_drf.a55,tests.a55)
p.c.b11 = predict(fit_drf.b11,tests.b11)
p.c.b22 = predict(fit_drf.b22,tests.b22)

p.c.result<-c(p.c.a11,p.c.a22,p.c.a33,p.c.a44,p.c.a55,p.c.b11,p.c.b22)
p.c.result
plot(p.c.result)

##################################################################################################

trains_1 <-tail(pd.data,rows-200)[1:(rows-202),]
trains_2 <-tail(pd.data,rows-200)[2:(rows-201),]
results<-tail(pd.data,rows-200)[3:(rows-200),]
tests_1<-tail(pd.data,2)[1,]
tests_2<-tail(pd.data,2)[2,]
#A:
trn1<-trains_1$n
trn2<-trains_2$n
a1.1<-trains_1$d.a1
a2.1<-trains_1$d.a2
a3.1<-trains_1$d.a3
a4.1<-trains_1$d.a4
a5.1<-trains_1$d.a5
a1.2<-trains_2$d.a1
a2.2<-trains_2$d.a2
a3.2<-trains_2$d.a3
a4.2<-trains_2$d.a4
a5.2<-trains_2$d.a5
resa1<-results$d.a1
resa2<-results$d.a2
resa3<-results$d.a3
resa4<-results$d.a4
resa5<-results$d.a5
trains.a1<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
trains.a2<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
trains.a3<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
trains.a4<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
trains.a5<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
#B:
b1.1<-trains_1$d.b1
b2.1<-trains_1$d.b2
b1.2<-trains_2$d.b1
b2.2<-trains_2$d.b2
resb1<-results$d.b1
resb2<-results$d.b2
trains.b1<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
trains.b2<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
#Build DRForest model
fit_drf.a11 = randomForest(resa1 ~ a1.2+a1.1,data = trains.a1,importance = T)
fit_drf.a22 = randomForest(resa2 ~ a2.2+a2.1,data = trains.a2,importance = T)
fit_drf.a33 = randomForest(resa3 ~ a3.2+a3.1,data = trains.a3,importance = T)
fit_drf.a44 = randomForest(resa4 ~ a4.2+a4.1,data = trains.a4,importance = T)
fit_drf.a55 = randomForest(resa5 ~ a5.2+a5.1,data = trains.a5,importance = T)
fit_drf.b11 = randomForest(resb1 ~ b1.2+b1.1,data = trains.b1,importance = T)
fit_drf.b22 = randomForest(resb2 ~ b2.2+b1.1,data = trains.b1,importance = T)
#Buil test data
#A:
tsn1<-tests_1$n
tsn2<-tests_2$n
a1.1<-tests_1$d.a1
a2.1<-tests_1$d.a2
a3.1<-tests_1$d.a3
a4.1<-tests_1$d.a4
a5.1<-tests_1$d.a5
a1.2<-tests_2$d.a1
a2.2<-tests_2$d.a2
a3.2<-tests_2$d.a3
a4.2<-tests_2$d.a4
a5.2<-tests_2$d.a5
tests.a11<-data.frame(tsn1,tsn2,a1.2,a1.1)
tests.a22<-data.frame(tsn1,tsn2,a2.2,a2.1)
tests.a33<-data.frame(tsn1,tsn2,a3.2,a3.1)
tests.a44<-data.frame(tsn1,tsn2,a4.2,a4.1)
tests.a55<-data.frame(tsn1,tsn2,a5.2,a5.1)
#B:
b1.1<-tests_1$d.b1
b2.1<-tests_1$d.b2
b1.2<-tests_2$d.b1
b2.2<-tests_2$d.b2
tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
#DRForest testing suit
d.c.a11 = predict(fit_drf.a11,tests.a11)
d.c.a22 = predict(fit_drf.a22,tests.a22)
d.c.a33 = predict(fit_drf.a33,tests.a33)
d.c.a44 = predict(fit_drf.a44,tests.a44)
d.c.a55 = predict(fit_drf.a55,tests.a55)
d.c.b11 = predict(fit_drf.b11,tests.b11)
d.c.b22 = predict(fit_drf.b22,tests.b22)

d.c.result<-c(d.c.a11,d.c.a22,d.c.a33,d.c.a44,d.c.a55,d.c.b11,d.c.b22)
d.c.result
plot(d.c.result)


######################################################################################

p.c.result
d.c.result



