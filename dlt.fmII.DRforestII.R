library(randomForest)
rows<-dim(dlt)[1]
p.data<-matrix(0,nrow = rows-202,ncol = 15)
j<-0
for (i in rows:203) {
  dlt.data<-tail(dlt,i)[1:203,]
  p<-dlt.f.mclustII(dlt.data)
  
  j<-j+1
  
  n.a11<-p$nm.a11
  p.a11<-p$p.a11
  p.a11<-data.frame(n.a11,p.a11)
  
  n.a22<-p$nm.a22
  p.a22<-p$p.a22
  p.a22<-data.frame(n.a22,p.a22)
  
  n.a33<-p$nm.a33
  p.a33<-p$p.a33
  p.a33<-data.frame(n.a33,p.a33)
  
  n.a44<-p$nm.a44
  p.a44<-p$p.a44
  p.a44<-data.frame(n.a44,p.a44)
  
  n.a55<-p$nm.a55
  p.a55<-p$p.a55
  p.a55<-data.frame(n.a55,p.a55)
  
  n.b11<-p$nm.b11
  p.b11<-p$p.b11
  p.b11<-data.frame(n.b11,p.b11)
  
  n.b22<-p$nm.b22
  p.b22<-p$p.b22
  p.b22<-data.frame(n.b22,p.b22)
  
  dlt.n<-tail(dlt.data,1)$n
  dlt.a11<-tail(dlt.data,1)$a1
  dlt.a22<-tail(dlt.data,1)$a2
  dlt.a33<-tail(dlt.data,1)$a3
  dlt.a44<-tail(dlt.data,1)$a4
  dlt.a55<-tail(dlt.data,1)$a5
  dlt.b11<-tail(dlt.data,1)$b1
  dlt.b22<-tail(dlt.data,1)$b2
  
  p.data[j, 1]<-dlt.n
  
  p.data[j, 2]<-p.a11[n.a11==dlt.a11,]$p.a11
  p.data[j, 4]<-p.a22[n.a22==dlt.a22,]$p.a22
  p.data[j, 6]<-p.a33[n.a33==dlt.a33,]$p.a33
  p.data[j, 8]<-p.a44[n.a44==dlt.a44,]$p.a44
  p.data[j,10]<-p.a55[n.a55==dlt.a55,]$p.a55
  p.data[j,12]<-p.b11[n.b11==dlt.b11,]$p.b11
  p.data[j,14]<-p.b22[n.b22==dlt.b22,]$p.b22
  
  p.data[j, 3]<-p.a11[n.a11==dlt.a11,]$p.a11-max(p.a11$p.a11)
  p.data[j, 5]<-p.a22[n.a22==dlt.a22,]$p.a22-max(p.a22$p.a22)
  p.data[j, 7]<-p.a33[n.a33==dlt.a33,]$p.a33-max(p.a33$p.a33)
  p.data[j, 9]<-p.a44[n.a44==dlt.a44,]$p.a44-max(p.a44$p.a44)
  p.data[j,11]<-p.a55[n.a55==dlt.a55,]$p.a55-max(p.a55$p.a55)
  p.data[j,13]<-p.b11[n.b11==dlt.b11,]$p.b11-max(p.b11$p.b11)
  p.data[j,15]<-p.b22[n.b22==dlt.b22,]$p.b22-max(p.b22$p.b22)
  
}

n<-p.data[,1]
p.a11<-p.data[, 2]
d.a11<-p.data[, 3]
p.a22<-p.data[, 4]
d.a22<-p.data[, 5]
p.a33<-p.data[, 6]
d.a33<-p.data[, 7]
p.a44<-p.data[, 8]
d.a44<-p.data[, 9]
p.a55<-p.data[,10]
d.a55<-p.data[,11]
p.b11<-p.data[,12]
d.b11<-p.data[,13]
p.b22<-p.data[,14]
d.b22<-p.data[,15]

pd.data<-data.frame(n,p.a11,p.a22,p.a33,p.a44,p.a55,p.b11,p.b22,d.a11,d.a22,d.a33,d.a44,d.a55,d.b11,d.b22)

###############################################################################################
trains_1 <-tail(pd.data,rows-200)[1:(rows-204),]
trains_2 <-tail(pd.data,rows-200)[2:(rows-203),]
results<-tail(pd.data,rows-200)[3:(rows-202),]
tests_1<-tail(pd.data,2)[1,]
tests_2<-tail(pd.data,2)[2,]
#A:
trn1<-trains_1$n
trn2<-trains_2$n
a1.1<-trains_1$p.a11
a2.1<-trains_1$p.a22
a3.1<-trains_1$p.a33
a4.1<-trains_1$p.a44
a5.1<-trains_1$p.a55
a1.2<-trains_2$p.a11
a2.2<-trains_2$p.a22
a3.2<-trains_2$p.a33
a4.2<-trains_2$p.a44
a5.2<-trains_2$p.a55
resa1<-results$p.a11
resa2<-results$p.a22
resa3<-results$p.a33
resa4<-results$p.a44
resa5<-results$p.a55
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
#Build DRForestmodel
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
a1.1<-tests_1$p.a11
a2.1<-tests_1$p.a22
a3.1<-tests_1$p.a33
a4.1<-tests_1$p.a44
a5.1<-tests_1$p.a55
a1.2<-tests_2$p.a11
a2.2<-tests_2$p.a22
a3.2<-tests_2$p.a33
a4.2<-tests_2$p.a44
a5.2<-tests_2$p.a55
tests.a11<-data.frame(tsn1,tsn2,a1.2,a1.1)
tests.a22<-data.frame(tsn1,tsn2,a2.2,a2.1)
tests.a33<-data.frame(tsn1,tsn2,a3.2,a3.1)
tests.a44<-data.frame(tsn1,tsn2,a4.2,a4.1)
tests.a55<-data.frame(tsn1,tsn2,a5.2,a5.1)
#B:
b1.1<-tests_1$p.b11
b2.1<-tests_1$p.b22
b1.2<-tests_2$p.b11
b2.2<-tests_2$p.b22
tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
#SVM logidtic tesing suit
p.c.a11 = predict(fit_drf.a11,tests.a11)
p.c.a22 = predict(fit_drf.a22,tests.a22)
p.c.a33 = predict(fit_drf.a33,tests.a33)
p.c.a44 = predict(fit_drf.a44,tests.a44)
p.c.a55 = predict(fit_drf.a55,tests.a55)
p.c.b11 = predict(fit_drf.b11,tests.b11)
p.c.b22 = predict(fit_drf.b22,tests.b22)

p.c.resultII<-c(p.c.a11,p.c.a22,p.c.a33,p.c.a44,p.c.a55,p.c.b11,p.c.b22)
p.c.resultII
plot(p.c.resultII)

#################################################################################################

trains_1 <-tail(pd.data,rows-200)[1:(rows-204),]
trains_2 <-tail(pd.data,rows-200)[2:(rows-203),]
results<-tail(pd.data,rows-200)[3:(rows-202),]
tests_1<-tail(pd.data,2)[1,]
tests_2<-tail(pd.data,2)[2,]
#A:
trn1<-trains_1$n
trn2<-trains_2$n
a1.1<-trains_1$d.a11
a2.1<-trains_1$d.a22
a3.1<-trains_1$d.a33
a4.1<-trains_1$d.a44
a5.1<-trains_1$d.a55
a1.2<-trains_2$d.a11
a2.2<-trains_2$d.a22
a3.2<-trains_2$d.a33
a4.2<-trains_2$d.a44
a5.2<-trains_2$d.a55
resa1<-results$d.a11
resa2<-results$d.a22
resa3<-results$d.a33
resa4<-results$d.a44
resa5<-results$d.a55
trains.a1<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
trains.a2<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
trains.a3<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
trains.a4<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
trains.a5<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
#B:
b1.1<-trains_1$d.b11
b2.1<-trains_1$d.b22
b1.2<-trains_2$d.b11
b2.2<-trains_2$d.b22
resb1<-results$d.b11
resb2<-results$d.b22
trains.b1<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
trains.b2<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
#BuildDRForest model
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
a1.1<-tests_1$d.a11
a2.1<-tests_1$d.a22
a3.1<-tests_1$d.a33
a4.1<-tests_1$d.a44
a5.1<-tests_1$d.a55
a1.2<-tests_2$d.a11
a2.2<-tests_2$d.a22
a3.2<-tests_2$d.a33
a4.2<-tests_2$d.a44
a5.2<-tests_2$d.a55
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
#SVM predict testing suit
d.c.a11 = predict(fit_drf.a11,tests.a11)
d.c.a22 = predict(fit_drf.a22,tests.a22)
d.c.a33 = predict(fit_drf.a33,tests.a33)
d.c.a44 = predict(fit_drf.a44,tests.a44)
d.c.a55 = predict(fit_drf.a55,tests.a55)
d.c.b11 = predict(fit_drf.b11,tests.b11)
d.c.b22 = predict(fit_drf.b22,tests.b22)

d.c.resultII<-c(d.c.a11,d.c.a22,d.c.a33,d.c.a44,d.c.a55,d.c.b11,d.c.b22)
d.c.resultII
plot(d.c.resultII)


######################################################################################

p.c.resultII
d.c.resultII