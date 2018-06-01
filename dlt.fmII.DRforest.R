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
trains <-tail(pd.data,rows-200)[1:(rows-202),]
results<-tail(pd.data,(rows-201))
tests<-tail(pd.data,1)
#A:
testset.a<-tests[,2:6]
#A1:
r.a11<-results[,2]
trainset.a11<-cbind(trains[,2:6],r.a11)
rf.a11 = randomForest(r.a11 ~ p.a11+p.a22+p.a33+p.a44+p.a55,data = trainset.a11,importance = T)
p.r.a11 = predict(rf.a11,testset.a)
#A2:
r.a22<-results[,3]
trainset.a22<-cbind(trains[,2:6],r.a22)
rf.a22 = randomForest(r.a22 ~ p.a11+p.a22+p.a33+p.a44+p.a55,data = trainset.a22,importance = T)
p.r.a22 = predict(rf.a22,testset.a)
#A3:
r.a33<-results[,4]
trainset.a33<-cbind(trains[,2:6],r.a33)
rf.a33 = randomForest(r.a33 ~ p.a11+p.a22+p.a33+p.a44+p.a55,data = trainset.a33,importance = T)
p.r.a33 = predict(rf.a33,testset.a)
#A4:
r.a44<-results[,5]
trainset.a44<-cbind(trains[,2:6],r.a44)
rf.a44 = randomForest(r.a44 ~ p.a11+p.a22+p.a33+p.a44+p.a55,data = trainset.a44,importance = T)
p.r.a44 = predict(rf.a44,testset.a)
#A5:
r.a55<-results[,6]
trainset.a55<-cbind(trains[,2:6],r.a55)
rf.a55 = randomForest(r.a55 ~ p.a11+p.a22+p.a33+p.a44+p.a55,data = trainset.a55,importance = T)
p.r.a55 = predict(rf.a55,testset.a)

trains.b <-tail(pd.data,rows-200)[1:(rows-202),]
results.b<-tail(pd.data,(rows-201))
tests<-tail(pd.data,1)
#B:
testset.b<-tests[,7:8]
#B1:
r.b11<-results.b[,7]
trainset.b11<-cbind(trains.b[,7:8],r.b11)
rf.b11 = randomForest(r.b11 ~ p.b11+p.b22,data = trainset.b11,importance = T)
p.r.b11 = predict(rf.b11,testset.b)
#B2:
r.b22<-results.b[,8]
trainset.b22<-cbind(trains.b[,7:8],r.b22)
rf.b22 = randomForest(r.b22 ~ p.b11+p.b22,data = trainset.b22,importance = T)
p.r.b22 = predict(rf.b22,testset.b)

p.r.resultII<-c(p.r.a11,p.r.a22,p.r.a33,p.r.a44,p.r.a55,p.r.b11,p.r.b22)
p.r.resultII
plot(p.r.resultII)
#########################################################################################
trains <-tail(pd.data,rows-200)[1:(rows-202),]
results<-tail(pd.data,(rows-201))
tests<-tail(pd.data,1)
#A:
testset.a<-tests[,9:13]
#A1:
r.a11<-results[,9]
trainset.a11<-cbind(trains[,9:13],r.a11)
rf.a11 = randomForest(r.a11 ~ d.a11+d.a22+d.a33+d.a44+d.a55,data = trainset.a11,importance = T)
d.r.a11 = predict(rf.a11,testset.a)
#A2:
r.a22<-results[,10]
trainset.a22<-cbind(trains[,9:13],r.a22)
rf.a22 = randomForest(r.a22 ~ d.a11+d.a22+d.a33+d.a44+d.a55,data = trainset.a22,importance = T)
d.r.a22 = predict(rf.a22,testset.a)
#A3:
r.a33<-results[,11]
trainset.a33<-cbind(trains[,9:13],r.a33)
rf.a33 = randomForest(r.a33 ~ d.a11+d.a22+d.a33+d.a44+d.a55,data = trainset.a33,importance = T)
d.r.a33 = predict(rf.a33,testset.a)
#A4:
r.a44<-results[,12]
trainset.a44<-cbind(trains[,9:13],r.a44)
rf.a44 = randomForest(r.a44 ~ d.a11+d.a22+d.a33+d.a44+d.a55,data = trainset.a44,importance = T)
d.r.a44 = predict(rf.a44,testset.a)
#A5:
r.a55<-results[,13]
trainset.a55<-cbind(trains[,9:13],r.a55)
rf.a55 = randomForest(r.a55 ~ d.a11+d.a22+d.a33+d.a44+d.a55,data = trainset.a55,importance = T)
d.r.a55 = predict(rf.a55,testset.a)

trains.b <-tail(pd.data,rows-200)[1:(rows-202),]
results.b<-tail(pd.data,(rows-201))
tests<-tail(pd.data,1)
#B:
testset.b<-tests[,14:15]
#B1:
r.b11<-results.b[,14]
trainset.b11<-cbind(trains.b[,14:15],r.b11)
rf.b11 = randomForest(r.b11 ~ d.b11+d.b22,data = trainset.b11,importance = T)
d.r.b11 = predict(rf.b11,testset.b)
#B2:
r.b22<-results.b[,15]
trainset.b22<-cbind(trains.b[,14:15],r.b22)
rf.b22 = randomForest(r.b22 ~ d.b11+d.b22,data = trainset.b22,importance = T)
d.r.b22 = predict(rf.b22,testset.b)

d.r.resultII<-c(d.r.a11,d.r.a22,d.r.a33,d.r.a44,d.r.a55,d.r.b11,d.r.b22)
d.r.resultII
plot(d.r.resultII)

########################################################################################

p.r.resultII
d.r.resultII
