library(bnlearn)
library(Rgraphviz)
trains_1 <-tail(dlt,302)[1:300,]
trains_2 <-tail(dlt,302)[2:301,]
results<-tail(dlt,300)
tests_1<-tail(dlt,2)[1,]
tests_2<-tail(dlt,2)[2,]
#A:
trn1<-trains_1$n
trn2<-trains_2$n
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
resa1<-results$a1
resa2<-results$a2
resa3<-results$a3
resa4<-results$a4
resa5<-results$a5
trains.a1<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
trains.a2<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
trains.a3<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
trains.a4<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
trains.a5<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
#B:
b1.1<-trains_1$b1
b2.1<-trains_1$b2
b1.2<-trains_2$b1
b2.2<-trains_2$b2
resb1<-results$b1
resb2<-results$b2
trains.b1<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
trains.b2<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
#BN logistic trainingsuit
#A1:
bn.a11<-bnlearn::hc(trains.a1)
bn.a11<-set.arc(bn.a11,"a1.1","resa1")
bn.a11<-set.arc(bn.a11,"a1.2","resa1")
#plot(bn.a11)
#graphviz.plot(bn.a11, layout = "fdp")
fit_bn.a11 <- bn.fit(bn.a11, data = trains.a1)
#A2:
bn.a22<-bnlearn::hc(trains.a2)
bn.a22<-set.arc(bn.a22,"a2.1","resa2")
bn.a22<-set.arc(bn.a22,"a2.2","resa2")
#plot(bn.a22)
#graphviz.plot(bn.a22, layout = "fdp")
fit_bn.a22 <- bn.fit(bn.a22, data = trains.a2)
#A3:
bn.a33<-bnlearn::hc(trains.a3)
bn.a33<-set.arc(bn.a33,"a3.1","resa3")
bn.a33<-set.arc(bn.a33,"a3.2","resa3")
plot(bn.a33)
graphviz.plot(bn.a33, layout = "fdp")
fit_bn.a33 <- bn.fit(bn.a33, data = trains.a3)
#A4:
bn.a44<-bnlearn::hc(trains.a4)
bn.a44<-set.arc(bn.a44,"a4.1","resa4")
bn.a44<-set.arc(bn.a44,"a4.2","resa4")
plot(bn.a44)
graphviz.plot(bn.a44, layout = "fdp")
fit_bn.a44 <- bn.fit(bn.a44, data = trains.a4)
#A5:
bn.a55<-bnlearn::hc(trains.a5)
bn.a55<-set.arc(bn.a55,"a5.1","resa5")
bn.a55<-set.arc(bn.a55,"a5.2","resa5")
plot(bn.a55)
graphviz.plot(bn.a55, layout = "fdp")
fit_bn.a55 <- bn.fit(bn.a55, data = trains.a5)
#B1:
bn.b11<-bnlearn::hc(trains.b1)
bn.b11<-set.arc(bn.b11,"b1.1","resb1")
bn.b11<-set.arc(bn.b11,"b1.2","resb1")
plot(bn.b11)
graphviz.plot(bn.b11, layout = "fdp")
fit_bn.b11 <- bn.fit(bn.b11, data = trains.b1)
#B2:
bn.b22<-bnlearn::hc(trains.b2)
bn.b22<-set.arc(bn.b22,"b2.1","resb2")
bn.b22<-set.arc(bn.b22,"b2.2","resb2")
#plot(bn.b22)
#graphviz.plot(bn.b22, layout = "fdp")
fit_bn.b22 <- bn.fit(bn.b22, data = trains.b2)

#Build test data
#A:
tsn1<-tests_1$n
tsn2<-tests_2$n
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
tests.a11<-data.frame(tsn1,tsn2,a1.2,a1.1)
tests.a22<-data.frame(tsn1,tsn2,a2.2,a2.1)
tests.a33<-data.frame(tsn1,tsn2,a3.2,a3.1)
tests.a44<-data.frame(tsn1,tsn2,a4.2,a4.1)
tests.a55<-data.frame(tsn1,tsn2,a5.2,a5.1)
#B:
b1.1<-tests_1$b1
b2.1<-tests_1$a2
b1.2<-tests_2$b1
b2.2<-tests_2$b2
tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
#bn logistic testsuit
p.a11 <- predict(object = fit_bn.a11,tests.a11[3:4],node = "resa1")
p.a22 <- predict(object = fit_bn.a22,tests.a22[3:4],node = "resa2")
p.a33 <- predict(object = fit_bn.a33,tests.a33[3:4],node = "resa3")
p.a44 <- predict(object = fit_bn.a44,tests.a44[3:4],node = "resa4")
p.a55 <- predict(object = fit_bn.a55,tests.a55[3:4],node = "resa5")
p.b11 <- predict(object = fit_bn.b11,tests.b11[3:4],node = "resb1")
p.b22 <- predict(object = fit_bn.b22,tests.b22[3:4],node = "resb2")


result<-c(p.a11,p.a22,p.a33,p.a44,p.a55,p.b11,p.b22)
result
plot(result)


