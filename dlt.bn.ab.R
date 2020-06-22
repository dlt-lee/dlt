library(bnlearn)
library(Rgraphviz)
#Build data
tests<-tail(dlt,1)
trains <-tail(dlt,333)[1:332,]
results<-tail(dlt,332)

trn<-trains$n
a1<-trains$a1
a2<-trains$a2
a3<-trains$a3
a4<-trains$a4
a5<-trains$a5
b1<-trains$b1
b2<-trains$b2

resa1<-results$a1
resa2<-results$a2
resa3<-results$a3
resa4<-results$a4
resa5<-results$a5
resb1<-results$b1
resb2<-results$b2

trains.a1<-data.frame(a1,a2,a3,a4,a5,b1,b2,resa1)
trains.a2<-data.frame(a1,a2,a3,a4,a5,b1,b2,resa2)
trains.a3<-data.frame(a1,a2,a3,a4,a5,b1,b2,resa3)
trains.a4<-data.frame(a1,a2,a3,a4,a5,b1,b2,resa4)
trains.a5<-data.frame(a1,a2,a3,a4,a5,b1,b2,resa5)
trains.b1<-data.frame(a1,a2,a3,a4,a5,b1,b2,resb1)
trains.b2<-data.frame(a1,a2,a3,a4,a5,b1,b2,resb2)
#A1:
bn.a1<-bnlearn::hc(trains.a1)
bn.a1<-set.arc(bn.a1,"a5","resa1")
bn.a1<-set.arc(bn.a1,"b2","resa1")
graphviz.plot(bn.a1, layout = "fdp")
fit_bn.a1 <- bn.fit(bn.a1, data = trains.a1)
p.a1 <-predict(fit_bn.a1,tests[4:10],node = "resa1")
#A2:
bn.a2<-bnlearn::hc(trains.a2)
bn.a2<-set.arc(bn.a2,"a5","resa2")
bn.a2<-set.arc(bn.a2,"b2","resa2")
graphviz.plot(bn.a2, layout = "fdp")
fit_bn.a2 <- bn.fit(bn.a2, data = trains.a2)
p.a2 <-predict(fit_bn.a2,tests[4:10],node = "resa2")
#A3:
bn.a3<-bnlearn::hc(trains.a3)
bn.a3<-set.arc(bn.a3,"a5","resa3")
bn.a3<-set.arc(bn.a3,"b2","resa3")
graphviz.plot(bn.a3, layout = "fdp")
fit_bn.a3 <- bn.fit(bn.a3, data = trains.a3)
p.a3 <-predict(fit_bn.a3,tests[4:10],node = "resa3")
#A4:
bn.a4<-bnlearn::hc(trains.a4)
bn.a4<-set.arc(bn.a4,"a5","resa4")
bn.a4<-set.arc(bn.a4,"b2","resa4")
graphviz.plot(bn.a4, layout = "fdp")
fit_bn.a4 <- bn.fit(bn.a4, data = trains.a4)
p.a4 <-predict(fit_bn.a4,tests[4:10],node = "resa4")
#A5:
bn.a5<-bnlearn::hc(trains.a5)
bn.a5<-drop.arc(bn.a5,"resa5","a2")
bn.a5<-set.arc(bn.a5,"a5","resa5")
graphviz.plot(bn.a5, layout = "fdp")
bn.a5<-set.arc(bn.a5,"b2","resa5")
graphviz.plot(bn.a5, layout = "fdp")
fit_bn.a5 <- bn.fit(bn.a5, data = trains.a5)
p.a5 <-predict(fit_bn.a5,tests[4:10],node = "resa5")
#B1:
bn.b1<-bnlearn::hc(trains.b1)
bn.b1<-set.arc(bn.b1,"a5","resb1")
bn.b1<-set.arc(bn.b1,"b2","resb1")
graphviz.plot(bn.b1, layout = "fdp")
fit_bn.b1 <- bn.fit(bn.b1, data = trains.b1)
p.b1 <-predict(fit_bn.b1,tests[4:10],node = "resb1")
#B2:
bn.b2<-bnlearn::hc(trains.b2)
bn.b2<-set.arc(bn.b2,"a5","resb2")
bn.b2<-set.arc(bn.b2,"b2","resb2")
graphviz.plot(bn.b2, layout = "fdp")
fit_bn.b2 <- bn.fit(bn.b2, data = trains.b2)
p.b2 <-predict(fit_bn.b2,tests[4:10],node = "resb2")

result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
result
barplot(result)





