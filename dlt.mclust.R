library(mclust)
#Build data
tests<-tail(dlt,1)
trains.a <-tail(dlt,201)[1:200,]
results.a<-tail(dlt,200)
#A:
trn<-trains$n
a1<-trains.a$a1
a2<-trains.a$a2
a3<-trains.a$a3
a4<-trains.a$a4
a5<-trains.a$a5
resa1<-results.a$a1
resa2<-results.a$a2
resa3<-results.a$a3
resa4<-results.a$a4
resa5<-results.a$a5
trains.a1<-data.frame(a1,a2,a3,a4,a5,resa1)
trains.a2<-data.frame(a1,a2,a3,a4,a5,resa2)
trains.a3<-data.frame(a1,a2,a3,a4,a5,resa3)
trains.a4<-data.frame(a1,a2,a3,a4,a5,resa4)
trains.a5<-data.frame(a1,a2,a3,a4,a5,resa5)
#B:
trains.b<-tail(dlt,126)[1:125,]
results.b<-tail(dlt,125)
b1<-trains.b$b1
b2<-trains.b$b2
resb1<-results.b$b1
resb2<-results.b$b2
trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)
#build gMclust model
fit_mc.a1<-Mclust(trains.a1[1:5],parallel = TRUE)
fit_mc.a2<-Mclust(trains.a2[1:5],parallel = TRUE)
fit_mc.a3<-Mclust(trains.a3[1:5],parallel = TRUE)
fit_mc.a4<-Mclust(trains.a4[1:5],parallel = TRUE)
fit_mc.a5<-Mclust(trains.a5[1:5],parallel = TRUE)
fit_mc.b1<-Mclust(trains.b1[1:2],parallel = TRUE)
fit_mc.b2<-Mclust(trains.b2[1:2],parallel = TRUE)
#summary(fit_mc.a1,parametwrs = TRUE)
#Mclustpredict test suit
p.a1<-predict(fit_mc.a1, tests[4:8])
p.a2<-predict(fit_mc.a2, tests[4:8])
p.a3<-predict(fit_mc.a3, tests[4:8])
p.a4<-predict(fit_mc.a4, tests[4:8])
p.a5<-predict(fit_mc.a5, tests[4:8])
p.b1<-predict(fit_mc.b1, tests[9:10])
p.b2<-predict(fit_mc.b2, tests[9:10])
p.a1<-table(fit_mc.a1$classification,trains.a1$resa1)[p.a1$classification,]
barplot(p.a1,main = "a1")
p.a2<-table(fit_mc.a2$classification,trains.a2$resa2)[p.a2$classification,]
barplot(p.a2,main = "a2")
p.a3<-table(fit_mc.a3$classification,trains.a3$resa3)[p.a3$classification,]
barplot(p.a3,main = "a3")
p.a4<-table(fit_mc.a4$classification,trains.a4$resa4)[p.a4$classification,]
barplot(p.a4,main = "a4")
p.a5<-table(fit_mc.a5$classification,trains.a5$resa5)[p.a5$classification,]
barplot(p.a5,main = "a5")
p.b1<-table(fit_mc.b1$classification,trains.b1$resb1)[p.b1$classification,]
barplot(p.b1,main = "b1")
p.b2<-table(fit_mc.b2$classification,trains.b2$resb2)[p.b2$classification,]
barplot(p.b2,main = "b2")

sort(p.a1,decreasing = TRUE)
sort(p.a2,decreasing = TRUE)
sort(p.a3,decreasing = TRUE)
sort(p.a4,decreasing = TRUE)
sort(p.a5,decreasing = TRUE)
sort(p.b1,decreasing = TRUE)
sort(p.b2,decreasing = TRUE)



