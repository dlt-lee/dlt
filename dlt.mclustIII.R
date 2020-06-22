library(mclust)
#Build data
trains_1 <-tail(dlt,203)[3:202,]
trains_2 <-tail(dlt,203)[2:201,]
trains_3 <-tail(dlt,203)[1:200,]
results<-tail(dlt,200)
tests_1<-tail(dlt,3)[3,]
tests_2<-tail(dlt,3)[2,]
tests_3<-tail(dlt,3)[1,]
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
trains.a111<-data.frame(trn1,trn2,trn3,a1.3,a1.2,a1.1,resa1)
trains.a222<-data.frame(trn1,trn2,trn3,a2.3,a2.2,a2.1,resa2)
trains.a333<-data.frame(trn1,trn2,trn3,a3.3,a3.2,a3.1,resa3)
trains.a444<-data.frame(trn1,trn2,trn3,a4.3,a4.2,a4.1,resa4)
trains.a555<-data.frame(trn1,trn2,trn3,a5.3,a5.2,a5.1,resa5)
#B:
b1.1<-trains_1$b1
b2.1<-trains_1$b2
b1.2<-trains_2$b1
b2.2<-trains_2$b2
b1.3<-trains_3$b1
b2.3<-trains_3$b2
resb1<-results$b1
resb2<-results$b2
trains.b111<-data.frame(trn1,trn2,trn3,b1.3,b1.2,b1.1,resb1)
trains.b222<-data.frame(trn1,trn2,trn3,b2.3,b2.2,b2.1,resb2)
#build gMclust model
fit_mc.a111<-Mclust(trains.a111[,4:6],parallel = TRUE)
fit_mc.a222<-Mclust(trains.a222[,4:6],parallel = TRUE)
fit_mc.a333<-Mclust(trains.a333[,4:6],parallel = TRUE)
fit_mc.a444<-Mclust(trains.a444[,4:6],parallel = TRUE)
fit_mc.a555<-Mclust(trains.a555[,4:6],parallel = TRUE)
fit_mc.b111<-Mclust(trains.b111[,4:6],parallel = TRUE)
fit_mc.b222<-Mclust(trains.b222[,4:6],parallel = TRUE)
#Build test data
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
tests.a111<-data.frame(tsn1,tsn2,tsn3,a1.3,a1.2,a1.1)
tests.a222<-data.frame(tsn1,tsn2,tsn3,a2.3,a2.2,a2.1)
tests.a333<-data.frame(tsn1,tsn2,tsn3,a3.3,a3.2,a3.1)
tests.a444<-data.frame(tsn1,tsn2,tsn3,a4.3,a4.2,a4.1)
tests.a555<-data.frame(tsn1,tsn2,tsn3,a5.3,a5.2,a5.1)
#B:
b1.1<-tests_1$b1
b2.1<-tests_1$a2
b1.2<-tests_2$b1
b2.2<-tests_2$b2
b1.3<-tests_3$b1
b2.3<-tests_3$b2
tests.b111<-data.frame(tsn1,tsn2,tsn3,b1.3,b1.2,b1.1)
tests.b222<-data.frame(tsn1,tsn2,tsn3,b2.3,b2.2,b2.1)
#Mclust predict test suit
p.a111<-predict(fit_mc.a111, tests.a111[4:6])
p.a222<-predict(fit_mc.a222, tests.a222[4:6])
p.a333<-predict(fit_mc.a333, tests.a333[4:6])
p.a444<-predict(fit_mc.a444, tests.a444[4:6])
p.a555<-predict(fit_mc.a555, tests.a555[4:6])
p.b111<-predict(fit_mc.b111, tests.b111[4:6])
p.b222<-predict(fit_mc.b222, tests.b222[4:6])
p.a111<-table(fit_mc.a111$classification,trains.a111$resa1)[p.a111$classification,]
barplot(p.a111,main = "a111")
p.a222<-table(fit_mc.a222$classification,trains.a222$resa2)[p.a222$classification,]
barplot(p.a222,main = "a222")
p.a333<-table(fit_mc.a333$classification,trains.a333$resa3)[p.a333$classification,]
barplot(p.a333,main = "a333")
p.a444<-table(fit_mc.a444$classification,trains.a444$resa4)[p.a444$classification,]
barplot(p.a444,main = "a444")
p.a555<-table(fit_mc.a555$classification,trains.a555$resa5)[p.a555$classification,]
barplot(p.a555,main = "a555")
p.b111<-table(fit_mc.b111$classification,trains.b111$resb1)[p.b111$classification,]
barplot(p.b111,main = "b111")
p.b222<-table(fit_mc.b222$classification,trains.b222$resb2)[p.b222$classification,]
barplot(p.b222,main = "b222")

sort(p.a111,decreasing = TRUE)
sort(p.a222,decreasing = TRUE)
sort(p.a333,decreasing = TRUE)
sort(p.a444,decreasing = TRUE)
sort(p.a555,decreasing = TRUE)
sort(p.b111,decreasing = TRUE)
sort(p.b222,decreasing = TRUE)



