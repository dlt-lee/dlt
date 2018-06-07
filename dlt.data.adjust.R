rows<-dim(dlt)[1]

data.o.a1<-dlt[c(-1,-2,-3),]$a1
data.o.a2<-dlt[c(-1,-2,-3),]$a2
data.o.a3<-dlt[c(-1,-2,-3),]$a3
data.o.a4<-dlt[c(-1,-2,-3),]$a4
data.o.a5<-dlt[c(-1,-2,-3),]$a5
data.o.b1<-dlt[c(-1,-2,-3),]$b1
data.o.b2<-dlt[c(-1,-2,-3),]$b2

data.p.a1<-floor(head(testPredictions.a1,rows-3))
data.p.a2<-floor(head(testPredictions.a2,rows-3))
data.p.a3<-floor(head(testPredictions.a3,rows-3))
data.p.a4<-floor(head(testPredictions.a4,rows-3))
data.p.a5<-floor(head(testPredictions.a5,rows-3))
data.p.b1<-floor(head(testPredictions.b1,rows-3))
data.p.b2<-floor(head(testPredictions.b2,rows-3))

delta.a1<-data.o.a1-data.p.a1
delta.a2<-data.o.a2-data.p.a2
delta.a3<-data.o.a3-data.p.a3
delta.a4<-data.o.a4-data.p.a4
delta.a5<-data.o.a5-data.p.a5
delta.b1<-data.o.b1-data.p.b1
delta.b2<-data.o.b2-data.p.b2

barplot(tail(delta.a1,40),main = "Delta.a1")
barplot(tail(delta.a2,40),main = "Delta.a2")
barplot(tail(delta.a3,40),main = "Delta.a3")
barplot(tail(delta.a4,40),main = "Delta.a4")
barplot(tail(delta.a5,40),main = "Delta.a5")
barplot(tail(delta.b1,40),main = "Delta.b1")
barplot(tail(delta.b2,40),main = "Delta.b2")
