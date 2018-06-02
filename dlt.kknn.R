library(kknn) 
#Build data
tests<-tail(dlt,1)

trains.a <-tail(dlt,231)[1:230,]
results.a<-tail(dlt,230)

trn<-trains.a$n
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

trains.b<-tail(dlt,231)[1:230,]
results.b<-tail(dlt,230)

b1<-trains.b$b1
b2<-trains.b$b2

resb1<-results.b$b1
resb2<-results.b$b2

trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)


dlt_kknn.a1 <- kknn(resa1~a1+a2+a3+a4+a5,trains.a1,tests[4:8],distance = 5,  kernel= "triangular") 
p.a1 <- fitted(dlt_kknn.a1)
dlt_kknn.a2 <- kknn(resa2~a1+a2+a3+a4+a5,trains.a2,tests[4:8],distance = 5,  kernel= "triangular") 
p.a2 <- fitted(dlt_kknn.a2)
dlt_kknn.a3 <- kknn(resa3~a1+a2+a3+a4+a5,trains.a3,tests[4:8],distance = 5,  kernel= "triangular") 
p.a3 <- fitted(dlt_kknn.a3)
dlt_kknn.a4 <- kknn(resa4~a1+a2+a3+a4+a5,trains.a4,tests[4:8],distance = 5,  kernel= "triangular") 
p.a4 <- fitted(dlt_kknn.a4)
dlt_kknn.a5 <- kknn(resa5~a1+a2+a3+a4+a5,trains.a1,tests[4:8],distance = 5,  kernel= "triangular") 
p.a5 <- fitted(dlt_kknn.a5)
dlt_kknn.b1 <- kknn(resb1~b1+b2,trains.b1,tests[9:10],distance = 5,  kernel= "triangular") 
p.b1 <- fitted(dlt_kknn.b1)
dlt_kknn.b2 <- kknn(resb2~b1+b2,trains.b2,tests[9:10],distance = 5,  kernel= "triangular") 
p.b2 <- fitted(dlt_kknn.b2)

result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
result
plot(result)



