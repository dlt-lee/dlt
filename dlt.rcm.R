library(recommenderlab)
#Build data
tests<-tail(dlt,1)

trains.a <-tail(dlt,251)[1:250,]
results.a<-tail(dlt,250)

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


trains.b <-tail(dlt,126)[1:125,]
results.b<-tail(dlt,125)

b1<-trains.b$b1
b2<-trains.b$b2

resb1<-results.b$b1
resb2<-results.b$b2

trains.a1<-data.frame(a1,a2,a3,a4,a5,resa1)
trains.a2<-data.frame(a1,a2,a3,a4,a5,resa2)
trains.a3<-data.frame(a1,a2,a3,a4,a5,resa3)
trains.a4<-data.frame(a1,a2,a3,a4,a5,resa4)
trains.a5<-data.frame(a1,a2,a3,a4,a5,resa5)

trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)
#Convert
a.real<-as(trains.a[,4:8], "realRatingMatrix")
#as.matrix(rating)
#as(a.real, "matrix")
#a.real<-evaluationScheme(a.real, given=10,method = "split", train = trains.a1$resa1)
t.real<-as(tests[,4:8], "realRatingMatrix")
#build RCM model
model.popular <- Recommender(a.real, method = "POPULAR")
#GRCM predict test suit
predict.popular <- predict(model.popular, t.real,tpye="ratings")



