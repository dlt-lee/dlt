library(quantmod)
library(depmixS4)
library(TTR)
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
trains.a<-data.frame(a1,a2,a3,a4,a5,resa1,resa2,resa3,resa4,resa5)
#B:
trains.b<-tail(dlt,126)[1:125,]
results.b<-tail(dlt,125)
b1<-trains.b$b1
b2<-trains.b$b2
resb1<-results.b$b1
resb2<-results.b$b2
trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)
#Build DEPMIX model
mod_dm.a<-depmix(response = resa1~a1+a2+a3+a4+a5,data = trains.a1,nstates = 5,family = gaussian())
fit_dm.a<-fit(mod_dm.a, verbose = TRUE)
mod_dm.ta<-depmix(response = ~a1+a2+a3+a4+a5,
                  data = tests[4:8],nstates = 5,family = gaussian())
hmm2 <- setpars(mod_dm.ta, getpars(fit_dm.a))
viterbi(hmm2)
depmixS4::summary(fit_dm.a,which="transition")

