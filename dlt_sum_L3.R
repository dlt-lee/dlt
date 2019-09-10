library(HMM)
library(depmixS4)
library(matlab)
library(quantmod)
library(xgboost)

#rows<-dim(dlt)[1]
#line<-rows-98
#n<-c(0,0,0,0,0,0,0,0)

#for (i in 1:50) {
#  start<-line-609
#  data<-dlt[start:line,]
#  temp<-dlt_sum_L2(data)
#  n<-c(n,(dlt[line,]$n+1),temp)
#  start<-start+1
#  line<-line+1
#}
#n_m<-matrix(n,ncol = 8,byrow = TRUE)

cell<-c(
          18150,07,15,16,21,33,07,10,
          18151,03,10,19,19,33,04,05,
          18152,05,18,22,26,31,05,09,
          18153,03,14,15,26,30,08,09,
          18154,05,07,18,21,30,05,07,
          18155,07,11,24,25,28,05,09,
          19002,03,09,14,29,33,04,05,
          19003,04,07,25,26,27,01,09,
          19004,02,08,20,22,32,03,08,
          19005,04,18,20,20,23,03,08,
          19006,03,10,19,23,33,01,09,
          19007,06,13,20,22,28,05,11,
          19008,02,11,15,26,33,05,11,
          19009,07,08,19,26,31,05,10,
          19010,09,11,20,25,31,08,10,
          19011,07,09,18,27,31,05,11,
          19012,06,09,19,22,30,02,12,
          19013,04,10,22,22,26,07,10,
          19014,04,19,20,24,33,04,11,
          19015,03,08,17,20,30,01,11,
          19016,05,13,14,21,28,04,10,
          19017,05,06,08,25,30,03,08,
          19018,09,09,27,27,29,04,08,
          19019,01,08,15,21,30,02,04,
          19020,14,14,17,23,34,04,07,
          19021,04,14,22,23,32,04,09,
          19022,04,04,15,27,28,03,05,
          19023,03,11,20,21,25,03,10,
          19024,02,13,16,16,23,06,06,
          19025,05,11,23,26,29,03,10,
          19026,01,10,21,25,27,02,07,
          19027,10,12,14,21,26,04,08,
          19028,06,07,10,25,27,05,09,
          19029,07,14,19,23,31,04,09,
          19030,07,18,19,27,31,04,09,
          19031,04,12,16,21,30,02,11,
          19032,08,21,21,25,30,04,07,
          19033,06,10,17,20,31,05,05,
          19034,04,10,23,26,34,03,10,
          19035,04,11,13,25,33,03,07,
          19036,06,14,15,21,33,03,09,
          19037,06,11,12,14,28,05,06,
          19038,10,18,24,25,30,08,09,
          19039,03,12,18,25,30,04,06,
          19040,00,11,22,22,25,03,09,
          19041,06,08,11,14,32,05,09,
          19042,05,21,23,25,25,01,07,
          19043,09,11,16,21,30,05,12,
          19044,02,13,16,24,31,03,10,
          19045,06,09,22,25,31,04,09,
          19046,04,10,14,18,30,02,10,
          19047,06,10,13,20,31,06,09,
          19048,03,09,13,23,30,05,09,
          19049,04,07,09,18,30,01,08,
          19050,03,09,15,25,26,04,06,
          19051,04,11,20,24,31,03,04,
          19052,06,07,21,24,27,02,11,
          19053,01,14,19,23,33,02,08,
          19054,03,13,22,27,27,05,09,
          19055,06,08,18,28,30,05,08,
          19056,04,08,20,25,30,04,07,
          19057,05,09,21,28,31,05,11,
          19058,06,07,11,25,32,02,09,
          19059,10,10,16,26,33,04,11, 
          19060,08,14,21,26,29,02,09,
          19061,11,14,17,24,25,03,06,
          19062,06,11,13,25,27,04,07,
          19063,05,07,16,19,26,06,09,
          19064,05,09,15,21,27,05,10,
          19065,06,08,20,28,28,-1,10,
          19066,05,11,18,20,27,05,08,
          19067,05,09,25,28,30,04,10,
          19068,10,14,19,26,28,04,09,
          19069,04,06,14,25,32,04,04,
          19070,05,11,15,17,33,05,09,
          19071,05,15,15,22,34,05,08,
          19072,02,14,21,28,31,00,10,
          19073,08,13,15,24,26,04,07,
          19074,06,13,23,23,23,03,10,
          19075,02,11,17,22,30,06,10,
          19076,11,14,17,18,32,03,12,
          19077,04,15,19,22,31,01,10,
          19078,02,16,23,20,31,02,08,
          19079,03,14,17,24,32,05,07,
          19080,07,12,24,24,30,04,10,
          19081,03,07,19,21,30,04,09,
          19082,07,09,15,27,31,04,07,
          19083,05,12,15,25,28,03,07,
          19084,07,11,19,22,31,04,07,
          19085,04,09,11,13,24,02,10,
          19086,05,16,20,22,23,08,09,
          19087,05,08,12,25,29,05,09,
          19088,05,12,23,25,28,03,09,
          19089,07,12,18,20,35,05,06,
          19090,15,16,16,18,30,01,11,
          19091,04,13,16,25,33,04,07,
          19092,04,06,12,15,30,05,10,
          19093,05,09,14,25,32,07,09,
          19094,05,08,18,24,31,05,11,
          19095,03,06,22,30,33,02,09,
          19096,04,18,18,26,33,03,08,
          19097,03,11,21,26,33,04,09,
          19098,13,15,17,19,22,03,04,
          19099,05,14,24,26,32,03,10,
          19100,02,04,14,24,27,06,09,
          19101,02,12,21,24,29,05,12,
          19102,02,13,13,19,26,03,07,
          19103,05,12,13,25,27,05,07,
          19104,02,11,18,25,30,02,07,
          19105,09,13,22,25,32,05,11,
          19106,10,11,20,23,29,01,10
          
             )
ab_m<-matrix(cell,ncol = 8,byrow = TRUE)

a1.pre<-ab_m[,2]
a2.pre<-ab_m[,3]
a3.pre<-ab_m[,4]
a4.pre<-ab_m[,5]
a5.pre<-ab_m[,6]
b1.pre<-ab_m[,7]
b2.pre<-ab_m[,8]

a1.res<-tail(dlt,dim(ab_m)[1])$a1
a2.res<-tail(dlt,dim(ab_m)[1])$a2
a3.res<-tail(dlt,dim(ab_m)[1])$a3
a4.res<-tail(dlt,dim(ab_m)[1])$a4
a5.res<-tail(dlt,dim(ab_m)[1])$a5
b1.res<-tail(dlt,dim(ab_m)[1])$b1
b2.res<-tail(dlt,dim(ab_m)[1])$b2

a1.hmm<-data.frame(a1.pre,a1.res)
a2.hmm<-data.frame(a2.pre,a2.res)
a3.hmm<-data.frame(a3.pre,a3.res)
a4.hmm<-data.frame(a4.pre,a4.res)
a5.hmm<-data.frame(a5.pre,a5.res)
b1.hmm<-data.frame(a1.pre,b1.res)
b2.hmm<-data.frame(a2.pre,b2.res)


#Training
trains.T.ab<-Matrix(ab_m[,2:8],sparse=T)
bst.a1<-xgboost(data = trains.T.ab[,1:5],label = tail(dlt,dim(ab_m)[1])$a1,nrounds = 300,print_every_n = 300L)
bst.a2<-xgboost(data = trains.T.ab[,1:5],label = tail(dlt,dim(ab_m)[1])$a2,nrounds = 300,print_every_n = 300L)
bst.a3<-xgboost(data = trains.T.ab[,1:5],label = tail(dlt,dim(ab_m)[1])$a3,nrounds = 300,print_every_n = 300L)
bst.a4<-xgboost(data = trains.T.ab[,1:5],label = tail(dlt,dim(ab_m)[1])$a4,nrounds = 300,print_every_n = 300L)
bst.a5<-xgboost(data = trains.T.ab[,1:5],label = tail(dlt,dim(ab_m)[1])$a5,nrounds = 300,print_every_n = 300L)
bst.b1<-xgboost(data = trains.T.ab[,6:7],label = tail(dlt,dim(ab_m)[1])$b1,nrounds = 300,print_every_n = 300L)
bst.b2<-xgboost(data = trains.T.ab[,6:7],label = tail(dlt,dim(ab_m)[1])$b2,nrounds = 300,print_every_n = 300L)

#data preparation
pre.data<-dlt_sum_L2(tail(dlt,609))
pre.data<-c(sort(pre.data[1:5]),sort(pre.data[6:7]))

#predict
pre.T.data<-Matrix(pre.data,sparse=T)
testPredictions.a1 <- predict(object = bst.a1,newdata = t(pre.T.data[1:5]))
testPredictions.a2 <- predict(object = bst.a2,newdata = t(pre.T.data[1:5]))
testPredictions.a3 <- predict(object = bst.a3,newdata = t(pre.T.data[1:5]))
testPredictions.a4 <- predict(object = bst.a4,newdata = t(pre.T.data[1:5]))
testPredictions.a5 <- predict(object = bst.a5,newdata = t(pre.T.data[1:5]))
testPredictions.b1 <- predict(object = bst.a1,newdata = t(pre.T.data[6:7]))
testPredictions.b2 <- predict(object = bst.a2,newdata = t(pre.T.data[6:7]))

sum_l3_ab<-c(round(tail(testPredictions.a1,1)),
             round(tail(testPredictions.a2,1)),
             round(tail(testPredictions.a3,1)),
             round(tail(testPredictions.a4,1)),
             round(tail(testPredictions.a5,1)),
             round(tail(testPredictions.b1,1)),
             round(tail(testPredictions.b2,1)))

sum_l3_ab<-c(sort(sum_l3_ab[1:5]),sort(sum_l3_ab[6:7]))
sum_l2_ab<-dlt_sum_L2(dlt)
sum_l1_ab<-dlt_sum_L1(dlt)

sum_l2_ab
sum_l3_ab

sort(table(a1.hmm[which(a1.pre==sum_l3_ab[1]),]$a1.res))
sort(table(a2.hmm[which(a2.pre==sum_l3_ab[2]),]$a2.res))
sort(table(a3.hmm[which(a3.pre==sum_l3_ab[3]),]$a3.res))
sort(table(a4.hmm[which(a4.pre==sum_l3_ab[4]),]$a4.res))
sort(table(a5.hmm[which(a5.pre==sum_l3_ab[5]),]$a5.res))
sort(table(b1.hmm[which(b1.pre==sum_l3_ab[6]),]$b1.res))
sort(table(b2.hmm[which(b2.pre==sum_l3_ab[7]),]$b2.res))

pre.data










