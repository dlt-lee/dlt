library(bnlearn)
library(Rgraphviz)
#Build matrix
ab.drf.m  <-round(matrix(ab.drf  ,ncol = 7,byrow = TRUE)[-1,])
ab.drfII.m<-round(matrix(ab.drfII,ncol = 7,byrow = TRUE)[-1,])
ab.bnf.m  <-round(matrix(ab.bnf  ,ncol = 7,byrow = TRUE)[-1,])
ab.bnfII.m<-round(matrix(ab.bnfII,ncol = 7,byrow = TRUE)[-1,])
ab.xgb.m  <-round(matrix(ab.xgb  ,ncol = 7,byrow = TRUE)[-1,])
ab.xgbII.m<-round(matrix(ab.xgbII,ncol = 7,byrow = TRUE)[-1,])
ab.glmf.m  <-round(matrix(ab.glmf  ,ncol = 7,byrow = TRUE)[-1,])
ab.glmfII.m<-round(matrix(ab.glmfII,ncol = 7,byrow = TRUE)[-1,])
ab.svmf.m  <-round(matrix(ab.svmf  ,ncol = 7,byrow = TRUE)[-1,])
ab.svmfII.m<-round(matrix(ab.svmfII,ncol = 7,byrow = TRUE)[-1,])
ab.knnf.m  <-round(matrix(ab.knnf  ,ncol = 7,byrow = TRUE)[-1,])
ab.knnfII.m<-round(matrix(ab.knnfII,ncol = 7,byrow = TRUE)[-1,])
ab.bnfAB.m<-round(matrix(ab.bnfAB,ncol = 7,byrow = TRUE)[-1,])
#Build data.frame
a1.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,1]
a2.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,2]
a3.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,3]
a4.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,4]
a5.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,5]
b1.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,6]
b2.drf  <-ab.drf.m  [1:(dim(ab.drf.m)[1]-1)  ,7]
a1.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),1]
a2.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),2]
a3.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),3]
a4.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),4]
a5.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),5]
b1.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),6]
b2.drfII<-ab.drfII.m[1:(dim(ab.drfII.m)[1]-1),7]
a1.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,1]
a2.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,2]
a3.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,3]
a4.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,4]
a5.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,5]
b1.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,6]
b2.bnf  <-ab.bnf.m  [1:(dim(ab.bnf.m)[1]-1)  ,7]
a1.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),1]
a2.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),2]
a3.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),3]
a4.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),4]
a5.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),5]
b1.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),6]
b2.bnfII<-ab.bnfII.m[1:(dim(ab.bnfII.m)[1]-1),7]
a1.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,1]
a2.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,2]
a3.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,3]
a4.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,4]
a5.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,5]
b1.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,6]
b2.xgb  <-ab.xgb.m  [1:(dim(ab.xgb.m)[1]-1)  ,7]
a1.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),1]
a2.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),2]
a3.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),3]
a4.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),4]
a5.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),5]
b1.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),6]
b2.xgbII<-ab.xgbII.m[1:(dim(ab.xgbII.m)[1]-1),7]
a1.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),1]
a2.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),2]
a3.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),3]
a4.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),4]
a5.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),5]
b1.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),6]
b2.bnfAB<-ab.bnfAB.m[1:(dim(ab.bnfAB.m)[1]-1),7]
a1.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,1]
a2.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,2]
a3.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,3]
a4.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,4]
a5.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,5]
b1.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,6]
b2.glmf <-ab.glmf.m[1:(dim(ab.glmf.m)[1]-1)  ,7]
a1.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),1]
a2.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),2]
a3.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),3]
a4.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),4]
a5.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),5]
b1.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),6]
b2.glmfII<-ab.glmfII.m[1:(dim(ab.glmfII.m)[1]-1),7]
a1.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,1]
a2.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,2]
a3.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,3]
a4.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,4]
a5.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,5]
b1.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,6]
b2.svmf <-ab.svmf.m[1:(dim(ab.svmf.m)[1]-1)  ,7]
a1.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),1]
a2.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),2]
a3.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),3]
a4.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),4]
a5.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),5]
b1.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),6]
b2.svmfII<-ab.svmfII.m[1:(dim(ab.svmfII.m)[1]-1),7]
a1.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,1]
a2.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,2]
a3.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,3]
a4.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,4]
a5.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,5]
b1.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,6]
b2.knnf <-ab.knnf.m[1:(dim(ab.knnf.m)[1]-1)  ,7]
a1.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),1]
a2.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),2]
a3.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),3]
a4.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),4]
a5.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),5]
b1.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),6]
b2.knnfII<-ab.knnfII.m[1:(dim(ab.knnfII.m)[1]-1),7]

result<-dlt[(s.n+1):row.ab,]
a1.r<-result[, 4]
a2.r<-result[, 5]
a3.r<-result[, 6]
a4.r<-result[, 7]
a5.r<-result[, 8]
b1.r<-result[, 9]
b2.r<-result[,10]

#Build Bayes train data
trains.a1<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      a1.r)
trains.a2<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      a2.r)
trains.a3<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      a3.r)
trains.a4<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      a4.r)
trains.a5<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      a5.r)
trains.b1<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      b1.r)
trains.b2<-data.frame(a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
                      a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
                      a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
                      a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
                      a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
                      a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
                      a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
                      b1.drf,   b2.drf,
                      b1.glmf,  b2.glmf,
                      b1.xgb,   b2.xgb,
                      b1.drfII, b2.drfII,
                      b1.glmfII,b2.glmfII,
                      b1.xgbII, b2.xgbII,
                      b1.bnfAB, b2.bnfAB,
                      b2.r)

#Bayes
#A1:
bn.a1<-bnlearn::hc(trains.a1)
bn.a1<-   set.arc(bn.a1,"a1.drf","a1.r")
bn.a1<-   set.arc(bn.a1,"a2.drf","a1.r")
bn.a1<-   set.arc(bn.a1,"a3.drf","a1.r")
bn.a1<-   set.arc(bn.a1,"a4.drf","a1.r")
bn.a1<-   set.arc(bn.a1,"a5.drf","a1.r")
bn.a1<-   set.arc(bn.a1,"b1.drf","a1.r")
bn.a1<-   set.arc(bn.a1,"b2.drf","a1.r")
bn.a1<- set.arc(bn.a1,"a1.drfII","a1.r")
bn.a1<- set.arc(bn.a1,"a2.drfII","a1.r")
bn.a1<- set.arc(bn.a1,"a3.drfII","a1.r")
bn.a1<- set.arc(bn.a1,"a4.drfII","a1.r")
bn.a1<- set.arc(bn.a1,"b1.drfII","a1.r")
bn.a1<- set.arc(bn.a1,"b2.drfII","a1.r")
bn.a1<-  set.arc(bn.a1,"a1.glmf","a1.r")
bn.a1<-  set.arc(bn.a1,"a2.glmf","a1.r")
bn.a1<-  set.arc(bn.a1,"a3.glmf","a1.r")
bn.a1<-  set.arc(bn.a1,"a4.glmf","a1.r")
bn.a1<-  set.arc(bn.a1,"a5.glmf","a1.r")
bn.a1<-  set.arc(bn.a1,"b1.glmf","a1.r")
bn.a1<-  set.arc(bn.a1,"b2.glmf","a1.r")
bn.a1<-set.arc(bn.a1,"a1.glmfII","a1.r")
bn.a1<-set.arc(bn.a1,"a2.glmfII","a1.r")
bn.a1<-set.arc(bn.a1,"a3.glmfII","a1.r")
bn.a1<-set.arc(bn.a1,"a4.glmfII","a1.r")
bn.a1<-set.arc(bn.a1,"a5.glmfII","a1.r")
bn.a1<-set.arc(bn.a1,"b1.glmfII","a1.r")
bn.a1<-set.arc(bn.a1,"b2.glmfII","a1.r")
bn.a1<-   set.arc(bn.a1,"a1.xgb","a1.r")
bn.a1<-   set.arc(bn.a1,"a2.xgb","a1.r")
bn.a1<-   set.arc(bn.a1,"a3.xgb","a1.r")
bn.a1<-   set.arc(bn.a1,"a4.xgb","a1.r")
bn.a1<-   set.arc(bn.a1,"a5.xgb","a1.r")
bn.a1<-   set.arc(bn.a1,"b1.xgb","a1.r")
bn.a1<-   set.arc(bn.a1,"b2.xgb","a1.r")
bn.a1<- set.arc(bn.a1,"a1.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"a2.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"a3.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"a4.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"a5.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"b1.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"b2.xgbII","a1.r")
bn.a1<- set.arc(bn.a1,"a1.bnfAB","a1.r")
bn.a1<- set.arc(bn.a1,"a2.bnfAB","a1.r")
bn.a1<- set.arc(bn.a1,"a3.bnfAB","a1.r")
bn.a1<- set.arc(bn.a1,"a4.bnfAB","a1.r")
bn.a1<- set.arc(bn.a1,"a5.bnfAB","a1.r")
bn.a1<- set.arc(bn.a1,"b1.bnfAB","a1.r")
bn.a1<- set.arc(bn.a1,"b2.bnfAB","a1.r")
graphviz.plot(bn.a1, layout = "fdp",main = "a1")
fit_bn.a1 <- bn.fit(bn.a1, data = trains.a1)
#A2:
bn.a2<-bnlearn::hc(trains.a2)
bn.a2<-   set.arc(bn.a2,"a1.drf","a2.r")
bn.a2<-   set.arc(bn.a2,"a2.drf","a2.r")
bn.a2<-   set.arc(bn.a2,"a3.drf","a2.r")
bn.a2<-   set.arc(bn.a2,"a4.drf","a2.r")
bn.a2<-   set.arc(bn.a2,"a5.drf","a2.r")
bn.a2<-   set.arc(bn.a2,"b1.drf","a2.r")
bn.a2<-   set.arc(bn.a2,"b2.drf","a2.r")
bn.a2<- set.arc(bn.a2,"a1.drfII","a2.r")
bn.a2<- set.arc(bn.a2,"a2.drfII","a2.r")
bn.a2<- set.arc(bn.a2,"a3.drfII","a2.r")
bn.a2<- set.arc(bn.a2,"a4.drfII","a2.r")
bn.a2<- set.arc(bn.a2,"b1.drfII","a2.r")
bn.a2<- set.arc(bn.a2,"b2.drfII","a2.r")
bn.a2<-  set.arc(bn.a2,"a1.glmf","a2.r")
bn.a2<-  set.arc(bn.a2,"a2.glmf","a2.r")
bn.a2<-  set.arc(bn.a2,"a3.glmf","a2.r")
bn.a2<-  set.arc(bn.a2,"a4.glmf","a2.r")
bn.a2<-  set.arc(bn.a2,"a5.glmf","a2.r")
bn.a2<-  set.arc(bn.a2,"b1.glmf","a2.r")
bn.a2<-  set.arc(bn.a2,"b2.glmf","a2.r")
bn.a2<-set.arc(bn.a2,"a1.glmfII","a2.r")
bn.a2<-set.arc(bn.a2,"a2.glmfII","a2.r")
bn.a2<-set.arc(bn.a2,"a3.glmfII","a2.r")
bn.a2<-set.arc(bn.a2,"a4.glmfII","a2.r")
bn.a2<-set.arc(bn.a2,"a5.glmfII","a2.r")
bn.a2<-set.arc(bn.a2,"b1.glmfII","a2.r")
bn.a2<-set.arc(bn.a2,"b2.glmfII","a2.r")
bn.a2<-   set.arc(bn.a2,"a1.xgb","a2.r")
bn.a2<-   set.arc(bn.a2,"a2.xgb","a2.r")
bn.a2<-   set.arc(bn.a2,"a3.xgb","a2.r")
bn.a2<-   set.arc(bn.a2,"a4.xgb","a2.r")
bn.a2<-   set.arc(bn.a2,"a5.xgb","a2.r")
bn.a2<-   set.arc(bn.a2,"b1.xgb","a2.r")
bn.a2<-   set.arc(bn.a2,"b2.xgb","a2.r")
bn.a2<- set.arc(bn.a2,"a1.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"a2.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"a3.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"a4.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"a5.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"b1.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"b2.xgbII","a2.r")
bn.a2<- set.arc(bn.a2,"a1.bnfAB","a2.r")
bn.a2<- set.arc(bn.a2,"a2.bnfAB","a2.r")
bn.a2<- set.arc(bn.a2,"a3.bnfAB","a2.r")
bn.a2<- set.arc(bn.a2,"a4.bnfAB","a2.r")
bn.a2<- set.arc(bn.a2,"a5.bnfAB","a2.r")
bn.a2<- set.arc(bn.a2,"b1.bnfAB","a2.r")
bn.a2<- set.arc(bn.a2,"b2.bnfAB","a2.r")
graphviz.plot(bn.a2, layout = "fdp",main = "a2")
fit_bn.a2 <- bn.fit(bn.a2, data = trains.a2)
#A3:
bn.a3<-bnlearn::hc(trains.a3)
bn.a3<-   set.arc(bn.a3,"a1.drf","a3.r")
bn.a3<-   set.arc(bn.a3,"a2.drf","a3.r")
bn.a3<-   set.arc(bn.a3,"a3.drf","a3.r")
bn.a3<-   set.arc(bn.a3,"a4.drf","a3.r")
bn.a3<-   set.arc(bn.a3,"a5.drf","a3.r")
bn.a3<-   set.arc(bn.a3,"b1.drf","a3.r")
bn.a3<-   set.arc(bn.a3,"b2.drf","a3.r")
bn.a3<- set.arc(bn.a3,"a1.drfII","a3.r")
bn.a3<- set.arc(bn.a3,"a2.drfII","a3.r")
bn.a3<- set.arc(bn.a3,"a3.drfII","a3.r")
bn.a3<- set.arc(bn.a3,"a4.drfII","a3.r")
bn.a3<- set.arc(bn.a3,"b1.drfII","a3.r")
bn.a3<- set.arc(bn.a3,"b2.drfII","a3.r")
bn.a3<-  set.arc(bn.a3,"a1.glmf","a3.r")
bn.a3<-  set.arc(bn.a3,"a2.glmf","a3.r")
bn.a3<-  set.arc(bn.a3,"a3.glmf","a3.r")
bn.a3<-  set.arc(bn.a3,"a4.glmf","a3.r")
bn.a3<-  set.arc(bn.a3,"a5.glmf","a3.r")
bn.a3<-  set.arc(bn.a3,"b1.glmf","a3.r")
bn.a3<-  set.arc(bn.a3,"b2.glmf","a3.r")
bn.a3<-set.arc(bn.a3,"a1.glmfII","a3.r")
bn.a3<-set.arc(bn.a3,"a2.glmfII","a3.r")
bn.a3<-set.arc(bn.a3,"a3.glmfII","a3.r")
bn.a3<-set.arc(bn.a3,"a4.glmfII","a3.r")
bn.a3<-set.arc(bn.a3,"a5.glmfII","a3.r")
bn.a3<-set.arc(bn.a3,"b1.glmfII","a3.r")
bn.a3<-set.arc(bn.a3,"b2.glmfII","a3.r")
bn.a3<-   set.arc(bn.a3,"a1.xgb","a3.r")
bn.a3<-   set.arc(bn.a3,"a2.xgb","a3.r")
bn.a3<-   set.arc(bn.a3,"a3.xgb","a3.r")
bn.a3<-   set.arc(bn.a3,"a4.xgb","a3.r")
bn.a3<-   set.arc(bn.a3,"a5.xgb","a3.r")
bn.a3<-   set.arc(bn.a3,"b1.xgb","a3.r")
bn.a3<-   set.arc(bn.a3,"b2.xgb","a3.r")
bn.a3<- set.arc(bn.a3,"a1.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"a2.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"a3.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"a4.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"a5.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"b1.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"b2.xgbII","a3.r")
bn.a3<- set.arc(bn.a3,"a1.bnfAB","a3.r")
bn.a3<- set.arc(bn.a3,"a2.bnfAB","a3.r")
bn.a3<- set.arc(bn.a3,"a3.bnfAB","a3.r")
bn.a3<- set.arc(bn.a3,"a4.bnfAB","a3.r")
bn.a3<- set.arc(bn.a3,"a5.bnfAB","a3.r")
bn.a3<- set.arc(bn.a3,"b1.bnfAB","a3.r")
bn.a3<- set.arc(bn.a3,"b2.bnfAB","a3.r")
graphviz.plot(bn.a3, layout = "fdp",main = "a3")
fit_bn.a3 <- bn.fit(bn.a3, data = trains.a3)
#A4:
bn.a4<-bnlearn::hc(trains.a4)
bn.a4<-   set.arc(bn.a4,"a1.drf","a4.r")
bn.a4<-   set.arc(bn.a4,"a2.drf","a4.r")
bn.a4<-   set.arc(bn.a4,"a3.drf","a4.r")
bn.a4<-   set.arc(bn.a4,"a4.drf","a4.r")
bn.a4<-   set.arc(bn.a4,"a5.drf","a4.r")
bn.a4<-   set.arc(bn.a4,"b1.drf","a4.r")
bn.a4<-   set.arc(bn.a4,"b2.drf","a4.r")
bn.a4<- set.arc(bn.a4,"a1.drfII","a4.r")
bn.a4<- set.arc(bn.a4,"a2.drfII","a4.r")
bn.a4<- set.arc(bn.a4,"a3.drfII","a4.r")
bn.a4<- set.arc(bn.a4,"a4.drfII","a4.r")
bn.a4<- set.arc(bn.a4,"b1.drfII","a4.r")
bn.a4<- set.arc(bn.a4,"b2.drfII","a4.r")
bn.a4<-  set.arc(bn.a4,"a1.glmf","a4.r")
bn.a4<-  set.arc(bn.a4,"a2.glmf","a4.r")
bn.a4<-  set.arc(bn.a4,"a3.glmf","a4.r")
bn.a4<-  set.arc(bn.a4,"a4.glmf","a4.r")
bn.a4<-  set.arc(bn.a4,"a5.glmf","a4.r")
bn.a4<-  set.arc(bn.a4,"b1.glmf","a4.r")
bn.a4<-  set.arc(bn.a4,"b2.glmf","a4.r")
bn.a4<-set.arc(bn.a4,"a1.glmfII","a4.r")
bn.a4<-set.arc(bn.a4,"a2.glmfII","a4.r")
bn.a4<-set.arc(bn.a4,"a3.glmfII","a4.r")
bn.a4<-set.arc(bn.a4,"a4.glmfII","a4.r")
bn.a4<-set.arc(bn.a4,"a5.glmfII","a4.r")
bn.a4<-set.arc(bn.a4,"b1.glmfII","a4.r")
bn.a4<-set.arc(bn.a4,"b2.glmfII","a4.r")
bn.a4<-   set.arc(bn.a4,"a1.xgb","a4.r")
bn.a4<-   set.arc(bn.a4,"a2.xgb","a4.r")
bn.a4<-   set.arc(bn.a4,"a3.xgb","a4.r")
bn.a4<-   set.arc(bn.a4,"a4.xgb","a4.r")
bn.a4<-   set.arc(bn.a4,"a5.xgb","a4.r")
bn.a4<-   set.arc(bn.a4,"b1.xgb","a4.r")
bn.a4<-   set.arc(bn.a4,"b2.xgb","a4.r")
bn.a4<- set.arc(bn.a4,"a1.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"a2.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"a3.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"a4.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"a5.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"b1.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"b2.xgbII","a4.r")
bn.a4<- set.arc(bn.a4,"a1.bnfAB","a4.r")
bn.a4<- set.arc(bn.a4,"a2.bnfAB","a4.r")
bn.a4<- set.arc(bn.a4,"a3.bnfAB","a4.r")
bn.a4<- set.arc(bn.a4,"a4.bnfAB","a4.r")
bn.a4<- set.arc(bn.a4,"a5.bnfAB","a4.r")
bn.a4<- set.arc(bn.a4,"b1.bnfAB","a4.r")
bn.a4<- set.arc(bn.a4,"b2.bnfAB","a4.r")
graphviz.plot(bn.a4, layout = "fdp",main = "a4")
fit_bn.a4 <- bn.fit(bn.a4, data = trains.a4)
#A5:
bn.a5<-bnlearn::hc(trains.a5)
bn.a5<-   set.arc(bn.a5,"a1.drf","a5.r")
bn.a5<-   set.arc(bn.a5,"a2.drf","a5.r")
bn.a5<-   set.arc(bn.a5,"a3.drf","a5.r")
bn.a5<-   set.arc(bn.a5,"a4.drf","a5.r")
bn.a5<-   set.arc(bn.a5,"a5.drf","a5.r")
bn.a5<-   set.arc(bn.a5,"b1.drf","a5.r")
bn.a5<-   set.arc(bn.a5,"b2.drf","a5.r")
bn.a5<- set.arc(bn.a5,"a1.drfII","a5.r")
bn.a5<- set.arc(bn.a5,"a2.drfII","a5.r")
bn.a5<- set.arc(bn.a5,"a3.drfII","a5.r")
bn.a5<- set.arc(bn.a5,"a4.drfII","a5.r")
bn.a5<- set.arc(bn.a5,"b1.drfII","a5.r")
bn.a5<- set.arc(bn.a5,"b2.drfII","a5.r")
bn.a5<-  set.arc(bn.a5,"a1.glmf","a5.r")
bn.a5<-  set.arc(bn.a5,"a2.glmf","a5.r")
bn.a5<-  set.arc(bn.a5,"a3.glmf","a5.r")
bn.a5<-  set.arc(bn.a5,"a4.glmf","a5.r")
bn.a5<-  set.arc(bn.a5,"a5.glmf","a5.r")
bn.a5<-  set.arc(bn.a5,"b1.glmf","a5.r")
bn.a5<-  set.arc(bn.a5,"b2.glmf","a5.r")
bn.a5<-set.arc(bn.a5,"a1.glmfII","a5.r")
bn.a5<-set.arc(bn.a5,"a2.glmfII","a5.r")
bn.a5<-set.arc(bn.a5,"a3.glmfII","a5.r")
bn.a5<-set.arc(bn.a5,"a4.glmfII","a5.r")
bn.a5<-set.arc(bn.a5,"a5.glmfII","a5.r")
bn.a5<-set.arc(bn.a5,"b1.glmfII","a5.r")
bn.a5<-set.arc(bn.a5,"b2.glmfII","a5.r")
bn.a5<-   set.arc(bn.a5,"a1.xgb","a5.r")
bn.a5<-   set.arc(bn.a5,"a2.xgb","a5.r")
bn.a5<-   set.arc(bn.a5,"a3.xgb","a5.r")
bn.a5<-   set.arc(bn.a5,"a4.xgb","a5.r")
bn.a5<-   set.arc(bn.a5,"a5.xgb","a5.r")
bn.a5<-   set.arc(bn.a5,"b1.xgb","a5.r")
bn.a5<-   set.arc(bn.a5,"b2.xgb","a5.r")
bn.a5<- set.arc(bn.a5,"a1.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"a2.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"a3.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"a4.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"a5.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"b1.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"b2.xgbII","a5.r")
bn.a5<- set.arc(bn.a5,"a1.bnfAB","a5.r")
bn.a5<- set.arc(bn.a5,"a2.bnfAB","a5.r")
bn.a5<- set.arc(bn.a5,"a3.bnfAB","a5.r")
bn.a5<- set.arc(bn.a5,"a4.bnfAB","a5.r")
bn.a5<- set.arc(bn.a5,"a5.bnfAB","a5.r")
bn.a5<- set.arc(bn.a5,"b1.bnfAB","a5.r")
bn.a5<- set.arc(bn.a5,"b2.bnfAB","a5.r")
graphviz.plot(bn.a5, layout = "fdp",main = "a5")
fit_bn.a5 <- bn.fit(bn.a5, data = trains.a5)
#B1:
bn.b1<-bnlearn::hc(trains.b1)
bn.b1<-   set.arc(bn.b1,"a1.drf","b1.r")
bn.b1<-   set.arc(bn.b1,"a2.drf","b1.r")
bn.b1<-   set.arc(bn.b1,"a3.drf","b1.r")
bn.b1<-   set.arc(bn.b1,"a4.drf","b1.r")
bn.b1<-   set.arc(bn.b1,"a5.drf","b1.r")
bn.b1<-   set.arc(bn.b1,"b1.drf","b1.r")
bn.b1<-   set.arc(bn.b1,"b2.drf","b1.r")
bn.b1<- set.arc(bn.b1,"a1.drfII","b1.r")
bn.b1<- set.arc(bn.b1,"a2.drfII","b1.r")
bn.b1<- set.arc(bn.b1,"a3.drfII","b1.r")
bn.b1<- set.arc(bn.b1,"a4.drfII","b1.r")
bn.b1<- set.arc(bn.b1,"b1.drfII","b1.r")
bn.b1<- set.arc(bn.b1,"b2.drfII","b1.r")
bn.b1<-  set.arc(bn.b1,"a1.glmf","b1.r")
bn.b1<-  set.arc(bn.b1,"a2.glmf","b1.r")
bn.b1<-  set.arc(bn.b1,"a3.glmf","b1.r")
bn.b1<-  set.arc(bn.b1,"a4.glmf","b1.r")
bn.b1<-  set.arc(bn.b1,"a5.glmf","b1.r")
bn.b1<-  set.arc(bn.b1,"b1.glmf","b1.r")
bn.b1<-  set.arc(bn.b1,"b2.glmf","b1.r")
bn.b1<-set.arc(bn.b1,"a1.glmfII","b1.r")
bn.b1<-set.arc(bn.b1,"a2.glmfII","b1.r")
bn.b1<-set.arc(bn.b1,"a3.glmfII","b1.r")
bn.b1<-set.arc(bn.b1,"a4.glmfII","b1.r")
bn.b1<-set.arc(bn.b1,"a5.glmfII","b1.r")
bn.b1<-set.arc(bn.b1,"b1.glmfII","b1.r")
bn.b1<-set.arc(bn.b1,"b2.glmfII","b1.r")
bn.b1<-   set.arc(bn.b1,"a1.xgb","b1.r")
bn.b1<-   set.arc(bn.b1,"a2.xgb","b1.r")
bn.b1<-   set.arc(bn.b1,"a3.xgb","b1.r")
bn.b1<-   set.arc(bn.b1,"a4.xgb","b1.r")
bn.b1<-   set.arc(bn.b1,"a5.xgb","b1.r")
bn.b1<-   set.arc(bn.b1,"b1.xgb","b1.r")
bn.b1<-   set.arc(bn.b1,"b2.xgb","b1.r")
bn.b1<- set.arc(bn.b1,"a1.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"a2.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"a3.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"a4.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"a5.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"b1.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"b2.xgbII","b1.r")
bn.b1<- set.arc(bn.b1,"a1.bnfAB","b1.r")
bn.b1<- set.arc(bn.b1,"a2.bnfAB","b1.r")
bn.b1<- set.arc(bn.b1,"a3.bnfAB","b1.r")
bn.b1<- set.arc(bn.b1,"a4.bnfAB","b1.r")
bn.b1<- set.arc(bn.b1,"a5.bnfAB","b1.r")
bn.b1<- set.arc(bn.b1,"b1.bnfAB","b1.r")
bn.b1<- set.arc(bn.b1,"b2.bnfAB","b1.r")
graphviz.plot(bn.b1, layout = "fdp",main = "b1")
fit_bn.b1 <- bn.fit(bn.b1, data = trains.b1)
#B2:
bn.b2<-bnlearn::hc(trains.b2)
bn.b2<-   set.arc(bn.b2,"a1.drf","b2.r")
bn.b2<-   set.arc(bn.b2,"a2.drf","b2.r")
bn.b2<-   set.arc(bn.b2,"a3.drf","b2.r")
bn.b2<-   set.arc(bn.b2,"a4.drf","b2.r")
bn.b2<-   set.arc(bn.b2,"a5.drf","b2.r")
bn.b2<-   set.arc(bn.b2,"b1.drf","b2.r")
bn.b2<-   set.arc(bn.b2,"b2.drf","b2.r")
bn.b2<- set.arc(bn.b2,"a1.drfII","b2.r")
bn.b2<- set.arc(bn.b2,"a2.drfII","b2.r")
bn.b2<- set.arc(bn.b2,"a3.drfII","b2.r")
bn.b2<- set.arc(bn.b2,"a4.drfII","b2.r")
bn.b2<- set.arc(bn.b2,"b1.drfII","b2.r")
bn.b2<- set.arc(bn.b2,"b2.drfII","b2.r")
bn.b2<-  set.arc(bn.b2,"a1.glmf","b2.r")
bn.b2<-  set.arc(bn.b2,"a2.glmf","b2.r")
bn.b2<-  set.arc(bn.b2,"a3.glmf","b2.r")
bn.b2<-  set.arc(bn.b2,"a4.glmf","b2.r")
bn.b2<-  set.arc(bn.b2,"a5.glmf","b2.r")
bn.b2<-  set.arc(bn.b2,"b1.glmf","b2.r")
bn.b2<-  set.arc(bn.b2,"b2.glmf","b2.r")
bn.b2<-set.arc(bn.b2,"a1.glmfII","b2.r")
bn.b2<-set.arc(bn.b2,"a2.glmfII","b2.r")
bn.b2<-set.arc(bn.b2,"a3.glmfII","b2.r")
bn.b2<-set.arc(bn.b2,"a4.glmfII","b2.r")
bn.b2<-set.arc(bn.b2,"a5.glmfII","b2.r")
bn.b2<-set.arc(bn.b2,"b1.glmfII","b2.r")
bn.b2<-set.arc(bn.b2,"b2.glmfII","b2.r")
bn.b2<-   set.arc(bn.b2,"a1.xgb","b2.r")
bn.b2<-   set.arc(bn.b2,"a2.xgb","b2.r")
bn.b2<-   set.arc(bn.b2,"a3.xgb","b2.r")
bn.b2<-   set.arc(bn.b2,"a4.xgb","b2.r")
bn.b2<-   set.arc(bn.b2,"a5.xgb","b2.r")
bn.b2<-   set.arc(bn.b2,"b1.xgb","b2.r")
bn.b2<-   set.arc(bn.b2,"b2.xgb","b2.r")
bn.b2<- set.arc(bn.b2,"a1.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"a2.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"a3.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"a4.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"a5.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"b1.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"b2.xgbII","b2.r")
bn.b2<- set.arc(bn.b2,"a1.bnfAB","b2.r")
bn.b2<- set.arc(bn.b2,"a2.bnfAB","b2.r")
bn.b2<- set.arc(bn.b2,"a3.bnfAB","b2.r")
bn.b2<- set.arc(bn.b2,"a4.bnfAB","b2.r")
bn.b2<- set.arc(bn.b2,"a5.bnfAB","b2.r")
bn.b2<- set.arc(bn.b2,"b1.bnfAB","b2.r")
bn.b2<- set.arc(bn.b2,"b2.bnfAB","b2.r")
graphviz.plot(bn.b2, layout = "fdp",main = "b2")
fit_bn.b2 <- bn.fit(bn.b2, data = trains.b2)


#Build predict data
a1.drf  <-tail(ab.drf.m,1)[  1]
a2.drf  <-tail(ab.drf.m,1)[  2]
a3.drf  <-tail(ab.drf.m,1)[  3]
a4.drf  <-tail(ab.drf.m,1)[  4]
a5.drf  <-tail(ab.drf.m,1)[  5]
b1.drf  <-tail(ab.drf.m,1)[  6]
b2.drf  <-tail(ab.drf.m,1)[  7]
a1.drfII<-tail(ab.drfII.m,1)[1]
a2.drfII<-tail(ab.drfII.m,1)[2]
a3.drfII<-tail(ab.drfII.m,1)[3]
a4.drfII<-tail(ab.drfII.m,1)[4]
a5.drfII<-tail(ab.drfII.m,1)[5]
b1.drfII<-tail(ab.drfII.m,1)[6]
b2.drfII<-tail(ab.drfII.m,1)[7]
a1.bnf  <-tail(ab.bnf.m,1)[  1]
a2.bnf  <-tail(ab.bnf.m,1)[  2]
a3.bnf  <-tail(ab.bnf.m,1)[  3]
a4.bnf  <-tail(ab.bnf.m,1)[  4]
a5.bnf  <-tail(ab.bnf.m,1)[  5]
b1.bnf  <-tail(ab.bnf.m,1)[  6]
b2.bnf  <-tail(ab.bnf.m,1)[  7]
a1.bnfII<-tail(ab.bnfII.m,1)[1]
a2.bnfII<-tail(ab.bnfII.m,1)[2]
a3.bnfII<-tail(ab.bnfII.m,1)[3]
a4.bnfII<-tail(ab.bnfII.m,1)[4]
a5.bnfII<-tail(ab.bnfII.m,1)[5]
b1.bnfII<-tail(ab.bnfII.m,1)[6]
b2.bnfII<-tail(ab.bnfII.m,1)[7]
a1.xgb  <-tail(ab.xgb.m,1)[  1]
a2.xgb  <-tail(ab.xgb.m,1)[  2]
a3.xgb  <-tail(ab.xgb.m,1)[  3]
a4.xgb  <-tail(ab.xgb.m,1)[  4]
a5.xgb  <-tail(ab.xgb.m,1)[  5]
b1.xgb  <-tail(ab.xgb.m,1)[  6]
b2.xgb  <-tail(ab.xgb.m,1)[  7]
a1.xgbII<-tail(ab.xgbII.m,1)[1]
a2.xgbII<-tail(ab.xgbII.m,1)[2]
a3.xgbII<-tail(ab.xgbII.m,1)[3]
a4.xgbII<-tail(ab.xgbII.m,1)[4]
a5.xgbII<-tail(ab.xgbII.m,1)[5]
b1.xgbII<-tail(ab.xgbII.m,1)[6]
b2.xgbII<-tail(ab.xgbII.m,1)[7]
a1.bnfAB<-tail(ab.bnfAB.m,1)[1]
a2.bnfAB<-tail(ab.bnfAB.m,1)[2]
a3.bnfAB<-tail(ab.bnfAB.m,1)[3]
a4.bnfAB<-tail(ab.bnfAB.m,1)[4]
a5.bnfAB<-tail(ab.bnfAB.m,1)[5]
b1.bnfAB<-tail(ab.bnfAB.m,1)[6]
b2.bnfAB<-tail(ab.bnfAB.m,1)[7]
a1.glmf  <-tail(ab.glmf.m,1)[1]
a2.glmf  <-tail(ab.glmf.m,1)[2]
a3.glmf  <-tail(ab.glmf.m,1)[3]
a4.glmf  <-tail(ab.glmf.m,1)[4]
a5.glmf  <-tail(ab.glmf.m,1)[5]
b1.glmf  <-tail(ab.glmf.m,1)[6]
b2.glmf  <-tail(ab.glmf.m,1)[7]
a1.glmfII<-tail(ab.glmfII.m,1)[1]
a2.glmfII<-tail(ab.glmfII.m,1)[2]
a3.glmfII<-tail(ab.glmfII.m,1)[3]
a4.glmfII<-tail(ab.glmfII.m,1)[4]
a5.glmfII<-tail(ab.glmfII.m,1)[5]
b1.glmfII<-tail(ab.glmfII.m,1)[6]
b2.glmfII<-tail(ab.glmfII.m,1)[7]
a1.svmf  <-tail(ab.svmf.m,1)[1]
a2.svmf  <-tail(ab.svmf.m,1)[2]
a3.svmf  <-tail(ab.svmf.m,1)[3]
a4.svmf  <-tail(ab.svmf.m,1)[4]
a5.svmf  <-tail(ab.svmf.m,1)[5]
b1.svmf  <-tail(ab.svmf.m,1)[6]
b2.svmf  <-tail(ab.svmf.m,1)[7]
a1.svmfII<-tail(ab.svmfII.m,1)[1]
a2.svmfII<-tail(ab.svmfII.m,1)[2]
a3.svmfII<-tail(ab.svmfII.m,1)[3]
a4.svmfII<-tail(ab.svmfII.m,1)[4]
a5.svmfII<-tail(ab.svmfII.m,1)[5]
b1.svmfII<-tail(ab.svmfII.m,1)[6]
b2.svmfII<-tail(ab.svmfII.m,1)[7]
a1.knnf  <-tail(ab.knnf.m,1)[1]
a2.knnf  <-tail(ab.knnf.m,1)[2]
a3.knnf  <-tail(ab.knnf.m,1)[3]
a4.knnf  <-tail(ab.knnf.m,1)[4]
a5.knnf  <-tail(ab.knnf.m,1)[5]
b1.knnf  <-tail(ab.knnf.m,1)[6]
b2.knnf  <-tail(ab.knnf.m,1)[7]
a1.knnfII<-tail(ab.knnfII.m,1)[1]
a2.knnfII<-tail(ab.knnfII.m,1)[2]
a3.knnfII<-tail(ab.knnfII.m,1)[3]
a4.knnfII<-tail(ab.knnfII.m,1)[4]
a5.knnfII<-tail(ab.knnfII.m,1)[5]
b1.knnfII<-tail(ab.knnfII.m,1)[6]
b2.knnfII<-tail(ab.knnfII.m,1)[7]

test   <-data.frame(
  a1.drf,   a2.drf,   a3.drf,   a4.drf,   a5.drf,
  a1.glmf,  a2.glmf,  a3.glmf,  a4.glmf,  a5.glmf,
  a1.xgb,   a2.xgb,   a3.xgb,   a4.xgb,   a5.xgb,
  a1.drfII, a2.drfII, a3.drfII, a4.drfII, a5.drfII,
  a1.glmfII,a2.glmfII,a3.glmfII,a4.glmfII,a5.glmfII,
  a1.xgbII, a2.xgbII, a3.xgbII, a4.xgbII, a5.xgbII,
  a1.bnfAB, a2.bnfAB, a3.bnfAB, a4.bnfAB, a5.bnfAB,
  b1.drf,   b2.drf,
  b1.glmf,  b2.glmf,
  b1.xgb,   b2.xgb,
  b1.drfII, b2.drfII,
  b1.glmfII,b2.glmfII,
  b1.xgbII, b2.xgbII,
  b1.bnfAB, b2.bnfAB)

p.bn.a1 <-predict(fit_bn.a1,test,node = "a1.r")
p.bn.a2 <-predict(fit_bn.a2,test,node = "a2.r")
p.bn.a3 <-predict(fit_bn.a3,test,node = "a3.r")
p.bn.a4 <-predict(fit_bn.a4,test,node = "a4.r")
p.bn.a5 <-predict(fit_bn.a5,test,node = "a5.r")
p.bn.b1 <-predict(fit_bn.b1,test,node = "b1.r")
p.bn.b2 <-predict(fit_bn.b2,test,node = "b2.r")

result<-c(p.bn.a1,p.bn.a2,p.bn.a3,p.bn.a4,p.bn.a5,p.bn.b1,p.bn.b2)

result



