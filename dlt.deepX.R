row.ab<-dim(dlt)[1]
clu.ab<-dim(dlt)[2]
ab.drf   <-c(0,0,0,0,0,0,0)
ab.drfII <-c(0,0,0,0,0,0,0)
ab.bnf   <-c(0,0,0,0,0,0,0)
ab.bnfII <-c(0,0,0,0,0,0,0)
ab.xgb   <-c(0,0,0,0,0,0,0)
ab.xgbII <-c(0,0,0,0,0,0,0)
ab.bnfAB <-c(0,0,0,0,0,0,0)
ab.glmf  <-c(0,0,0,0,0,0,0)
ab.glmfII<-c(0,0,0,0,0,0,0)
ab.svmf  <-c(0,0,0,0,0,0,0)
ab.svmfII<-c(0,0,0,0,0,0,0)
ab.knnf  <-c(0,0,0,0,0,0,0)
ab.knnfII<-c(0,0,0,0,0,0,0)
s.n<-floor(row.ab/2)
for(i in s.n:row.ab) {
  data<-head(dlt,i)
  print("#######################################################################################")
  print(c(i,row.ab))
  count<-s.n
  #Randomforest
  t.drf   <-dlt.DRF(data,count)
  ab.drf  <-c(ab.drf,t.drf)
  t.drfII <-dlt.DRFII(data,count)
  ab.drfII<-c(ab.drfII,t.drfII)
  
  #Bayess
  t.bnf   <-dlt.BNF(data,count)
  ab.bnf  <-c(ab.bnf,t.bnf)
  t.bnfII <-dlt.BNFII(data,count)
  ab.bnfII<-c(ab.bnfII,t.bnfII)
  
  #xgBoost
  t.xgb   <-dlt.XGB(data,count)
  ab.xgb  <-c(ab.xgb,t.xgb)
  t.xgbII <-dlt.XGBII(data,count)
  ab.xgbII<-c(ab.xgbII,t.xgbII)
  
  #GLM
  t.glmf   <-dlt.GLMF(data,count)
  ab.glmf  <-c(ab.glmf,t.glmf)
  t.glmfII <-dlt.GLMFII(data,count)
  ab.glmfII<-c(ab.glmfII,t.glmfII)
  
  #SVM
  t.svmf   <-dlt.SVMF(data,count)
  ab.svmf  <-c(ab.svmf,t.svmf)
  t.svmfII <-dlt.SVMFII(data,count)
  ab.svmfII<-c(ab.svmfII,t.svmfII)
  
  #KNN
  t.knnf   <-dlt.KNNF(data,count)
  ab.knnf  <-c(ab.knnf,t.knnf)
  t.knnfII <-dlt.KNNFII(data,count)
  ab.knnfII<-c(ab.knnfII,t.knnfII)
  
  #Bayess AB
  t.bnfAB<-dlt.BNab(data,count)
  ab.bnfAB<-c(ab.bnfAB,t.bnfAB)
  
}
#Build matrix
ab.drf.m  <-matrix(ab.drf  ,ncol = 7,byrow = TRUE)[-1,]
ab.drfII.m<-matrix(ab.drfII,ncol = 7,byrow = TRUE)[-1,]
ab.bnf.m  <-matrix(ab.bnf  ,ncol = 7,byrow = TRUE)[-1,]
ab.bnfII.m<-matrix(ab.bnfII,ncol = 7,byrow = TRUE)[-1,]
ab.xgb.m  <-matrix(ab.xgb  ,ncol = 7,byrow = TRUE)[-1,]
ab.xgbII.m<-matrix(ab.xgbII,ncol = 7,byrow = TRUE)[-1,]
ab.glmf.m  <-matrix(ab.glmf  ,ncol = 7,byrow = TRUE)[-1,]
ab.glmfII.m<-matrix(ab.glmfII,ncol = 7,byrow = TRUE)[-1,]
ab.svmf.m  <-matrix(ab.svmf  ,ncol = 7,byrow = TRUE)[-1,]
ab.svmfII.m<-matrix(ab.svmfII,ncol = 7,byrow = TRUE)[-1,]
ab.knnf.m  <-matrix(ab.knnf  ,ncol = 7,byrow = TRUE)[-1,]
ab.knnfII.m<-matrix(ab.knnfII,ncol = 7,byrow = TRUE)[-1,]
ab.bnfAB.m<-matrix(ab.bnfAB,ncol = 7,byrow = TRUE)[-1,]
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

#Build training data
train.a1<-data.frame(a1.drf,a1.drfII,a1.xgb,a1.xgbII,
                     a1.bnf,a1.bnfII,a1.svmf,a1.svmfII,a1.knnf,a1.knnfII,
                     #                     a1.glmf,a1.glmfII,
                     a1.bnfAB,a1.r)
train.a2<-data.frame(
  a2.drf,a2.drfII,a2.xgb,a2.xgbII,
  a2.bnf,a2.bnfII,a2.svmf,a2.svmfII,a2.knnf,a2.knnfII,
  #                     a2.glmf,a2.glmfII,
  a2.bnfAB,a2.r)
train.a3<-data.frame(
  a3.drf,a3.drfII,a3.xgb,a3.xgbII,
  a3.bnf,a3.bnfII,a3.svmf,a3.svmfII,a3.knnf,a3.knnfII,
  #                     a3.glmf,a3.glmfII,
  a3.bnfAB,a3.r)
train.a4<-data.frame(a4.drf,a4.drfII,a4.xgb,a4.xgbII,
                     a4.bnf,a4.bnfII,a4.svmf,a4.svmfII,a4.knnf,a4.knnfII,
                     #                    a4.glmf,a4.glmfII,
                     a4.bnfAB,a4.r)
train.a5<-data.frame(a5.drf,a5.drfII,a5.xgb,a5.xgbII,
                     a5.bnf,a5.bnfII,a5.svmf,a5.svmfII,a5.knnf,a5.knnfII,
                     #                     a5.glmf,a5.glmfII,
                     a5.bnfAB,a5.r)
train.b1<-data.frame(b1.drf,b1.drfII,b1.xgb,b1.xgbII,
                     b1.bnf,b1.bnfII,b1.svmf,b1.svmfII,b1.knnf,b1.knnfII,
                     #                     b1.glmf,b1.glmfII,
                     b1.bnfAB,b1.r)
train.b2<-data.frame(b2.drf,b2.drfII,b2.xgb,b2.xgbII,
                     b2.bnf,b2.bnfII,b2.svmf,b2.svmfII,b2.knnf,b2.knnfII,
                     #                     b2.glmf,b2.glmfII,
                     b2.bnfAB,b2.r)

#randomForest training
rf.a1 = randomForest(a1.r ~ a1.drf+a1.drfII+a1.xgb+a1.xgbII
                     +a1.bnf+a1.bnfII+a1.svmf+a1.svmfII+a1.knnf+a1.knnfII
                     #                     +a1.glmf+a1.glmfII
                     +a1.bnfAB,data = train.a1,importance = T,ntrees=1000)
rf.a2 = randomForest(a2.r ~ a2.drf+a2.drfII+a2.xgb+a2.xgbII
                     +a2.bnf+a2.bnfII+a1.svmf+a2.svmfII+a2.knnf+a2.knnfII
                     #                     +a2.glmf+a2.glmfII
                     +a2.bnfAB,data = train.a2,importance = T,ntrees=1000)
rf.a3 = randomForest(a3.r ~ a3.drf+a3.drfII+a3.xgb+a3.xgbII
                     +a3.bnf+a3.bnfII+a1.svmf+a3.svmfII+a3.knnf+a3.knnfII
                     #                     +a3.glmf+a3.glmfII
                     +a3.bnfAB,data = train.a3,importance = T,ntrees=1000)
rf.a4 = randomForest(a4.r ~ a4.drf+a4.drfII+a4.xgb+a4.xgbII
                     +a4.bnf+a4.bnfII+a1.svmf+a4.svmfII+a4.knnf+a4.knnfII
                     #                     +a4.glmf+a4.glmfII
                     +a4.bnfAB,data = train.a4,importance = T,ntrees=1000)
rf.a5 = randomForest(a5.r ~ a5.drf+a5.drfII+a5.xgb+a5.xgbII
                     +a5.bnf+a5.bnfII+a1.svmf+a5.svmfII+a5.knnf+a5.knnfII
                     #                     +a5.glmf+a5.glmfII
                     +a5.bnfAB,data = train.a5,importance = T,ntrees=1000)
rf.b1 = randomForest(b1.r ~ b1.drf+b1.drfII+b1.xgb+b1.xgbII
                     +b1.bnf+b1.bnfII+b1.svmf+b1.svmfII+b1.knnf+b1.knnfII
                     #                     +b1.glmf+b1.glmfII
                     +b1.bnfAB,data = train.b1,importance = T,ntrees=1000)
rf.b2 = randomForest(b2.r ~ b2.drf+b2.drfII+b2.xgb+b2.xgbII
                     +b2.bnf+b2.bnfII+b1.svmf+b2.svmfII+b2.knnf+b2.knnfII
                     #                     +b2.glmf+b2.glmfII
                     +b2.bnfAB,data = train.b2,importance = T,ntrees=1000)

#Bayes training
#A1:
bn.a1<-bnlearn::hc(train.a1)
graphviz.plot(bn.a1, layout = "fdp")
#A2:
bn.a2<-bnlearn::hc(train.a2)
graphviz.plot(bn.a2, layout = "fdp")

#xgboost training
#xgb.a1<-Matrix(as.matrix(train.a1[,1:13]),sparse=T)
#xgb.a2<-Matrix(as.matrix(train.a2[,1:13]),sparse=T)
#xgb.a3<-Matrix(as.matrix(train.a3[,1:13]),sparse=T)
#xgb.a4<-Matrix(as.matrix(train.a4[,1:13]),sparse=T)
#xgb.a5<-Matrix(as.matrix(train.a5[,1:13]),sparse=T)
#xgb.b1<-Matrix(as.matrix(train.b1[,1:13]),sparse=T)
#xgb.b2<-Matrix(as.matrix(train.b2[,1:13]),sparse=T)

xgb.a1<-Matrix(as.matrix(train.a1[,1:11]),sparse=T)
xgb.a2<-Matrix(as.matrix(train.a2[,1:11]),sparse=T)
xgb.a3<-Matrix(as.matrix(train.a3[,1:11]),sparse=T)
xgb.a4<-Matrix(as.matrix(train.a4[,1:11]),sparse=T)
xgb.a5<-Matrix(as.matrix(train.a5[,1:11]),sparse=T)
xgb.b1<-Matrix(as.matrix(train.b1[,1:11]),sparse=T)
xgb.b2<-Matrix(as.matrix(train.b2[,1:11]),sparse=T)
n=300
bst.a1 <- xgboost(data = xgb.a1,label = train.a1$a1.r,nrounds = n,print_every_n = 300L)
bst.a2 <- xgboost(data = xgb.a2,label = train.a2$a2.r,nrounds = n,print_every_n = 300L)
bst.a3 <- xgboost(data = xgb.a3,label = train.a3$a3.r,nrounds = n,print_every_n = 300L)
bst.a4 <- xgboost(data = xgb.a4,label = train.a4$a4.r,nrounds = n,print_every_n = 300L)
bst.a5 <- xgboost(data = xgb.a5,label = train.a5$a5.r,nrounds = n,print_every_n = 300L)
bst.b1 <- xgboost(data = xgb.b1,label = train.b1$b1.r,nrounds = n,print_every_n = 300L)
bst.b2 <- xgboost(data = xgb.b2,label = train.b2$b2.r,nrounds = n,print_every_n = 300L)

#GLM training
#fit_glm.a1 = glmnet(as.matrix(train.a1[,1:13]), train.a1$a1.r, family="gaussian", nlambda=50, alpha=1)
#fit_glm.a2 = glmnet(as.matrix(train.a2[,1:13]), train.a2$a2.r, family="gaussian", nlambda=50, alpha=1)
#fit_glm.a3 = glmnet(as.matrix(train.a3[,1:13]), train.a3$a3.r, family="gaussian", nlambda=50, alpha=1)
#fit_glm.a4 = glmnet(as.matrix(train.a4[,1:13]), train.a4$a4.r, family="gaussian", nlambda=50, alpha=1)
#fit_glm.a5 = glmnet(as.matrix(train.a5[,1:13]), train.a5$a5.r, family="gaussian", nlambda=50, alpha=1)
#fit_glm.b1 = glmnet(as.matrix(train.b1[,1:13]), train.b1$b1.r, family="gaussian", nlambda=50, alpha=1)
#fit_glm.b2 = glmnet(as.matrix(train.b2[,1:13]), train.b1$b1.r, family="gaussian", nlambda=50, alpha=1)

#cvfit_glm.a1 = cv.glmnet(as.matrix(train.a1[,1:13]), train.a1$a1.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
#cvfit_glm.a2 = cv.glmnet(as.matrix(train.a2[,1:13]), train.a2$a2.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
#cvfit_glm.a3 = cv.glmnet(as.matrix(train.a3[,1:13]), train.a3$a3.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
#cvfit_glm.a4 = cv.glmnet(as.matrix(train.a4[,1:13]), train.a4$a4.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
#cvfit_glm.a5 = cv.glmnet(as.matrix(train.a5[,1:13]), train.a5$a5.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
#cvfit_glm.b1 = cv.glmnet(as.matrix(train.b1[,1:13]), train.b1$b1.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
#cvfit_glm.b2 = cv.glmnet(as.matrix(train.b2[,1:13]), train.b2$b2.r, family = "gaussian", type.measure = "mse",parallel = TRUE)

fit_glm.a1 = glmnet(as.matrix(train.a1[,1:11]), train.a1$a1.r, family="gaussian", nlambda=50, alpha=1)
fit_glm.a2 = glmnet(as.matrix(train.a2[,1:11]), train.a2$a2.r, family="gaussian", nlambda=50, alpha=1)
fit_glm.a3 = glmnet(as.matrix(train.a3[,1:11]), train.a3$a3.r, family="gaussian", nlambda=50, alpha=1)
fit_glm.a4 = glmnet(as.matrix(train.a4[,1:11]), train.a4$a4.r, family="gaussian", nlambda=50, alpha=1)
fit_glm.a5 = glmnet(as.matrix(train.a5[,1:11]), train.a5$a5.r, family="gaussian", nlambda=50, alpha=1)
fit_glm.b1 = glmnet(as.matrix(train.b1[,1:11]), train.b1$b1.r, family="gaussian", nlambda=50, alpha=1)
fit_glm.b2 = glmnet(as.matrix(train.b2[,1:11]), train.b1$b1.r, family="gaussian", nlambda=50, alpha=1)

cvfit_glm.a1 = cv.glmnet(as.matrix(train.a1[,1:11]), train.a1$a1.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a2 = cv.glmnet(as.matrix(train.a2[,1:11]), train.a2$a2.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a3 = cv.glmnet(as.matrix(train.a3[,1:11]), train.a3$a3.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a4 = cv.glmnet(as.matrix(train.a4[,1:11]), train.a4$a4.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.a5 = cv.glmnet(as.matrix(train.a5[,1:11]), train.a5$a5.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.b1 = cv.glmnet(as.matrix(train.b1[,1:11]), train.b1$b1.r, family = "gaussian", type.measure = "mse",parallel = TRUE)
cvfit_glm.b2 = cv.glmnet(as.matrix(train.b2[,1:11]), train.b2$b2.r, family = "gaussian", type.measure = "mse",parallel = TRUE)

#KNN
#Build KNN model
#fit_knn.a1 <- knnreg(train.a1[,1:13], train.a1$a1.r, k = 10)
#fit_knn.a2 <- knnreg(train.a2[,1:13], train.a2$a2.r, k = 10)
#fit_knn.a3 <- knnreg(train.a3[,1:13], train.a3$a3.r, k = 10)
#fit_knn.a4 <- knnreg(train.a4[,1:13], train.a4$a4.r, k = 10)
#fit_knn.a5 <- knnreg(train.a5[,1:13], train.a5$a5.r, k = 10)
#fit_knn.b1 <- knnreg(train.b1[,1:13], train.b1$b1.r, k = 10)
#fit_knn.b2 <- knnreg(train.b2[,1:13], train.b2$b2.r, k = 10)

fit_knn.a1 <- knnreg(train.a1[,1:11], train.a1$a1.r, k = 10)
fit_knn.a2 <- knnreg(train.a2[,1:11], train.a2$a2.r, k = 10)
fit_knn.a3 <- knnreg(train.a3[,1:11], train.a3$a3.r, k = 10)
fit_knn.a4 <- knnreg(train.a4[,1:11], train.a4$a4.r, k = 10)
fit_knn.a5 <- knnreg(train.a5[,1:11], train.a5$a5.r, k = 10)
fit_knn.b1 <- knnreg(train.b1[,1:11], train.b1$b1.r, k = 10)
fit_knn.b2 <- knnreg(train.b2[,1:11], train.b2$b2.r, k = 10)

#SVM training
fit_svm.a1=svm(a1.r ~ a1.drf+a1.drfII+a1.xgb+a1.xgbII
               +a1.bnf+a1.bnfII+a1.svmf+a1.svmfII+a1.knnf+a1.knnfII
               #               +a1.glmf+a1.glmfII
               +a1.bnfAB,data = train.a1)
fit_svm.a2 = svm(a2.r ~ a2.drf+a2.drfII+a2.xgb+a2.xgbII
                 +a2.bnf+a2.bnfII+a1.svmf+a2.svmfII+a2.knnf+a2.knnfII
                 #                     +a2.glmf+a2.glmfII
                 +a2.bnfAB,data = train.a2)
fit_svm.a3 = svm(a3.r ~ a3.drf+a3.drfII+a3.xgb+a3.xgbII
                 +a3.bnf+a3.bnfII+a1.svmf+a3.svmfII+a3.knnf+a3.knnfII
                 #                     +a3.glmf+a3.glmfII
                 +a3.bnfAB,data = train.a3)
fit_svm.a4 = svm(a4.r ~ a4.drf+a4.drfII+a4.xgb+a4.xgbII
                 +a4.bnf+a4.bnfII+a1.svmf+a4.svmfII+a4.knnf+a4.knnfII
                 #                     +a4.glmf+a4.glmfII
                 +a4.bnfAB,data = train.a4)
fit_svm.a5 = svm(a5.r ~ a5.drf+a5.drfII+a5.xgb+a5.xgbII
                 +a5.bnf+a5.bnfII+a1.svmf+a5.svmfII+a5.knnf+a5.knnfII
                 #                     +a5.glmf+a5.glmfII
                 +a5.bnfAB,data = train.a5)
fit_svm.b1 = svm(b1.r ~ b1.drf+b1.drfII+b1.xgb+b1.xgbII
                 +b1.bnf+b1.bnfII+b1.svmf+b1.svmfII+b1.knnf+b1.knnfII
                 #                     +b1.glmf+b1.glmfII
                 +b1.bnfAB,data = train.b1)
fit_svm.b2 = svm(b2.r ~ b2.drf+b2.drfII+b2.xgb+b2.xgbII
                 +b2.bnf+b2.bnfII+b1.svmf+b2.svmfII+b2.knnf+b2.knnfII
                 #                     +b2.glmf+b2.glmfII
                 +b2.bnfAB,data = train.b2)





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

test.a1<-data.frame(a1.drf,a1.drfII,a1.xgb,a1.xgbII,
                    a1.bnf,a1.bnfII,a1.svmf,a1.svmfII,a1.knnf,a1.knnfII,
                    #                    a1.glmf,a1.glmfII,
                    a1.bnfAB)
test.a2<-data.frame(a2.drf,a2.drfII,a2.xgb,a2.xgbII,
                    a2.bnf,a2.bnfII,a2.svmf,a2.svmfII,a2.knnf,a2.knnfII,
                    #                    a2.glmf,a2.glmfII,
                    a2.bnfAB)
test.a3<-data.frame(a3.drf,a3.drfII,a3.xgb,a3.xgbII,
                    a3.bnf,a3.bnfII,a3.svmf,a3.svmfII,a3.knnf,a3.knnfII,
                    #                    a3.glmf,a3.glmfII,
                    a3.bnfAB)
test.a4<-data.frame(a4.drf,a4.drfII,a4.xgb,a4.xgbII,
                    a4.bnf,a4.bnfII,a4.svmf,a4.svmfII,a4.knnf,a4.knnfII,
                    #                    a4.glmf,a4.glmfII,
                    a4.bnfAB)
test.a5<-data.frame(a5.drf,a5.drfII,a5.xgb,a5.xgbII,
                    a5.bnf,a5.bnfII,a5.svmf,a5.svmfII,a5.knnf,a5.knnfII,
                    #                    a5.glmf,a5.glmfII,
                    a5.bnfAB)
test.b1<-data.frame(b1.drf,b1.drfII,b1.xgb,b1.xgbII,
                    b1.bnf,b1.bnfII,b1.svmf,b1.svmfII,b1.knnf,b1.knnfII,
                    #                    b1.glmf,b1.glmfII,
                    b1.bnfAB)
test.b2<-data.frame(b2.drf,b2.drfII,b2.xgb,b2.xgbII,
                    b2.bnf,b2.bnfII,b2.svmf,b2.svmfII,b2.knnf,b2.knnfII,
                    #                    b2.glmf,b2.glmfII,
                    b2.bnfAB)

#Bayes predict
#p.a1 <-predict(fit_bn.a1,round(test.a1),node = "a1.r")


#randomForest predict

p.a1 = predict(rf.a1,test.a1)
p.a2 = predict(rf.a2,test.a2)
p.a3 = predict(rf.a3,test.a3)
p.a4 = predict(rf.a4,test.a4)
p.a5 = predict(rf.a5,test.a5)
p.b1 = predict(rf.b1,test.b1)
p.b2 = predict(rf.b2,test.b2)

#Result
r.rdf<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)


#xgboostpredict
#t.a1<-Matrix(as.matrix(test.a1[,1:13]),sparse=T)
#t.a2<-Matrix(as.matrix(test.a2[,1:13]),sparse=T)
#t.a3<-Matrix(as.matrix(test.a3[,1:13]),sparse=T)
#t.a4<-Matrix(as.matrix(test.a4[,1:13]),sparse=T)
#t.a5<-Matrix(as.matrix(test.a5[,1:13]),sparse=T)
#t.b1<-Matrix(as.matrix(test.b1[,1:13]),sparse=T)
#t.b2<-Matrix(as.matrix(test.b2[,1:13]),sparse=T)

t.a1<-Matrix(as.matrix(test.a1[,1:11]),sparse=T)
t.a2<-Matrix(as.matrix(test.a2[,1:11]),sparse=T)
t.a3<-Matrix(as.matrix(test.a3[,1:11]),sparse=T)
t.a4<-Matrix(as.matrix(test.a4[,1:11]),sparse=T)
t.a5<-Matrix(as.matrix(test.a5[,1:11]),sparse=T)
t.b1<-Matrix(as.matrix(test.b1[,1:11]),sparse=T)
t.b2<-Matrix(as.matrix(test.b2[,1:11]),sparse=T)

p.a1 <- predict(object = bst.a1,newdata = t(t.a1))
p.a2 <- predict(object = bst.a2,newdata = t(t.a2))
p.a3 <- predict(object = bst.a3,newdata = t(t.a3))
p.a4 <- predict(object = bst.a4,newdata = t(t.a4))
p.a5 <- predict(object = bst.a5,newdata = t(t.a5))
p.b1 <- predict(object = bst.b1,newdata = t(t.b1))
p.b2 <- predict(object = bst.b2,newdata = t(t.b2))

#Result
result.a<-c(mean(p.a1),mean(p.a2),mean(p.a3),mean(p.a4),mean(p.a5))
r.xgb<-c(sort(result.a),mean(p.b1),mean(p.b2))

#GLM test suit
#p.a1<-predict(cvfit_glm.a1, newx=as.matrix(test.a1[1:13]),s="lambda.1se")
#p.a2<-predict(cvfit_glm.a2, newx=as.matrix(test.a2[1:13]),s="lambda.1se")
#p.a3<-predict(cvfit_glm.a3, newx=as.matrix(test.a3[1:13]),s="lambda.1se")
#p.a4<-predict(cvfit_glm.a4, newx=as.matrix(test.a4[1:13]),s="lambda.1se")
#p.a5<-predict(cvfit_glm.a5, newx=as.matrix(test.a5[1:13]),s="lambda.1se")
#p.b1<-predict(cvfit_glm.b1, newx=as.matrix(test.b1[1:13]),s="lambda.1se")
#p.b2<-predict(cvfit_glm.b2, newx=as.matrix(test.b2[1:13]),s="lambda.1se")

p.a1<-predict(cvfit_glm.a1, newx=as.matrix(test.a1[1:11]),s="lambda.1se")
p.a2<-predict(cvfit_glm.a2, newx=as.matrix(test.a2[1:11]),s="lambda.1se")
p.a3<-predict(cvfit_glm.a3, newx=as.matrix(test.a3[1:11]),s="lambda.1se")
p.a4<-predict(cvfit_glm.a4, newx=as.matrix(test.a4[1:11]),s="lambda.1se")
p.a5<-predict(cvfit_glm.a5, newx=as.matrix(test.a5[1:11]),s="lambda.1se")
p.b1<-predict(cvfit_glm.b1, newx=as.matrix(test.b1[1:11]),s="lambda.1se")
p.b2<-predict(cvfit_glm.b2, newx=as.matrix(test.b2[1:11]),s="lambda.1se")

#Result
r.glm<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)

#KNN test suit
p.a1 <- predict(fit_knn.a1, test.a1)
p.a2 <- predict(fit_knn.a2, test.a2)
p.a3 <- predict(fit_knn.a3, test.a3)
p.a4 <- predict(fit_knn.a4, test.a4)
p.a5 <- predict(fit_knn.a5, test.a5)
p.b1 <- predict(fit_knn.b1, test.b1)
p.b2 <- predict(fit_knn.b2, test.b2)

#Result
r.knn<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)

#SVM test suit
p.a1 <- predict(object = fit_svm.a1,newdata = test.a1)
p.a2 <- predict(object = fit_svm.a2,newdata = test.a2)
p.a3 <- predict(object = fit_svm.a3,newdata = test.a3)
p.a4 <- predict(object = fit_svm.a4,newdata = test.a4)
p.a5 <- predict(object = fit_svm.a5,newdata = test.a5)
p.b1 <- predict(object = fit_svm.b1,newdata = test.b1)
p.b2 <- predict(object = fit_svm.b2,newdata = test.b2)

#Reault
r.svm<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)



drf<-t.drf
drfII<-t.drfII
bnf<-t.bnf
bnII<-t.bnfII
xgb<-t.xgb
xgbII<-t.xgbII
glmf<-t.glmf
glmfII<-t.glmfII
svmf<-t.svmf
svmfII<-t.svmfII
knn<-t.knnf
knnII<-t.knnfII
bnAB<-t.bnfAB

AB.final<-data.frame(drf,drfII,bnf,bnII,xgb,xgbII,glmf,glmfII,svmf,svmfII,knn,knnII,bnAB)

floor(r.rdf)
floor(r.svm)
floor(r.glm)
floor(r.xgb)
floor(r.knn)







