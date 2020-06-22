library(corrplot)
library(AppliedPredictiveModeling)
library(caret)


dlt_numeric<-dlt[,4:10]
#Pre-processing transformation
trans<-preProcess(dlt_numeric,method = c("BoxCox","center","scale","pca"))
#implement transformed
transformed<-predict(trans,dlt_numeric)
#filter
nearZeroVar(dlt_numeric)
#correlations
correlations<-cor(dlt_numeric)
dim(correlations)
#plot hclust
corrplot(correlations,order = "hclust")

#检查线性相关 0.5
highCorr<-findCorrelation(correlations,cutoff = 0.5)
filtereddlt_numeric<-dlt_numeric[,-highCorr]

#组建特征样本
#f.n<-head(dlt$n,(dim(filtereddlt_numeric)[1]-1))
#f.a1<-head(filtereddlt_numeric$a1,(dim(filtereddlt_numeric)[1]-1))
#f.a5<-head(filtereddlt_numeric$a5,(dim(filtereddlt_numeric)[1]-1))
#f.b2<-head(filtereddlt_numeric$b2,(dim(filtereddlt_numeric)[1]-1))
f.n<-head(dlt$n,(dim(dlt_numeric)[1]-1))
f.a1<-head(dlt_numeric$a1,(dim(dlt_numeric)[1]-1))
f.a2<-head(dlt_numeric$a2,(dim(dlt_numeric)[1]-1))
f.a3<-head(dlt_numeric$a3,(dim(dlt_numeric)[1]-1))
f.a4<-head(dlt_numeric$a4,(dim(dlt_numeric)[1]-1))
f.a5<-head(dlt_numeric$a5,(dim(dlt_numeric)[1]-1))
f.b1<-head(dlt_numeric$b1,(dim(dlt_numeric)[1]-1))
f.b2<-head(dlt_numeric$b2,(dim(dlt_numeric)[1]-1))
#组建校准样本
#n.a1<-tail(dlt$a1,(dim(filtereddlt_numeric)[1]-1))
#n.a2<-tail(dlt$a2,(dim(filtereddlt_numeric)[1]-1))
#n.a3<-tail(dlt$a3,(dim(filtereddlt_numeric)[1]-1))
#n.a4<-tail(dlt$a4,(dim(filtereddlt_numeric)[1]-1))
#n.a5<-tail(dlt$a5,(dim(filtereddlt_numeric)[1]-1))
#n.b1<-tail(dlt$b1,(dim(filtereddlt_numeric)[1]-1))
#n.b2<-tail(dlt$b2,(dim(filtereddlt_numeric)[1]-1))
n.a1<-tail(dlt$a1,(dim(dlt_numeric)[1]-1))
n.a2<-tail(dlt$a2,(dim(dlt_numeric)[1]-1))
n.a3<-tail(dlt$a3,(dim(dlt_numeric)[1]-1))
n.a4<-tail(dlt$a4,(dim(dlt_numeric)[1]-1))
n.a5<-tail(dlt$a5,(dim(dlt_numeric)[1]-1))
n.b1<-tail(dlt$b1,(dim(dlt_numeric)[1]-1))
n.b2<-tail(dlt$b2,(dim(dlt_numeric)[1]-1))

#Build classes data
#fn.a1<-data.frame(f.n,f.a1,f.a5,f.b2,n.a1)
#fn.a2<-data.frame(f.n,f.a1,f.a5,f.b2,n.a2)
#fn.a3<-data.frame(f.n,f.a1,f.a5,f.b2,n.a3)
#fn.a4<-data.frame(f.n,f.a1,f.a5,f.b2,n.a4)
#fn.a5<-data.frame(f.n,f.a1,f.a5,f.b2,n.a5)
#fn.b1<-data.frame(f.n,f.a1,f.a5,f.b2,n.b1)
#fn.b2<-data.frame(f.n,f.a1,f.a5,f.b2,n.b2)
fn.a1<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.a1)
fn.a2<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.a2)
fn.a3<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.a3)
fn.a4<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.a4)
fn.a5<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.a5)
fn.b1<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.b1)
fn.b2<-data.frame(f.n,f.a1,f.a2,f.a3,f.a4,f.a5,f.b1,f.b2,n.b2)

###################################################################################################
#分割
#设定随机数种子，保证结果可重复
set.seed(1)
#默认返回结果是列表
#list = FALSE 这些行数表示划分为训练集的样本
trainingROWS.a1<-createDataPartition(fn.a1$n.a1,p = .80,list = FALSE)
trainingROWS.a2<-createDataPartition(fn.a2$n.a2,p = .80,list = FALSE)
trainingROWS.a3<-createDataPartition(fn.a3$n.a3,p = .80,list = FALSE)
trainingROWS.a4<-createDataPartition(fn.a4$n.a4,p = .80,list = FALSE)
trainingROWS.a5<-createDataPartition(fn.a5$n.a5,p = .80,list = FALSE)
trainingROWS.b1<-createDataPartition(fn.b1$n.b1,p = .80,list = FALSE)
trainingROWS.b2<-createDataPartition(fn.b2$n.b2,p = .80,list = FALSE)
#3次重复抽样
#trainingROWS<-createDataPartition(fn.a1$f.a1,p = .80,times = 3)
#选取这些行数代表的样本到训练集
trainPredictors.a1<-fn.a1[trainingROWS.a1,2:4]
trainPredictors.a2<-fn.a2[trainingROWS.a2,2:4]
trainPredictors.a3<-fn.a3[trainingROWS.a3,2:4]
trainPredictors.a4<-fn.a4[trainingROWS.a4,2:4]
trainPredictors.a5<-fn.a5[trainingROWS.a5,2:4]
trainPredictors.b1<-fn.b1[trainingROWS.b1,2:4]
trainPredictors.b2<-fn.b2[trainingROWS.b2,2:4]
trainClasses.a1<-fn.a1[trainingROWS.a1,5]
trainClasses.a2<-fn.a2[trainingROWS.a2,5]
trainClasses.a3<-fn.a3[trainingROWS.a3,5]
trainClasses.a4<-fn.a4[trainingROWS.a4,5]
trainClasses.a5<-fn.a5[trainingROWS.a5,5]
trainClasses.b1<-fn.b1[trainingROWS.b1,5]
trainClasses.b2<-fn.b2[trainingROWS.b2,5]
#用负整数取下标得到测试集
testPredictors.a1<-fn.a1[-trainingROWS.a1,2:4]
testPredictors.a2<-fn.a2[-trainingROWS.a2,2:4]
testPredictors.a3<-fn.a3[-trainingROWS.a3,2:4]
testPredictors.a4<-fn.a4[-trainingROWS.a4,2:4]
testPredictors.a5<-fn.a5[-trainingROWS.a5,2:4]
testPredictors.b1<-fn.b1[-trainingROWS.b1,2:4]
testPredictors.b2<-fn.b2[-trainingROWS.b2,2:4]
testClasses.a1<-fn.a1[-trainingROWS.a1,5]
testClasses.a2<-fn.a2[-trainingROWS.a2,5]
testClasses.a3<-fn.a3[-trainingROWS.a3,5]
testClasses.a4<-fn.a4[-trainingROWS.a4,5]
testClasses.a5<-fn.a5[-trainingROWS.a5,5]
testClasses.b1<-fn.b1[-trainingROWS.b1,5]
testClasses.b2<-fn.b2[-trainingROWS.b2,5]

###################################################################################################
#设定随机数种子，保证结果可重复（Boostatrap）
set.seed(1)
trainingROWS.a1<-createResample(y = fn.a1$n.a1,times = 1,list = TRUE)
###################################################################################################
#设定随机数种子，保证结果可重复（KNN）
set.seed(1)
trainingROWS.a1<-createFolds(y = fn.a1$n.a1,k = 10,list = TRUE,returnTrain = TRUE)
###################################################################################################
#设定随机数种子，保证结果可重复（KNN）
set.seed(1)
trainingROWS.a1<-createMultiFolds(y = fn.a1$n.a1,k = 10,times = 5)
###################################################################################################
set.seed(1056)
svmFit<-train(n.a1~f.a1+f.a2+f.a3+f.a4+f.a5+f.b1+f.b2,
              data = fn.a1,
              method = "svmRadial",
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 50))
plot(svmFit,scales = list(x = list(log = 2)))




