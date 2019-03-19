
data.for.xgb.clu<-for.clu.xgb()
data.for.xgb.row<-for.row.xgb()

data.xgb.clu.clu<-dlt.xgb.clu.clu(data.for.xgb.clu)
data.xgb.row.clu<-dlt.xgb.row.clu(data.for.xgb.row)
data.xgb.clu.row<-dlt.xgb.clu.row(data.for.xgb.clu)
data.xgb.row.row<-dlt.xgb.row.row(data.for.xgb.row)

data.rdf.clu.clu<-dlt.rdf.clu.clu(data.for.xgb.clu)
data.xgb.clu.cum<-dlt.xgb.clu.cum(data.for.xgb.clu)
data.xgb.row.cum<-dlt.xgb.row.cum(data.for.xgb.row)
data.xgb.clu.row.aver<-dlt.xgb.clu.row.aver(data.for.xgb.clu)
data.xgb.row.row.aver<-dlt.xgb.row.row.aver(data.for.xgb.row)

data.tr<-dlt.data.filter(dlt)
data.xgb.clu<-dlt.xgboost.clu(dlt,data.tr)
data.xgb.row<-dlt.xgboost.row(dlt,data.tr)



#clu:
sort(table(data.xgb.clu$a1.clu))
sort(table(data.xgb.clu$a2.clu))
sort(table(data.xgb.clu$a3.clu))
sort(table(data.xgb.clu$a4.clu))
sort(table(data.xgb.clu$a5.clu))
sort(table(data.xgb.clu$b1.clu))
sort(table(data.xgb.clu$b2.clu))
#row:
sort(table(data.xgb.row$a1.row))
sort(table(data.xgb.row$a2.row))
sort(table(data.xgb.row$a3.row))
sort(table(data.xgb.row$a4.row))
sort(table(data.xgb.row$a5.row))
sort(table(data.xgb.row$b1.row))
sort(table(data.xgb.row$b2.row))
#clu.row:
sort(table(data.xgb.clu.row.aver$a1))
sort(table(data.xgb.clu.row.aver$a2))
sort(table(data.xgb.clu.row.aver$a3))
sort(table(data.xgb.clu.row.aver$a4))
sort(table(data.xgb.clu.row.aver$a5))
sort(table(data.xgb.clu.row.aver$b1))
sort(table(data.xgb.clu.row.aver$b2))
#row.row:
sort(table(data.xgb.row.row.aver$a1))
sort(table(data.xgb.row.row.aver$a2))
sort(table(data.xgb.row.row.aver$a3))
sort(table(data.xgb.row.row.aver$a4))
sort(table(data.xgb.row.row.aver$a5))
sort(table(data.xgb.row.row.aver$b1))
sort(table(data.xgb.row.row.aver$b2))


data.xgb.clu.clu
data.rdf.clu.clu
data.xgb.row.clu
data.xgb.clu.row
data.xgb.row.row
data.xgb.clu.cum
data.xgb.row.cum


train.sa<-head(dlt$sa,dim(dlt)[1]-1)
train.sb<-head(dlt$sb,dim(dlt)[1]-1)
result.sa<-tail(dlt$sa,dim(dlt)[1]-1)
result.sb<-tail(dlt$sb,dim(dlt)[1]-1)

trains.sa<-data.frame(train.sa,result.sa)
trains.T.sa<-Matrix(as.matrix(trains.sa[,1]),sparse=T)
trains.sb<-data.frame(train.sb,result.sb)
trains.T.sb<-Matrix(as.matrix(trains.sb[,1]),sparse=T)
bst.sa<-xgboost(data = trains.T.sa,label = trains.sa$result.sa,nrounds = 300,print_every_n = 300L)
bst.sb<-xgboost(data = trains.T.sb,label = trains.sb$result.sb,nrounds = 300,print_every_n = 300L)

tests.sa<-data.frame(result.sa)
tests.T.sa<-Matrix(as.matrix(tests.sa[,1]),sparse=T)
testPredictions.sa <- predict(object = bst.sa,newdata = tests.T.sa)

tests.sb<-data.frame(result.sb)
tests.T.sb<-Matrix(as.matrix(tests.sb[,1]),sparse=T)
testPredictions.sb <- predict(object = bst.sb,newdata = tests.T.sb)

sa.max<-ceiling(tail(testPredictions.sa,1))
sa.min<-floor(tail(testPredictions.sa,1))
sb.max<-ceiling(tail(testPredictions.sb,1))
sb.min<-floor(tail(testPredictions.sb,1))

c(sa.min,sa.max)
c(sb.min,sb.max)





