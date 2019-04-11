
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

data.xgb.clu.clu.a<-sort(data.xgb.clu.clu[1:5])
data.xgb.clu.clu.b<-sort(data.xgb.clu.clu[6:7])

data.rdf.clu.clu.a<-sort(as.numeric(data.rdf.clu.clu[1:5]))
data.rdf.clu.clu.b<-sort(as.numeric(data.rdf.clu.clu[6:7]))

data.xgb.row.clu.a<-sort(data.xgb.row.clu[1:5])
data.xgb.row.clu.b<-sort(data.xgb.row.clu[6:7])

data.xgb.clu.row.a<-sort(data.xgb.clu.row[1:5])
data.xgb.clu.row.b<-sort(data.xgb.clu.row[6:7])

data.xgb.row.row.a<-sort(data.xgb.row.row[1:5])
data.xgb.row.row.b<-sort(data.xgb.row.row[6:7])

r.a1<-c(data.xgb.clu.clu.a[1],data.rdf.clu.clu.a[1],data.xgb.row.clu.a[1],data.xgb.clu.row.a[1],
        data.xgb.row.row.a[1],data.xgb.clu.cum$a1)
r.a2<-c(data.xgb.clu.clu.a[2],data.rdf.clu.clu.a[2],data.xgb.row.clu.a[2],data.xgb.clu.row.a[2],
        data.xgb.row.row.a[2],data.xgb.clu.cum$a2)
r.a3<-c(data.xgb.clu.clu.a[3],data.rdf.clu.clu.a[3],data.xgb.row.clu.a[3],data.xgb.clu.row.a[3],
        data.xgb.row.row.a[3],data.xgb.clu.cum$a3)
r.a4<-c(data.xgb.clu.clu.a[4],data.rdf.clu.clu.a[4],data.xgb.row.clu.a[4],data.xgb.clu.row.a[4],
        data.xgb.row.row.a[4],data.xgb.clu.cum$a4)
r.a5<-c(data.xgb.clu.clu.a[5],data.rdf.clu.clu.a[5],data.xgb.row.clu.a[5],data.xgb.clu.row.a[5],
        data.xgb.row.row.a[5],data.xgb.clu.cum$a5)
r.b1<-c(data.xgb.clu.clu.b[1],data.rdf.clu.clu.b[1],data.xgb.row.clu.b[1],data.xgb.clu.row.b[1],
        data.xgb.row.row.b[1],data.xgb.clu.cum$b1)
r.b2<-c(data.xgb.clu.clu.b[2],data.rdf.clu.clu.b[2],data.xgb.row.clu.b[2],data.xgb.clu.row.b[2],
        data.xgb.row.row.b[2],data.xgb.clu.cum$b2)

sort(table(r.a1))
sort(table(r.a2))
sort(table(r.a3))
sort(table(r.a4))
sort(table(r.a5))
sort(table(r.b1))
sort(table(r.b2))
########################################################################################################
#train.sa<-head(dlt$sa,dim(dlt)[1]-1)
#train.sb<-head(dlt$sb,dim(dlt)[1]-1)
#result.sa<-tail(dlt$sa,dim(dlt)[1]-1)
#result.sb<-tail(dlt$sb,dim(dlt)[1]-1)

#train
sa.1<-dlt$sa[1:(dim(dlt)[1]-3)]
sa.2<-dlt$sa[2:(dim(dlt)[1]-2)]
sa.3<-dlt$sa[3:(dim(dlt)[1]-1)]
result.sa<-dlt$sa[4:dim(dlt)[1]]
trains.sa<-data.frame(sa.1,sa.2,sa.3,result.sa)
trains.T.sa<-Matrix(as.matrix(trains.sa[,1:3]),sparse=T)
bst.sa<-xgboost(data = trains.T.sa,label = trains.sa$result.sa,nrounds = 300,print_every_n = 300L)


sb.1<-dlt$sb[1:(dim(dlt)[1]-3)]
sb.2<-dlt$sb[2:(dim(dlt)[1]-2)]
sb.3<-dlt$sb[3:(dim(dlt)[1]-1)]
result.sb<-dlt$sb[4:dim(dlt)[1]]
trains.sb<-data.frame(sb.1,sb.2,sb.3,result.sb)
trains.T.sb<-Matrix(as.matrix(trains.sb[,1:3]),sparse=T)
bst.sb<-xgboost(data = trains.T.sb,label = trains.sb$result.sb,nrounds = 300,print_every_n = 300L)

#predoct
sa.1<-dlt$sa[2:(dim(dlt)[1]-2)]
sa.2<-dlt$sa[3:(dim(dlt)[1]-1)]
sa.3<-dlt$sa[4:dim(dlt)[1]]
tests.sa<-data.frame(sa.1,sa.2,sa.3)
tests.T.sa<-Matrix(as.matrix(tests.sa),sparse=T)
testPredictions.sa <- predict(object = bst.sa,newdata = tests.T.sa)

sb.1<-dlt$sb[2:(dim(dlt)[1]-2)]
sb.2<-dlt$sb[3:(dim(dlt)[1]-1)]
sb.3<-dlt$sb[4:dim(dlt)[1]]
tests.sb<-data.frame(sb.1,sb.2,sb.3)
tests.T.sb<-Matrix(as.matrix(tests.sb),sparse=T)
testPredictions.sb <- predict(object = bst.sb,newdata = tests.T.sb)

sa.pre<-round(testPredictions.sa)
sb.pre<-round(testPredictions.sb)

c(tail(sa.pre,1),tail(sb.pre,1))



