
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


r.a1<-sort(unique(c(data.xgb.clu$a1.clu,data.xgb.row$a1.row,data.xgb.clu.row.aver$a1,data.xgb.row.row.aver$a1)))
r.a2<-sort(unique(c(data.xgb.clu$a2.clu,data.xgb.row$a2.row,data.xgb.clu.row.aver$a2,data.xgb.row.row.aver$a2)))
r.a3<-sort(unique(c(data.xgb.clu$a3.clu,data.xgb.row$a3.row,data.xgb.clu.row.aver$a3,data.xgb.row.row.aver$a3)))
r.a4<-sort(unique(c(data.xgb.clu$a4.clu,data.xgb.row$a4.row,data.xgb.clu.row.aver$a4,data.xgb.row.row.aver$a4)))
r.a5<-sort(unique(c(data.xgb.clu$a5.clu,data.xgb.row$a5.row,data.xgb.clu.row.aver$a5,data.xgb.row.row.aver$a5)))
r.b1<-sort(unique(c(data.xgb.clu$b1.clu,data.xgb.row$b1.row,data.xgb.clu.row.aver$b1,data.xgb.row.row.aver$b1)))
r.b2<-sort(unique(c(data.xgb.clu$b2.clu,data.xgb.row$b2.row,data.xgb.clu.row.aver$b2,data.xgb.row.row.aver$b2)))

t.data<-0
for (i in r.a1) {
  if (i>0) {
    for (j in r.a2) {
      if (j>i) {
        for (h in r.a3) {
          if (h>j) {
            for (k in r.a4) {
              if (k>h) {
                for (l in r.a5) {
                  if (36>l&l>k) {
                    for (p in r.b1) {
                      for (q in r.b2) {
                        if (p>0&q>p) {
                          print(c(i,j,h,k,l,p,q))
                          t.data<-c(t.data,i,j,h,k,l,p,q)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
t.data<-t.data[-1]
t.m.data<-matrix(t.data,,ncol = 7,byrow = TRUE)

sum.a1<-c(m_all[,04],t.m.data[,1])
sum.a2<-c(m_all[,05],t.m.data[,2])
sum.a3<-c(m_all[,06],t.m.data[,3])
sum.a4<-c(m_all[,07],t.m.data[,4])
sum.a5<-c(m_all[,08],t.m.data[,5])
sum.b1<-c(m_all[,09],t.m.data[,6])
sum.b2<-c(m_all[,10],t.m.data[,7])

l<-length(sum.a1)
n<-1
t<-1
for (i in 2:l) {
  t<-t+1
  n<-c(n,t)
}

sum.data<-data.frame(n,sum.a1,sum.a2,sum.a3,sum.a4,sum.a5,sum.b1,sum.b2)
sum.data<-data.matrix(sum.data)
sum.sa<-apply(sum.data[,2:6],1,sum,na.rm=TRUE)
sum.sb<-apply(sum.data[,7:8],1,sum,na.rm=TRUE)

sum.data<-data.frame(n,sum.a1,sum.a2,sum.a3,sum.a4,sum.a5,sum.b1,sum.b2,sum.sa,sum.sb)
sum.fre<-as.data.frame(table(sum.data$sum.sa,sum.data$sum.sb)) 
sum.sa<-sum.fre$Var1
sum.sb<-sum.fre$Var2
sum.Freq<-sum.fre$Freq
ab_s_f<-data.frame(sum.sa,sum.sb,sum.Freq)
sum.data<-merge(sum.data,ab_s_f,by=c("sum.sa","sum.sb"))
sum.data<-sum.data[order(sum.data$n),]

#=======================================================================================================

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
testPredictions.sa <- predict(object = bst.sa,newdata = t(tests.T.sa))

tests.sb<-data.frame(result.sb)
tests.T.sb<-Matrix(as.matrix(tests.sb[,1]),sparse=T)
testPredictions.sb <- predict(object = bst.sb,newdata = t(tests.T.sb))

sa.max<-ceiling(testPredictions.sa)
sa.min<-floor(testPredictions.sa)
sb.max<-ceiling(testPredictions.sb)
sb.min<-floor(testPredictions.sb)

#=======================================================================================================

temp<-tail(sum.data,l-dim(dlt)[1])
temp[which(temp$sum.Freq==1&(temp$sum.sa==sa.max|temp$sum.sa==sa.min)&(temp$sum.sb==sb.max|temp$sum.sb==sb.min)),]











