
m_all
sum.a1<-c(m_all[,04],data.xgb.clu$a1.clu,data.xgb.row$a1.row,data.xgb.clu.row.aver$a1,data.xgb.row.row.aver$a1)
sum.a2<-c(m_all[,05],data.xgb.clu$a2.clu,data.xgb.row$a2.row,data.xgb.clu.row.aver$a2,data.xgb.row.row.aver$a2)
sum.a3<-c(m_all[,06],data.xgb.clu$a3.clu,data.xgb.row$a3.row,data.xgb.clu.row.aver$a3,data.xgb.row.row.aver$a3)
sum.a4<-c(m_all[,07],data.xgb.clu$a4.clu,data.xgb.row$a4.row,data.xgb.clu.row.aver$a4,data.xgb.row.row.aver$a4)
sum.a5<-c(m_all[,08],data.xgb.clu$a5.clu,data.xgb.row$a5.row,data.xgb.clu.row.aver$a5,data.xgb.row.row.aver$a5)
sum.b1<-c(m_all[,09],data.xgb.clu$b1.clu,data.xgb.row$b1.row,data.xgb.clu.row.aver$b1,data.xgb.row.row.aver$b1)
sum.b2<-c(m_all[,10],data.xgb.clu$b2.clu,data.xgb.row$b2.row,data.xgb.clu.row.aver$b2,data.xgb.row.row.aver$b2)

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

temp<-tail(sum.data,64)
temp[which(temp$sum.Freq==1),]