data<-dlt
count<-dim(dlt)[1]
temp.ab<-c(0,0,0,0,0,0,0,0)
for (i in 1:30) {
  temp.c<-dlt.xgboost.I(data,count,i*10)
  temp.ab<-c(temp.ab,i,temp.c)
}
#return(matrix(temp.ab,ncol = 8,byrow = TRUE))

result.data<-matrix(temp.ab,ncol = 8,byrow = TRUE)


result.data[-1,]

