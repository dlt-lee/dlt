data<-dlt
count<-dim(dlt)[1]
temp.ab<-c(0,0,0,0,0,0,0,0)
seed<-c(0,1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97)
for (i in seed) {
    temp.c<-dlt.pr.xgb.XX(data,count,i)
  temp.ab<-c(temp.ab,i,temp.c)
}
#return(matrix(temp.ab,ncol = 8,byrow = TRUE))

result.data<-matrix(temp.ab,ncol = 8,byrow = TRUE)


result.data[-1,]

