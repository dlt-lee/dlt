data<-dlt
count<-dim(dlt)[1]
temp.ab<-c(0,0,0,0,0,0,0,0)
seed<-c(0,1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
        ,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199
        )
for (i in seed) {
    temp.c<-dlt.pr.xgb.XX(data,count,i)
  temp.ab<-c(temp.ab,i,temp.c)
}
#return(matrix(temp.ab,ncol = 8,byrow = TRUE))

result.data<-matrix(temp.ab,ncol = 8,byrow = TRUE)


result.data<-result.data[-1,]

#A:
sort(-table(result.data[,2]))
sort(-table(result.data[,3]))
sort(-table(result.data[,4]))
sort(-table(result.data[,5]))
sort(-table(result.data[,6]))
#B:
sort(-table(result.data[,7]))
sort(-table(result.data[,8]))


