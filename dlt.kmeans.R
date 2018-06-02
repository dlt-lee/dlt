newiris <-tail(dlt,200);

#A:
kc <- kmeans(newiris[,4:8], 20);
f<-fitted(kc);
clu<-kc$cluster;
a1<-f[,1];
a2<-f[,2];
a3<-f[,3];
a4<-f[,4];
a5<-f[,5];
n<-newiris$n;
fm<-data.frame(n,a1,a2,a3,a4,a5,clu);
num<-tail(clu,1);
num<-fm[fm$clu==num,]$n
ran<-dim(data.matrix(num))-1
m<-tail(dlt,1)
for(i in 1:ran[1]) {
  
  m<-rbind(m,dlt[dlt$n==num[i],])
}
m[,4:8]
table(data.matrix(m[,4:8]))

#B:
kc <- kmeans(newiris[,9:10], 20);
f<-fitted(kc);
clu<-kc$cluster;
b1<-f[,1];
b2<-f[,2];
n<-newiris$n;
fm<-data.frame(n,b1,b2,clu);
num<-tail(clu,1);
num<-fm[fm$clu==num,]$n
ran<-dim(data.matrix(num))-1
m<-tail(dlt,1)
for(i in 1:ran[1]) {
  
  m<-rbind(m,dlt[dlt$n==num[i],])
}
m[,9:10]
table(data.matrix(m[,9:10]))

