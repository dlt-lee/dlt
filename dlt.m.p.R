library(mclust)
library(FNN)
#Build data
tests<-tail(dlt,1)
trains.a <-tail(dlt,301)[1:300,]
results.a<-tail(dlt,300)
#A:
trn<-trains$n
a1<-trains.a$a1
a2<-trains.a$a2
a3<-trains.a$a3
a4<-trains.a$a4
a5<-trains.a$a5
resa1<-results.a$a1
resa2<-results.a$a2
resa3<-results.a$a3
resa4<-results.a$a4
resa5<-results.a$a5
trains.a1<-data.frame(a1,a2,a3,a4,a5,resa1)
trains.a2<-data.frame(a1,a2,a3,a4,a5,resa2)
trains.a3<-data.frame(a1,a2,a3,a4,a5,resa3)
trains.a4<-data.frame(a1,a2,a3,a4,a5,resa4)
trains.a5<-data.frame(a1,a2,a3,a4,a5,resa5)
#B:
trains.b<-tail(dlt,151)[1:150,]
results.b<-tail(dlt,150)
b1<-trains.b$b1
b2<-trains.b$b2
resb1<-results.b$b1
resb2<-results.b$b2
trains.b1<-data.frame(b1,b2,resb1)
trains.b2<-data.frame(b1,b2,resb2)
#构建gMclust回归模型
fit_mc.a1<-Mclust(trains.a1[1:5],parallel = TRUE)
fit_mc.a2<-Mclust(trains.a2[1:5],parallel = TRUE)
fit_mc.a3<-Mclust(trains.a3[1:5],parallel = TRUE)
fit_mc.a4<-Mclust(trains.a4[1:5],parallel = TRUE)
fit_mc.a5<-Mclust(trains.a5[1:5],parallel = TRUE)
fit_mc.b1<-Mclust(trains.b1[1:2],parallel = TRUE)
fit_mc.b2<-Mclust(trains.b2[1:2],parallel = TRUE)
#summary(fit_mc.a1,parametwrs = TRUE)
#Mclust回归模型预测测试集
p.a1<-predict(fit_mc.a1, tests[4:8])
p.a2<-predict(fit_mc.a2, tests[4:8])
p.a3<-predict(fit_mc.a3, tests[4:8])
p.a4<-predict(fit_mc.a4, tests[4:8])
p.a5<-predict(fit_mc.a5, tests[4:8])
p.b1<-predict(fit_mc.b1, tests[9:10])
p.b2<-predict(fit_mc.b2, tests[9:10])
p.a1<-table(fit_mc.a1$classification,trains.a1$resa1)[p.a1$classification,]
barplot(p.a1,main = "a1")
p.a2<-table(fit_mc.a2$classification,trains.a2$resa2)[p.a2$classification,]
barplot(p.a2,main = "a2")
p.a3<-table(fit_mc.a3$classification,trains.a3$resa3)[p.a3$classification,]
barplot(p.a3,main = "a3")
p.a4<-table(fit_mc.a4$classification,trains.a4$resa4)[p.a4$classification,]
barplot(p.a4,main = "a4")
p.a5<-table(fit_mc.a5$classification,trains.a5$resa5)[p.a5$classification,]
barplot(p.a5,main = "a5")
p.b1<-table(fit_mc.b1$classification,trains.b1$resb1)[p.b1$classification,]
barplot(p.b1,main = "b1")
p.b2<-table(fit_mc.b2$classification,trains.b2$resb2)[p.b2$classification,]
barplot(p.b2,main = "b2")

#sort(p.a1,decreasing = TRUE)
nm.a1<-as.integer(names(p.a1))
p<-(p.a1)/sum(p.a1)
p.a1<-cbind(nm.a1,p)
p.a1<-as.data.frame(p.a1)
p.a1<-p.a1[which(p!=0),]

#sort(p.a2,decreasing = TRUE)
nm.a2<-as.integer(names(p.a2))
p<-(p.a2)/sum(p.a2)
p.a2<-cbind(nm.a2,p)
p.a2<-as.data.frame(p.a2)
p.a2<-p.a2[which(p!=0),]

#sort(p.a3,decreasing = TRUE)
nm.a3<-as.integer(names(p.a3))
p<-(p.a3)/sum(p.a3)
p.a3<-cbind(nm.a3,p)
p.a3<-as.data.frame(p.a3)
p.a3<-p.a3[which(p!=0),]

#sort(p.a4,decreasing = TRUE)
nm.a4<-as.integer(names(p.a4))
p<-(p.a4)/sum(p.a4)
p.a4<-cbind(nm.a4,p)
p.a4<-as.data.frame(p.a4)
p.a4<-p.a4[which(p!=0),]

#sort(p.a5,decreasing = TRUE)
nm.a5<-as.integer(names(p.a5))
p<-(p.a5)/sum(p.a5)
p.a5<-cbind(nm.a5,p)
p.a5<-as.data.frame(p.a5)
p.a5<-p.a5[which(p!=0),]

#sort(p.b1,decreasing = TRUE)
nm.b1<-as.integer(names(p.b1))
p<-(p.b1)/sum(p.b1)
p.b1<-cbind(nm.b1,p)
p.b1<-as.data.frame(p.b1)
p.b1<-p.b1[which(p!=0),]

#sort(p.b2,decreasing = TRUE)
nm.b2<-as.integer(names(p.b2))
p<-(p.b2)/sum(p.b2)
p.b2<-cbind(nm.b2,p)
p.b2<-as.data.frame(p.b2)
p.b2<-p.b2[which(p!=0),]

l.a1<-dim(p.a1)[1]
l.a2<-dim(p.a2)[1]
l.a3<-dim(p.a3)[1]
l.a4<-dim(p.a4)[1]
l.a5<-dim(p.a5)[1]
l.b1<-dim(p.b1)[1]
l.b2<-dim(p.b2)[1]

#a.data<-matrix(0,nrow = l.a1*l.a2*l.a3*l.a4*l.a5,ncol = 7)
#b.data<-matrix(0,nrow = l.b1*l.b2,ncol = 7)

a.data<-c(1,1,1,1,1,1)

for(i in 1:l.a1) {
  for(j in 1:l.a2) {
    if(p.a2[[j,1]]>p.a1[i,1]) {
      for(g in 1:l.a3) {
        if(p.a3[g,1]>p.a2[j,1]) {
          for(k in 1:l.a4)
            if(p.a4[k,1]>p.a3[g,1]) {
              for(h in 1:l.a5) {
                if(p.a5[h,1]>p.a4[k,1])
                  a.temp<-c(p.a1[i,1],p.a2[j,1],p.a3[g,1],p.a4[k,1],p.a5[h,1],
                            p.a1[i,2]*p.a2[j,2]*p.a3[g,2]*p.a4[k,2]*p.a5[h,2])
                print(a.temp)
                a.data<-c(a.data,a.temp)
                
              }
            }
        }
      }
    }
  }
}
a.data.m<-matrix(a.data,ncol = 6,byrow = TRUE)[-1,]
a1<-a.data.m[,1]
a2<-a.data.m[,2]
a3<-a.data.m[,3]
a4<-a.data.m[,4]
a5<-a.data.m[,5]
ap<-a.data.m[,6]
a.data.f<-data.frame(a1,a2,a3,a4,a5,ap)
ap.short<-matrix(a.data.f[!duplicated(a.data.f$ap),]$ap,ncol = 1,byrow = TRUE)
n.ap<-dim(ap.short)[1]
p.a<-c(1,1)
for(i in 1:n.ap) {
  a.nm<-dim(a.data.f[which(ap==ap.short[i,1]),])[1]
  a.p<-ap.short[i,1]
  p.a<-c(p.a,a.p,a.nm)
}
p.a.m<-matrix(p.a,ncol = 2,byrow = TRUE)[-1,]
##################################################################################################

b.data<-c(1,1,1)
for(m in 1:l.b1) {
  for(n in 1:l.b2) {
    if(p.b2[[n,1]]>p.b1[m,1]) {
      b.temp<-c(p.b1[m,1],p.b2[n,1],
                p.b1[m,2]*p.b2[n,2])
      print(b.temp)
      b.data<-c(b.data,b.temp)
      
    }
  }
}
b.data.m<-matrix(b.data,ncol = 3,byrow = TRUE)[-1,]
b1<-b.data.m[,1]
b2<-b.data.m[,2]
bp<-b.data.m[,3]
b.data.f<-data.frame(b1,b2,bp)
bp.short<-matrix(b.data.f[!duplicated(b.data.f$bp),]$bp,ncol = 1,byrow = TRUE)
n.bp<-dim(bp.short)[1]
p.b<-c(1,1)
for(i in 1:n.bp) {
  b.nm<-dim(b.data.f[which(bp==bp.short[i,1]),])[1]
  b.p<-bp.short[i,1]
  p.b<-c(p.b,b.p,b.nm)
}
p.b.m<-matrix(p.b,ncol = 2,byrow = TRUE)[-1,]

#############################################################################################

r.pa<-dim(p.a.m)[1]
r.pb<-dim(p.b.m)[1]
ab.p<-c(1,1,1,1,1)
for(p in 1:r.pa) {
  for(q in 1:r.pb) {
    ab.temp<-c(p.a.m[p,1],p.a.m[p,2],p.b.m[q,1],p.b.m[q,2],p.a.m[p,1]*p.b.m[q,1])
    print(ab.temp)
    ab.p<-c(ab.p,ab.temp)
  }
}
ab.p.m<-matrix(ab.p,ncol = 5,byrow = TRUE)[-1,]
a.p<-ab.p.m[,1]
a.n<-ab.p.m[,2]
b.p<-ab.p.m[,3]
b.n<-ab.p.m[,4]
ab.p<-ab.p.m[,5]
ab.p.f<-data.frame(a.p,a.n,b.p,b.n,ab.p)

#############################################################################################
ab.final.p<-ab.p.f[which(ab.p>1e-09&ab.p<2e-09),]
#p.n.a<-p.a.m[p.a.m[,2]==24,]
#p.n.a<-ab.final.p[!duplicated(ab.final.p$a.p),]$a.p
r.ab<-dim(ab.final.p)[1]
#a.data.final<-a.data.f[which(ap==p.n.a[,1])]
ab.data<-c(1,1,1,1,1,1,1,1,1,1,1,1)
for(i in 1:r.ab) {
  ln<-c(i,r.ab)
  print(ln)
  p.f.a<-ab.final.p[i,]$a.p
  p.f.b<-ab.final.p[i,]$b.p
  data.f.a<-a.data.f[which(ap==p.f.a),]
  data.f.b<-b.data.f[which(bp==p.f.b),]
  r.af<-dim(data.f.a)[1]
  r.bf<-dim(data.f.b)[1]
  for(p in 1:r.af) {
    for(q in 1:r.bf) {
      ab.temp<-c(data.f.a[p,]$a1,data.f.a[p,]$a2,data.f.a[p,]$a3,data.f.a[p,]$a4,
                 data.f.a[p,]$a5,data.f.b[q,]$b1,data.f.b[q,]$b2,p.f.a,p.f.b,
                 sum(data.f.a[p,]$a1,data.f.a[p,]$a2,data.f.a[p,]$a3,data.f.a[p,]$a4,data.f.a[p,]$a5),
                 sum(data.f.b[q,]$b1,data.f.b[q,]$b2),ab.final.p[i,]$ab.p)
      ab.data<-c(ab.data,ab.temp)
      #print(ab.temp)
    }
  }
  
}

ab.data.m<-matrix(ab.data,ncol = 12,byrow = TRUE)[-1,]
a1 <-ab.data.m[, 1]
a2 <-ab.data.m[, 2]
a3 <-ab.data.m[, 3]
a4 <-ab.data.m[, 4]
a5 <-ab.data.m[, 5]
b1 <-ab.data.m[, 6]
b2 <-ab.data.m[, 7]
pa <-ab.data.m[, 8]
pb <-ab.data.m[, 9]
sa <-ab.data.m[,10]
sb <-ab.data.m[,11]
pab<-ab.data.m[,12]

ab.data.f<-data.frame(a1,a2,a3,a4,a5,sa,b1,b2,sb,pa,pb,pab)
##############################################################################################
a1<-6
a2<-13
a3<-19
a4<-25#
a5<-30
b1<-4
b2<-9

train_ab.t<-tail(ab.data.f,700000)[,1:7]
train_ab.h<-head(ab.data.f,700000)[,1:7]
test_ab<-data.frame(a1,a2,a3,a4,a5,b1,b2)
N<-100000
cl_ab <-  factor(c(rep("a1",N), rep("a2",N), rep("a3",N), 
                   rep("a4",N), rep("a5",N), rep("b1",N), rep("b2",N)))
knn(train_ab.t, test_ab, cl_ab, k = 100, prob=TRUE)

t_f<-train_ab.t[c(3470,3458,3380,3476,3386,3452,3374,3463,3469),]

cl_ab <-  factor(c(rep("a1",500), rep("a2",500), rep("a3",500), rep("a4",500), rep("a5",500), rep("b1",500), rep("b2",500)))
knn(train_ab.h, test_ab, cl_ab, k = 10, prob=TRUE)

t_f<-train_ab.t[c(3066,3072,2982,3150,3060,2988,3156,2976,3144,3078),]

##############################################################################################

ab.data.m[ab.data.m[,1]==4&ab.data.m[,4]==25&ab.data.m[,5]==31&ab.data.m[,6]==2&ab.data.m[,7]==9,]

a.final<-a.data.f[which(a1==19&a2==27&a3==31&a4==32&a5==35),]
b.final<-b.data.f[which(b1==11&b2==12),]
ap.final<-ab.p.f[which(a.p==a.final$ap&b.p==b.final$bp),]

ab.data.f[which((sb==13|sb==10|sb==14|sb==23|sb==7|sb==9|sb==12|sb==15|sb==16|sb==17)&(sa==49|sa==57|sa==59|sa==61|sa==76|sa==88|sa==102|sa==130)&a4==17&b2==9),]
