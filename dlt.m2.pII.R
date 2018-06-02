library(mclust)
library(FNN)
#Build data
trains_1 <-tail(dlt,302)[1:300,]
trains_2 <-tail(dlt,302)[2:301,]
results<-tail(dlt,300)
tests_1<-tail(dlt,2)[1,]
tests_2<-tail(dlt,2)[2,]
#A:
trn1<-trains_1$n
trn2<-trains_2$n
a1.1<-trains_1$a1
a2.1<-trains_1$a2
a3.1<-trains_1$a3
a4.1<-trains_1$a4
a5.1<-trains_1$a5
a1.2<-trains_2$a1
a2.2<-trains_2$a2
a3.2<-trains_2$a3
a4.2<-trains_2$a4
a5.2<-trains_2$a5
resa1<-results$a1
resa2<-results$a2
resa3<-results$a3
resa4<-results$a4
resa5<-results$a5
trains.a11<-data.frame(trn1,trn2,a1.2,a1.1,resa1)
trains.a22<-data.frame(trn1,trn2,a2.2,a2.1,resa2)
trains.a33<-data.frame(trn1,trn2,a3.2,a3.1,resa3)
trains.a44<-data.frame(trn1,trn2,a4.2,a4.1,resa4)
trains.a55<-data.frame(trn1,trn2,a5.2,a5.1,resa5)
#B:
b1.1<-trains_1$b1
b2.1<-trains_1$b2
b1.2<-trains_2$b1
b2.2<-trains_2$b2
resb1<-results$b1
resb2<-results$b2
trains.b11<-data.frame(trn1,trn2,b1.2,b1.1,resb1)
trains.b22<-data.frame(trn1,trn2,b2.2,b2.1,resb2)
#构建gMclust回归模型
fit_mc.a11<-Mclust(trains.a11[3:4],parallel = TRUE)
fit_mc.a22<-Mclust(trains.a22[3:4],parallel = TRUE)
fit_mc.a33<-Mclust(trains.a33[3:4],parallel = TRUE)
fit_mc.a44<-Mclust(trains.a44[3:4],parallel = TRUE)
fit_mc.a55<-Mclust(trains.a55[3:4],parallel = TRUE)
fit_mc.b11<-Mclust(trains.b11[3:4],parallel = TRUE)
fit_mc.b22<-Mclust(trains.b22[3:4],parallel = TRUE)


#summary(fit_mc.a1,parametwrs = TRUE)
#Build test data
#A:
tsn1<-tests_1$n
tsn2<-tests_2$n
a1.1<-tests_1$a1
a2.1<-tests_1$a2
a3.1<-tests_1$a3
a4.1<-tests_1$a4
a5.1<-tests_1$a5
a1.2<-tests_2$a1
a2.2<-tests_2$a2
a3.2<-tests_2$a3
a4.2<-tests_2$a4
a5.2<-tests_2$a5
tests.a11<-data.frame(tsn1,tsn2,a1.2,a1.1)
tests.a22<-data.frame(tsn1,tsn2,a2.2,a2.1)
tests.a33<-data.frame(tsn1,tsn2,a3.2,a3.1)
tests.a44<-data.frame(tsn1,tsn2,a4.2,a4.1)
tests.a55<-data.frame(tsn1,tsn2,a5.2,a5.1)
#B:
b1.1<-tests_1$b1
b2.1<-tests_1$a2
b1.2<-tests_2$b1
b2.2<-tests_2$b2
tests.b11<-data.frame(tsn1,tsn2,b1.2,b1.1)
tests.b22<-data.frame(tsn1,tsn2,b2.2,b2.1)
#Mclust回归模型预测测试集
p.a11<-predict(fit_mc.a11, tests.a11[3:4])
p.a22<-predict(fit_mc.a22, tests.a22[3:4])
p.a33<-predict(fit_mc.a33, tests.a33[3:4])
p.a44<-predict(fit_mc.a44, tests.a44[3:4])
p.a55<-predict(fit_mc.a55, tests.a55[3:4])
p.b11<-predict(fit_mc.b11, tests.b11[3:4])
p.b22<-predict(fit_mc.b22, tests.b22[3:4])
p.a11<-table(fit_mc.a11$classification,trains.a11$resa1)[p.a11$classification,]
barplot(p.a11,main = "a11")
p.a22<-table(fit_mc.a22$classification,trains.a22$resa2)[p.a22$classification,]
barplot(p.a22,main = "a22")
p.a33<-table(fit_mc.a33$classification,trains.a33$resa3)[p.a33$classification,]
barplot(p.a33,main = "a33")
p.a44<-table(fit_mc.a44$classification,trains.a44$resa4)[p.a44$classification,]
barplot(p.a44,main = "a44")
p.a55<-table(fit_mc.a55$classification,trains.a55$resa5)[p.a55$classification,]
barplot(p.a55,main = "a55")
p.b11<-table(fit_mc.b11$classification,trains.b11$resb1)[p.b11$classification,]
barplot(p.b11,main = "b11")
p.b22<-table(fit_mc.b22$classification,trains.b22$resb2)[p.b22$classification,]
barplot(p.b22,main = "b22")

#sort(p.a11,decreasing = TRUE)
nm.a11<-as.integer(names(p.a11))
p<-(p.a11)/sum(p.a11)
p.a11<-cbind(nm.a11,p)
p.a11<-as.data.frame(p.a11)
p.a11<-p.a11[which(p!=0),]

#sort(p.a22,decreasing = TRUE)
nm.a22<-as.integer(names(p.a22))
p<-(p.a22)/sum(p.a22)
p.a22<-cbind(nm.a22,p)
p.a22<-as.data.frame(p.a22)
p.a22<-p.a22[which(p!=0),]

#sort(p.a33,decreasing = TRUE)
nm.a33<-as.integer(names(p.a33))
p<-(p.a33)/sum(p.a33)
p.a33<-cbind(nm.a33,p)
p.a33<-as.data.frame(p.a33)
p.a33<-p.a33[which(p!=0),]

#sort(p.a44,decreasing = TRUE)
nm.a44<-as.integer(names(p.a44))
p<-(p.a44)/sum(p.a44)
p.a44<-cbind(nm.a44,p)
p.a44<-as.data.frame(p.a44)
p.a44<-p.a44[which(p!=0),]

#sort(p.a55,decreasing = TRUE)
nm.a55<-as.integer(names(p.a55))
p<-(p.a55)/sum(p.a55)
p.a55<-cbind(nm.a55,p)
p.a55<-as.data.frame(p.a55)
p.a55<-p.a55[which(p!=0),]

#sort(p.b11,decreasing = TRUE)
nm.b11<-as.integer(names(p.b11))
p<-(p.b11)/sum(p.b11)
p.b11<-cbind(nm.b11,p)
p.b11<-as.data.frame(p.b11)
p.b11<-p.b11[which(p!=0),]

#sort(p.b22,decreasing = TRUE)
nm.b22<-as.integer(names(p.b22))
p<-(p.b22)/sum(p.b22)
p.b22<-cbind(nm.b22,p)
p.b22<-as.data.frame(p.b22)
p.b22<-p.b22[which(p!=0),]

l.a11<-dim(p.a11)[1]
l.a22<-dim(p.a22)[1]
l.a33<-dim(p.a33)[1]
l.a44<-dim(p.a44)[1]
l.a55<-dim(p.a55)[1]
l.b11<-dim(p.b11)[1]
l.b22<-dim(p.b22)[1]

#############################################################################################
#Build B data
b.data<-c(1,1,1)
for(m in 1:l.b11) {
  for(n in 1:l.b22) {
    if(p.b22[[n,1]]>p.b11[m,1]) {
      b.temp<-c(p.b11[m,1],p.b22[n,1],
                p.b11[m,2]*p.b22[n,2])
      print(b.temp)
      b.data<-c(b.data,b.temp)
      
    }
  }
}
b.data.m<-matrix(b.data,ncol = 3,byrow = TRUE)[-1,]
b11<-b.data.m[,1]
b22<-b.data.m[,2]
bp<-b.data.m[,3]
b.data.f<-data.frame(b11,b22,bp)

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
#Build a Data & AB data
a.data<-c(1,1,1,1,1,1)
ab.data<-c(1,1,1,1,1,1,1,1,1,1,1,1)

for(i in 1:l.a11) {
  for(j in 1:l.a22) {
    if(p.a22[[j,1]]>p.a11[i,1]) {
      for(g in 1:l.a33) {
        if(p.a33[g,1]>p.a22[j,1]) {
          for(k in 1:l.a44) {
            if(p.a44[k,1]>p.a33[g,1]) {
              for(h in 1:l.a55) {
                if(p.a55[h,1]>p.a44[k,1])
                  p.a<-p.a11[i,2]*p.a22[j,2]*p.a33[g,2]*p.a44[k,2]*p.a55[h,2]
                a.temp<-c(p.a11[i,1],p.a22[j,1],p.a33[g,1],p.a44[k,1],p.a55[h,1],p.a)
                print(a.temp)
                a.data<-c(a.data,a.temp)
                
              }
              
              
            }
          }
        }
      }
    }
  }
}
a.data.m<-matrix(a.data,ncol = 6,byrow = TRUE)[-1,]
a11<-a.data.m[,1]
a22<-a.data.m[,2]
a33<-a.data.m[,3]
a44<-a.data.m[,4]
a55<-a.data.m[,5]
ap<-a.data.m[,6]
a.data.f<-data.frame(a11,a22,a33,a44,a55,ap)

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
#生成混合概率
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

lr<-2.4e-9   ##############################上限################################################## 
hr<-2.8e-9   ##############################下限##################################################


#生成符合概率区间组合

ab.final.p<-ab.p.f[which((ab.p>lr)&(ab.p<hr)),]
r.ab<-dim(ab.final.p)[1]
ab.data<-c(1,1,1,1,1,1,1,1,1,1,1,1)
for(i in 1:r.ab) {
  print(c(i,r.ab))
  p.f.a<-ab.final.p[i,]$a.p
  p.f.b<-ab.final.p[i,]$b.p
  data.f.a<-a.data.f[which(ap==p.f.a),]
  data.f.b<-b.data.f[which(bp==p.f.b),]
  r.af<-dim(data.f.a)[1]
  r.bf<-dim(data.f.b)[1]
  for(p in 1:r.af) {
    for(q in 1:r.bf) {
      ab.temp<-c(data.f.a[p,]$a11,data.f.a[p,]$a22,data.f.a[p,]$a33,data.f.a[p,]$a44,
                 data.f.a[p,]$a55,data.f.b[q,]$b11,data.f.b[q,]$b22,p.f.a,p.f.b,
                 sum(data.f.a[p,]$a11,data.f.a[p,]$a22,data.f.a[p,]$a33,data.f.a[p,]$a44,
                     data.f.a[p,]$a55),
                 sum(data.f.b[q,]$b11,data.f.b[q,]$b22),ab.final.p[i,]$ab.p)
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
############################################################################################
#KNN
a11<-6
a22<-11
a33<-19
a44<-24
a55<-30
b11<-4
b22<-9

train_a.t<-tail(a.data.f,400000)[,1:5]
train_a.h<-head(a.data.f,400000)[,1:5]
test_a<-data.frame(a11,a22,a33,a44,a55)

N<-80000
cl_a <-  factor(c(rep("a11",N), rep("a22",N), rep("a33",N), rep("a44",N), rep("a55",N)))
knn(train_a.t, test_a, cl_a, k = 100, prob=TRUE)

t_f.t<-a.data.f[c(85820,85803,86043,34299,81771,85579,131739,85835,85818,89515),]

cl_a <-  factor(c(rep("a11",N), rep("a22",N), rep("a33",N), rep("a44",N), rep("a55",N)))
knn(train_a.h, test_a, cl_a, k = 100, prob=TRUE)


h_f.h<-a.data.f[c(3326859,378380,382075,378363,374331,378603,378395,378378,378139),]

##############################################################################################
t.a1<-25
t.a2<-18
t.a3<-30
ab.f<-ab.data.f[which(
  #              (sa>=64&sa<=133)&
  #              (sb>=5&sb<=16)&
  #              (a1==t.a1|a2==t.a1|a3==t.a1|a4==t.a1|a5==t.a1)&
  (a1==t.a2|a2==t.a2|a3==t.a2|a4==t.a2|a5==t.a2)&
    (a1==t.a3|a2==t.a3|a3==t.a3|a4==t.a3|a5==t.a3)&
    (b1==3|b2==3)
),]


a11<-a.data.m[,1]
a22<-a.data.m[,2]
a33<-a.data.m[,3]
a44<-a.data.m[,4]
a55<-a.data.m[,5]
ap<-a.data.m[,6]
a.data.f<-data.frame(a11,a22,a33,a44,a55,ap)
b11<-b.data.m[,1]
b22<-b.data.m[,2]
bp<-b.data.m[,3]
b.data.f<-data.frame(b11,b22,bp)
a.data.f[which(a11==19&a22==20&a33==30&a44==32&a55==33),]
b.data.f[which(b11==3&b22==5),]




