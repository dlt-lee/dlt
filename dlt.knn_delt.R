library(FNN)
dim(m_all)
m_all<-m_all[order(m_all[,3]),]

#train_ab<-m_all[13:dim(m_all)[1],]
train_ab<-tail(m_all,200)
#train_a<-m_all[13:dim(m_all)[1],4:8]
train_a<-tail(m_all,200)[,4:8]
test_a<-m_2017[dim(m_2017)[1],2:6]
cl_a <-  factor(c(rep("a1",40), rep("a2",40), rep("a3",40), rep("a4",40), rep("a5",40)))
knn(train_a, test_a, cl_a, k = 10, prob=TRUE)

t_f<-train_ab[c(200,108,193,14,39,73),]


t_f[order(t_f[,3]),]

table(t_f[,4:8])
#table(t_f[3:7,4:8])

#table(t_f[,9:10])
#table(t_f[3:7,9:10])


#train_a<-m_all[13:dim(m_all)[1],4:8]
train_b<-tail(m_all,200)[,9:10]
test_b<-m_2017[dim(m_2017)[1],7:8]
cl_b <-  factor(c(rep("b1",100), rep("b2",100)))
knn(train_b, test_b, cl_b, k = 30, prob=TRUE)

t_f<-train_ab[c(80,43,11,69,103,140,59,99,101,195,72,29,130,26,125),]


t_f[order(t_f[,3]),]

#table(t_f[,4:8])
#table(t_f[3:7,4:8])

table(t_f[,9:10])
#table(t_f[3:7,9:10])


