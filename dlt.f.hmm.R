data<-dlt
count<-dim(dlt)[1]
dlt.f.hmm <- function(data,count) {
  library(HMM)
  library(markovchain)
  library(parallel)
  #Build data
  dlt.ab<-tail(data,count)
  rows.ab<-dim(dlt.ab)[1]
  dlt.a<-matrix(0,nrow = rows.ab*5,ncol = 1)
  j<-1
  test<-tail(data)
  for(i in 1:rows.ab) {
    dlt.a[j  ,1]<-dlt.ab$a1[i]
    dlt.a[j+1,1]<-dlt.ab$a2[i]
    dlt.a[j+2,1]<-dlt.ab$a3[i]
    dlt.a[j+3,1]<-dlt.ab$a4[i]
    dlt.a[j+4,1]<-dlt.ab$a5[i]
    j<-j+5
  }
  
  #Build HMM model
  states<-c("s1","s2","s3")
  #states<-c("s1","s2","s3","s4","s5")
  numstates<-length(states)
  symbols<- c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20,
              21,22,23,24,25,26,27,28,29,30,
              31,32,33,34,35)
  numsymbols<-length(symbols)
  
  set.seed(124124)
  startingProbabilities<-matrix(runif(numstates),1,numstates)
  startingProbabilities<-sweep(startingProbabilities,1,
                               rowSums(startingProbabilities),FUN = "/")
  
  set.seed(454235)
  trainsitionProbabilities<-matrix(runif(numstates*numstates),numstates,numstates)
  trainsitionProbabilities<-sweep(trainsitionProbabilities,1,
                                  rowSums(trainsitionProbabilities),FUN = "/")
  
  set.seed(923501)
  emissionProbilities<-matrix(runif(numstates*numsymbols),numstates,numsymbols)
  emissionProbilities<-sweep(emissionProbilities,1,
                             rowSums(emissionProbilities),FUN = "/")
  
  hmm<-initHMM(states,symbols,startProbs = startingProbabilities,
               emissionProbs = emissionProbilities)
  hmm_trained.a1<-baumWelch(hmm,data$a1)
  barplot(hmm_trained.a1$hmm$emissionProbs,main = "a1")
  
  
  #states<-c("s1","s2","s3","s4")
  numstates<-length(states)
  symbols<- c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20,
              21,22,23,24,25,26,27,28,29,30,
              31,32,33,34,35)
  numsymbols<-length(symbols)
  
  set.seed(124124)
  startingProbabilities<-matrix(runif(numstates),1,numstates)
  startingProbabilities<-sweep(startingProbabilities,1,
                               rowSums(startingProbabilities),FUN = "/")
  
  set.seed(454235)
  trainsitionProbabilities<-matrix(runif(numstates*numstates),numstates,numstates)
  trainsitionProbabilities<-sweep(trainsitionProbabilities,1,
                                  rowSums(trainsitionProbabilities),FUN = "/")
  
  set.seed(923501)
  emissionProbilities<-matrix(runif(numstates*numsymbols),numstates,numsymbols)
  emissionProbilities<-sweep(emissionProbilities,1,
                             rowSums(emissionProbilities),FUN = "/")
  
  hmm<-initHMM(states,symbols,startProbs = startingProbabilities,
               emissionProbs = emissionProbilities)
  hmm_trained.a2<-baumWelch(hmm,data$a2)
  barplot(hmm_trained.a2$hmm$emissionProbs,main = "a2")
  hmm_trained.a3<-baumWelch(hmm,data$a3) 
  barplot(hmm_trained.a3$hmm$emissionProbs,main = "a3")
  hmm_trained.a5<-baumWelch(hmm,data$a5) 
  barplot(hmm_trained.a5$hmm$emissionProbs,main = "a5")
  
  states<-c("s1","s2","s3")
  numstates<-length(states)
  symbols<- c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20,
              21,22,23,24,25,26,27,28,29,30,
              31,32,33,34,35)
  numsymbols<-length(symbols)
  
  set.seed(124124)
  startingProbabilities<-matrix(runif(numstates),1,numstates)
  startingProbabilities<-sweep(startingProbabilities,1,
                               rowSums(startingProbabilities),FUN = "/")
  
  set.seed(454235)
  trainsitionProbabilities<-matrix(runif(numstates*numstates),numstates,numstates)
  trainsitionProbabilities<-sweep(trainsitionProbabilities,1,
                                  rowSums(trainsitionProbabilities),FUN = "/")
  
  set.seed(923501)
  emissionProbilities<-matrix(runif(numstates*numsymbols),numstates,numsymbols)
  emissionProbilities<-sweep(emissionProbilities,1,
                             rowSums(emissionProbilities),FUN = "/")
  
  hmm<-initHMM(states,symbols,startProbs = startingProbabilities,
               emissionProbs = emissionProbilities)
  
  hmm_trained.a4<-baumWelch(hmm,data$a4)
  barplot(hmm_trained.a4$hmm$emissionProbs,main = "a4")
  hmm_trained.a<-baumWelch(hmm,dlt.a)
  barplot(hmm_trained.a$hmm$emissionProbs,main = "a")
  
  states<-c("s1","s2","s3","s4","s5","s6")
  #states<-c("s1","s2","s3","s4","s5","s6","s7","s8")
  numstates<-length(states)
  symbols<- c(1,2,3,4,5,6,7,8,9,10,11,12)
  numsymbols<-length(symbols)
  
  set.seed(124124)
  startingProbabilities<-matrix(runif(numstates),1,numstates)
  startingProbabilities<-sweep(startingProbabilities,1,
                               rowSums(startingProbabilities),FUN = "/")
  
  set.seed(454235)
  trainsitionProbabilities<-matrix(runif(numstates*numstates),numstates,numstates)
  trainsitionProbabilities<-sweep(trainsitionProbabilities,1,
                                  rowSums(trainsitionProbabilities),FUN = "/")
  
  set.seed(923501)
  emissionProbilities<-matrix(runif(numstates*numsymbols),numstates,numsymbols)
  emissionProbilities<-sweep(emissionProbilities,1,
                             rowSums(emissionProbilities),FUN = "/")
  
  hmm<-initHMM(states,symbols,startProbs = startingProbabilities,
               emissionProbs = emissionProbilities)
  hmm_trained.b1<-baumWelch(hmm,data$b1)
  barplot(hmm_trained.b1$hmm$emissionProbs,main = "b1")
  hmm_trained.b2<-baumWelch(hmm,data$b2)
  barplot(hmm_trained.b2$hmm$emissionProbs,main = "b2")
  
  
  
  trained_transition_probabilites.a1<-hmm_trained.a1$hmm$transProbs
  trained_transition_probabilites.a2<-hmm_trained.a2$hmm$transProbs
  trained_transition_probabilites.a3<-hmm_trained.a3$hmm$transProbs
  trained_transition_probabilites.a4<-hmm_trained.a4$hmm$transProbs
  trained_transition_probabilites.a5<-hmm_trained.a5$hmm$transProbs
  trained_transition_probabilites.b1<-hmm_trained.b1$hmm$transProbs
  trained_transition_probabilites.b2<-hmm_trained.b2$hmm$transProbs
  
  test<-tail(data,1)
  
  #a1
  #hmm_trained.a1$hmm$emissionProbs[,test$a1]
  a1.m<-max(hmm_trained.a1$hmm$emissionProbs[,test$a1])
  #trained_transition_probabilites.a1
  for(i in 1:length(hmm_trained.a1$hmm$emissionProbs[,test$a1])) {
    if(hmm_trained.a1$hmm$emissionProbs[,test$a1][i]==a1.m) {
      break()
    }
  }
  barplot(hmm_trained.a1$hmm$emissionProb[i,],main = "a1")
  n.a1<-as.integer(names(hmm_trained.a1$hmm$emissionProb[i,]))
  p.a1<-hmm_trained.a1$hmm$emissionProb[i,]
  p.a1.f<-data.frame(n.a1,p.a1)
  #p.a1.f<-p.a1.f[p.a1!=0,]
  #a2
  #hmm_trained.a2$hmm$emissionProbs[,test$a2]
  a2.m<-max(hmm_trained.a2$hmm$emissionProbs[,test$a2])
  #trained_transition_probabilites.a2
  for(i in 1:length(hmm_trained.a2$hmm$emissionProbs[,test$a2])) {
    if(hmm_trained.a2$hmm$emissionProbs[,test$a2][i]==a2.m) {
      break()
    }
  }
  barplot(hmm_trained.a2$hmm$emissionProb[i,],main = "a2")
  n.a2<-as.integer(names(hmm_trained.a2$hmm$emissionProb[i,]))
  p.a2<-hmm_trained.a2$hmm$emissionProb[i,]
  p.a2.f<-data.frame(n.a2,p.a2)
  #p.a2.f<-p.a2.f[p.a2!=0,]
  #a3
  #hmm_trained.a3$hmm$emissionProbs[,test$a3]
  a3.m<-max(hmm_trained.a3$hmm$emissionProbs[,test$a3])
  #trained_transition_probabilites.a3
  for(i in 1:length(hmm_trained.a3$hmm$emissionProbs[,test$a3])) {
    if(hmm_trained.a3$hmm$emissionProbs[,test$a3][i]==a3.m) {
      break()
    }
  }
  barplot(hmm_trained.a3$hmm$emissionProb[i,],main = "a3")
  n.a3<-as.integer(names(hmm_trained.a3$hmm$emissionProb[i,]))
  p.a3<-hmm_trained.a3$hmm$emissionProb[i,]
  p.a3.f<-data.frame(n.a3,p.a3)
  #p.a3.f<-p.a3.f[p.a3!=0,]
  #a4
  #hmm_trained.a4$hmm$emissionProbs[,test$a4]
  a4.m<-max(hmm_trained.a4$hmm$emissionProbs[,test$a4])
  #trained_transition_probabilites.a4
  for(i in 1:length(hmm_trained.a4$hmm$emissionProbs[,test$a4])) {
    if(hmm_trained.a4$hmm$emissionProbs[,test$a4][i]==a4.m) {
      break()
    }
  }
  barplot(hmm_trained.a4$hmm$emissionProb[i,],main = "a4")
  n.a4<-as.integer(names(hmm_trained.a4$hmm$emissionProb[i,]))
  p.a4<-hmm_trained.a4$hmm$emissionProb[i,]
  p.a4.f<-data.frame(n.a4,p.a4)
  #p.a4.f<-p.a4.f[p.a4!=0,]
  #a5
  #hmm_trained.a5$hmm$emissionProbs[,test$a5]
  a5.m<-max(hmm_trained.a5$hmm$emissionProbs[,test$a5])
  #trained_transition_probabilites.a5
  for(i in 1:length(hmm_trained.a5$hmm$emissionProbs[,test$a5])) {
    if(hmm_trained.a5$hmm$emissionProbs[,test$a5][i]==a5.m) {
      break()
    }
  }
  barplot(hmm_trained.a5$hmm$emissionProb[i,],main = "a5")
  n.a5<-as.integer(names(hmm_trained.a5$hmm$emissionProb[i,]))
  p.a5<-hmm_trained.a5$hmm$emissionProb[i,]
  p.a5.f<-data.frame(n.a5,p.a5)
  #p.a5.f<-p.a5.f[p.a5!=0,]
  #b1
  #hmm_trained.b1$hmm$emissionProbs[,test$b1]
  b1.m<-max(hmm_trained.b1$hmm$emissionProbs[,test$b1])
  #trained_transition_probabilites.b1
  for(i in 1:length(hmm_trained.b1$hmm$emissionProbs[,test$b1])) {
    if(hmm_trained.b1$hmm$emissionProbs[,test$b1][i]==b1.m) {
      break()
    }
  }
  barplot(hmm_trained.b1$hmm$emissionProb[i,],main = "b1")
  n.b1<-as.integer(names(hmm_trained.b1$hmm$emissionProb[i,]))
  p.b1<-hmm_trained.b1$hmm$emissionProb[i,]
  p.b1.f<-data.frame(n.b1,p.b1)
  #p.b1.f<-p.b1.f[p.b1!=0,]
  #b2
  #hmm_trained.b1$hmm$emissionProbs[,test$b2]
  b2.m<-max(hmm_trained.b2$hmm$emissionProbs[,test$b2])
  #trained_transition_probabilites.b2
  for(i in 1:length(hmm_trained.b2$hmm$emissionProbs[,test$b2])) {
    if(hmm_trained.b2$hmm$emissionProbs[,test$b2][i]==b2.m) {
      break()
    }
  }
  barplot(hmm_trained.b2$hmm$emissionProb[i,],main = "b2")
  n.b2<-as.integer(names(hmm_trained.b2$hmm$emissionProb[i,]))
  p.b2<-hmm_trained.b2$hmm$emissionProb[i,]
  p.b2.f<-data.frame(n.b2,p.b2)
  #p.b2.f<-p.b2.f[p.b2!=0,]
  
  result<-c(p.a1.f,p.a2.f,p.a3.f,p.a4.f,p.a5.f,p.b1.f,p.b2.f)
  return(result)
  
}


