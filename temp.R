a1.hmm<-cbind(ab_m[,2],tail(dlt,dim(ab_m)[1])$a1)
line<-dim(ab_m)[1]

#构建HMM回归模型
states<-c("s1","s2","s3","s4","s5")
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
for (i in 1:line) {
  hmm_trained.a1<-baumWelch(hmm,t(a1.hmm[i,]))
}

barplot(hmm_trained.a1$hmm$emissionProbs,main = "a1")
