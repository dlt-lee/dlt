library(HMM)

cell<-c(19092,3,8,15,22,32,5,10,2,11,15,27,30,2,5
             )

ab_m<-matrix(cell,ncol = 15,byrow = TRUE)
#构建HMM回归模型
states<-c("s1","s2","s3","s4","s5")
numstates<-length(states)
numstates<-length(states)
symbols<- c(01,02,03,04,05,06,07,08,09,10,
            11,12,13,14,15,16,17,18,19,20,
            21,22,23,24,25,26,27,28,29,30,
            31,32,33,34,35)
numsymbols<-length(symbols)

emissionProbilities<-matrix(runif(numstates*numsymbols),numstates,numsymbols)
emissionProbilities<-sweep(emissionProbilities,1,
                           rowSums(emissionProbilities),FUN = "/")

hmm<-initHMM(states,symbols,startProbs = startingProbabilities,
             emissionProbs = emissionProbilities)