dlt.BNF <- function(data,count) {
  library(bnlearn)
  #library(Rgraphviz)
  #Build data
  tests<-tail(data,1)
  
  trains.a <-tail(data,(count))[1:(count-1),]
  results.a<-tail(data,(count-1))
  #A:
  trn<-trains.a$n
  a1 <-trains.a$a1
  a2 <-trains.a$a2
  a3 <-trains.a$a3
  a4 <-trains.a$a4
  a5 <-trains.a$a5
  
  resa1<-results.a$a1
  resa2<-results.a$a2
  resa3<-results.a$a3
  resa4<-results.a$a4
  resa5<-results.a$a5
  
  #B:
  trains.b<-tail(data,count)[1:(count-1),]
  results.b<-tail(data,(count-1))
  
  b1<-trains.b$b1
  b2<-trains.b$b2
  
  resb1<-results.b$b1
  resb2<-results.b$b2
  
  trains.a1<-data.frame(a1,a2,a3,a4,a5,resa1)
  trains.a2<-data.frame(a1,a2,a3,a4,a5,resa2)
  trains.a3<-data.frame(a1,a2,a3,a4,a5,resa3)
  trains.a4<-data.frame(a1,a2,a3,a4,a5,resa4)
  trains.a5<-data.frame(a1,a2,a3,a4,a5,resa5)
  
  trains.b1<-data.frame(b1,b2,resb1)
  trains.b2<-data.frame(b1,b2,resb2)
  
  #A1:
  bn.a1<-bnlearn::hc(trains.a1)
  bn.a1<-set.arc(bn.a1,"a1","resa1")
  bn.a1<-set.arc(bn.a1,"a2","resa1")
  bn.a1<-set.arc(bn.a1,"a3","resa1")
  bn.a1<-set.arc(bn.a1,"a4","resa1")
  bn.a1<-set.arc(bn.a1,"a5","resa1")
  
  bn.a1<-drop.arc(bn.a1,"a1","a2")
  bn.a1<-drop.arc(bn.a1,"a2","a3")
  bn.a1<-drop.arc(bn.a1,"a3","a4")
  bn.a1<-drop.arc(bn.a1,"a4","a5")
  
  fit_bn.a1 <- bn.fit(bn.a1, data = trains.a1)
  p.a1 <-predict(fit_bn.a1,tests[4:8],node = "resa1")
  #A2:
  bn.a2<-bnlearn::hc(trains.a2)
  bn.a2<-set.arc(bn.a2,"a1","resa2")
  bn.a2<-set.arc(bn.a2,"a2","resa2")
  bn.a2<-set.arc(bn.a2,"a3","resa2")
  bn.a2<-set.arc(bn.a2,"a4","resa2")
  bn.a2<-set.arc(bn.a2,"a5","resa2")
  
  bn.a2<-drop.arc(bn.a2,"a1","a2")
  bn.a2<-drop.arc(bn.a2,"a2","a3")
  bn.a2<-drop.arc(bn.a2,"a3","a4")
  bn.a2<-drop.arc(bn.a2,"a4","a5")
  
  fit_bn.a2 <- bn.fit(bn.a2, data = trains.a2)
  p.a2 <-predict(fit_bn.a2,tests[4:8],node = "resa2")
  #A3:
  bn.a3<-bnlearn::hc(trains.a3)
  bn.a3<-set.arc(bn.a3,"a1","resa3")
  bn.a3<-set.arc(bn.a3,"a2","resa3")
  bn.a3<-set.arc(bn.a3,"a3","resa3")
  bn.a3<-set.arc(bn.a3,"a4","resa3")
  bn.a3<-set.arc(bn.a3,"a5","resa3")
  
  bn.a3<-drop.arc(bn.a3,"a1","a2")
  bn.a3<-drop.arc(bn.a3,"a2","a3")
  bn.a3<-drop.arc(bn.a3,"a3","a4")
  bn.a3<-drop.arc(bn.a3,"a4","a5")
  
  fit_bn.a3 <- bn.fit(bn.a3, data = trains.a3)
  p.a3<-predict(fit_bn.a3,tests[4:8],node = "resa3")
  #A4:
  bn.a4<-bnlearn::hc(trains.a4)
  bn.a4<-set.arc(bn.a4,"a1","resa4")
  bn.a4<-set.arc(bn.a4,"a2","resa4")
  bn.a4<-set.arc(bn.a4,"a3","resa4")
  bn.a4<-set.arc(bn.a4,"a4","resa4")
  bn.a4<-set.arc(bn.a4,"a5","resa4")
  
  bn.a4<-drop.arc(bn.a4,"a1","a2")
  bn.a4<-drop.arc(bn.a4,"a2","a3")
  bn.a4<-drop.arc(bn.a4,"a3","a4")
  bn.a4<-drop.arc(bn.a4,"a4","a5")
  
  fit_bn.a4 <- bn.fit(bn.a4, data = trains.a4)
  p.a4<-predict(fit_bn.a4,tests[4:8],node = "resa4")
  #A5:
  bn.a5<-bnlearn::hc(trains.a5)
  bn.a5<-set.arc(bn.a5,"a1","resa5")
  bn.a5<-set.arc(bn.a5,"a2","resa5")
  bn.a5<-set.arc(bn.a5,"a3","resa5")
  bn.a5<-set.arc(bn.a5,"a4","resa5")
  bn.a5<-set.arc(bn.a5,"a5","resa5")
  
  bn.a5<-drop.arc(bn.a5,"a1","a2")
  bn.a5<-drop.arc(bn.a5,"a2","a3")
  bn.a5<-drop.arc(bn.a5,"a3","a4")
  bn.a5<-drop.arc(bn.a5,"a4","a5")
  
  fit_bn.a5 <- bn.fit(bn.a5, data = trains.a5)
  p.a5<-predict(fit_bn.a5,tests[4:8],node = "resa5")
  #B1:
  bn.b1<-bnlearn::hc(trains.b1)
  bn.b1<-set.arc(bn.b1,"b1","resb1")
  bn.b1<-set.arc(bn.b1,"b2","resb1")
  
  bn.b1<-drop.arc(bn.b1,"b1","b2")
  
  
  fit_bn.b1 <- bn.fit(bn.b1, data = trains.b1)
  p.b1 <-predict(fit_bn.b1,tests[9:10],node = "resb1")
  
  #B2:
  bn.b2<-bnlearn::hc(trains.b2)
  bn.b2<-set.arc(bn.b2,"b1","resb2")
  bn.b2<-set.arc(bn.b2,"b2","resb2")
  
  bn.b2<-drop.arc(bn.b2,"b1","b2")
  
  fit_bn.b2 <- bn.fit(bn.b2, data = trains.b2)
  p.b2 <-predict(fit_bn.b2,tests[9:10],node = "resb2")
  
  result<-c(p.a1,p.a2,p.a3,p.a4,p.a5,p.b1,p.b2)
  barplot(result,main = "BNF")
  return(result)
  
  
}


