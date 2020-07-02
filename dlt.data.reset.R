dlt.data.reset <- function(data_org) {
  library(foreach)
  library(doParallel)
  
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  rows<-dim(data_org)[1]    # roews of orange data
  line<-rows-3              #prepare for tarining data
  
  n<-(line-line%%3)/3-2
  line<-line-line%%3+3
  
  data<-tail(data_org,line)
  
  
  
  a1.1<-0;a2.1<-0;a3.1<-0;a4.1<-0;a5.1<-0;b1.1<-0;b2.1<-0
  a1.2<-0;a2.2<-0;a3.2<-0;a4.2<-0;a5.2<-0;b1.2<-0;b2.2<-0
  a1.3<-0;a2.3<-0;a3.3<-0;a4.3<-0;a5.3<-0;b1.3<-0;b2.3<-0
  a1.4<-0;a2.4<-0;a3.4<-0;a4.4<-0;a5.4<-0;b1.4<-0;b2.4<-0
  a1.5<-0;a2.5<-0;a3.5<-0;a4.5<-0;a5.5<-0;b1.5<-0;b2.5<-0
  a1.6<-0;a2.6<-0;a3.6<-0;a4.6<-0;a5.6<-0;b1.6<-0;b2.6<-0
  a1.7<-0;a2.7<-0;a3.7<-0;a4.7<-0;a5.7<-0;b1.7<-0;b2.7<-0
  a1.8<-0;a2.8<-0;a3.8<-0;a4.8<-0;a5.8<-0;b1.8<-0;b2.8<-0
  a1.9<-0;a2.9<-0;a3.9<-0;a4.9<-0;a5.9<-0;b1.9<-0;b2.9<-0
  res.a1<-0
  res.a2<-0
  res.a3<-0
  res.a4<-0
  res.a5<-0
  res.b1<-0
  res.b2<-0
  
  j<-1
  foreach (i=1:n, .combine=cbind) %do% {
    a1.1<-c(a1.1,data[j+0,]$a1);a2.1<-c(a2.1,data[j+0,]$a2);a3.1<-c(a3.1,data[j+0,]$a3);a4.1<-c(a4.1,data[j+0,]$a4);a5.1<-c(a5.1,data[j+0,]$a5);b1.1<-c(b1.1,data[j+0,]$b1);b2.1<-c(b2.1,data[j+0,]$b2)
    a1.2<-c(a1.2,data[j+1,]$a1);a2.2<-c(a2.2,data[j+1,]$a2);a3.2<-c(a3.2,data[j+1,]$a3);a4.2<-c(a4.2,data[j+1,]$a4);a5.2<-c(a5.2,data[j+1,]$a5);b1.2<-c(b1.2,data[j+1,]$b1);b2.2<-c(b2.2,data[j+1,]$b2)
    a1.3<-c(a1.3,data[j+2,]$a1);a2.3<-c(a2.3,data[j+2,]$a2);a3.3<-c(a3.3,data[j+2,]$a3);a4.3<-c(a4.3,data[j+2,]$a4);a5.3<-c(a5.3,data[j+2,]$a5);b1.3<-c(b1.3,data[j+2,]$b1);b2.3<-c(b2.3,data[j+2,]$b2)
    a1.4<-c(a1.4,data[j+3,]$a1);a2.4<-c(a2.4,data[j+3,]$a2);a3.4<-c(a3.4,data[j+3,]$a3);a4.4<-c(a4.4,data[j+3,]$a4);a5.4<-c(a5.4,data[j+3,]$a5);b1.4<-c(b1.4,data[j+3,]$b1);b2.4<-c(b2.4,data[j+3,]$b2)
    a1.5<-c(a1.5,data[j+4,]$a1);a2.5<-c(a2.5,data[j+4,]$a2);a3.5<-c(a3.5,data[j+4,]$a3);a4.5<-c(a4.5,data[j+4,]$a4);a5.5<-c(a5.5,data[j+4,]$a5);b1.5<-c(b1.5,data[j+4,]$b1);b2.5<-c(b2.5,data[j+4,]$b2)
    a1.6<-c(a1.6,data[j+5,]$a1);a2.6<-c(a2.6,data[j+5,]$a2);a3.6<-c(a3.6,data[j+5,]$a3);a4.6<-c(a4.6,data[j+5,]$a4);a5.6<-c(a5.6,data[j+5,]$a5);b1.6<-c(b1.6,data[j+5,]$b1);b2.6<-c(b2.6,data[j+5,]$b2)
    a1.7<-c(a1.7,data[j+6,]$a1);a2.7<-c(a2.7,data[j+6,]$a2);a3.7<-c(a3.7,data[j+6,]$a3);a4.7<-c(a4.7,data[j+6,]$a4);a5.7<-c(a5.7,data[j+6,]$a5);b1.7<-c(b1.7,data[j+6,]$b1);b2.7<-c(b2.7,data[j+6,]$b2)
    a1.8<-c(a1.8,data[j+7,]$a1);a2.8<-c(a2.8,data[j+7,]$a2);a3.8<-c(a3.8,data[j+7,]$a3);a4.8<-c(a4.8,data[j+7,]$a4);a5.8<-c(a5.8,data[j+7,]$a5);b1.8<-c(b1.8,data[j+7,]$b1);b2.8<-c(b2.8,data[j+7,]$b2)
    a1.9<-c(a1.9,data[j+8,]$a1);a2.9<-c(a2.9,data[j+8,]$a2);a3.9<-c(a3.9,data[j+8,]$a3);a4.9<-c(a4.9,data[j+8,]$a4);a5.9<-c(a5.9,data[j+8,]$a5);b1.9<-c(b1.9,data[j+8,]$b1);b2.9<-c(b2.9,data[j+8,]$b2)
    res.a1<-c(res.a1,data[j+9,]$a1)
    res.a2<-c(res.a2,data[j+9,]$a2)
    res.a3<-c(res.a3,data[j+9,]$a3)
    res.a4<-c(res.a4,data[j+9,]$a4)
    res.a5<-c(res.a5,data[j+9,]$a5)
    res.b1<-c(res.b1,data[j+9,]$b1)
    res.b2<-c(res.b2,data[j+9,]$b2)
    j=j+3
    #print(j)
  }
  #stop cluster
  stopCluster(cl)
  
  a1.1<-as.matrix(a1.1)[-1];a2.1<-as.matrix(a2.1)[-1];a3.1<-as.matrix(a3.1)[-1];a4.1<-as.matrix(a4.1)[-1];a5.1<-as.matrix(a5.1)[-1];b1.1<-as.matrix(b1.1)[-1];b2.1<-as.matrix(b2.1)[-1]
  a1.2<-as.matrix(a1.2)[-1];a2.2<-as.matrix(a2.2)[-1];a3.2<-as.matrix(a3.2)[-1];a4.2<-as.matrix(a4.2)[-1];a5.2<-as.matrix(a5.2)[-1];b1.2<-as.matrix(b1.2)[-1];b2.2<-as.matrix(b2.2)[-1]
  a1.3<-as.matrix(a1.3)[-1];a2.3<-as.matrix(a2.3)[-1];a3.3<-as.matrix(a3.3)[-1];a4.3<-as.matrix(a4.3)[-1];a5.3<-as.matrix(a5.3)[-1];b1.3<-as.matrix(b1.3)[-1];b2.3<-as.matrix(b2.3)[-1]
  a1.4<-as.matrix(a1.4)[-1];a2.4<-as.matrix(a2.4)[-1];a3.4<-as.matrix(a3.4)[-1];a4.4<-as.matrix(a4.4)[-1];a5.4<-as.matrix(a5.4)[-1];b1.4<-as.matrix(b1.4)[-1];b2.4<-as.matrix(b2.4)[-1]
  a1.5<-as.matrix(a1.5)[-1];a2.5<-as.matrix(a2.5)[-1];a3.5<-as.matrix(a3.5)[-1];a4.5<-as.matrix(a4.5)[-1];a5.5<-as.matrix(a5.5)[-1];b1.5<-as.matrix(b1.5)[-1];b2.5<-as.matrix(b2.5)[-1]
  a1.6<-as.matrix(a1.6)[-1];a2.6<-as.matrix(a2.6)[-1];a3.6<-as.matrix(a3.6)[-1];a4.6<-as.matrix(a4.6)[-1];a5.6<-as.matrix(a5.6)[-1];b1.6<-as.matrix(b1.6)[-1];b2.6<-as.matrix(b2.6)[-1]
  a1.7<-as.matrix(a1.7)[-1];a2.7<-as.matrix(a2.7)[-1];a3.7<-as.matrix(a3.7)[-1];a4.7<-as.matrix(a4.7)[-1];a5.7<-as.matrix(a5.7)[-1];b1.7<-as.matrix(b1.7)[-1];b2.7<-as.matrix(b2.7)[-1]
  a1.8<-as.matrix(a1.8)[-1];a2.8<-as.matrix(a2.8)[-1];a3.8<-as.matrix(a3.8)[-1];a4.8<-as.matrix(a4.8)[-1];a5.8<-as.matrix(a5.8)[-1];b1.8<-as.matrix(b1.8)[-1];b2.8<-as.matrix(b2.8)[-1]
  a1.9<-as.matrix(a1.9)[-1];a2.9<-as.matrix(a2.9)[-1];a3.9<-as.matrix(a3.9)[-1];a4.9<-as.matrix(a4.9)[-1];a5.9<-as.matrix(a5.9)[-1];b1.9<-as.matrix(b1.9)[-1];b2.9<-as.matrix(b2.9)[-1]
  res.a1<-as.matrix(res.a1)[-1]
  res.a2<-as.matrix(res.a2)[-1]
  res.a3<-as.matrix(res.a3)[-1]
  res.a4<-as.matrix(res.a4)[-1]
  res.a5<-as.matrix(res.a5)[-1]
  res.b1<-as.matrix(res.b1)[-1]
  res.b2<-as.matrix(res.b2)[-1]
  
  trains<-data.frame(a1.1,a2.1,a3.1,a4.1,a5.1,b1.1,b2.1,
                     a1.2,a2.2,a3.2,a4.2,a5.2,b1.2,b2.2,
                     a1.3,a2.3,a3.3,a4.3,a5.3,b1.3,b2.3,
                     a1.4,a2.4,a3.4,a4.4,a5.4,b1.4,b2.4,
                     a1.5,a2.5,a3.5,a4.5,a5.5,b1.5,b2.5,
                     a1.6,a2.6,a3.6,a4.6,a5.6,b1.6,b2.6,
                     a1.7,a2.7,a3.7,a4.7,a5.7,b1.7,b2.7,
                     a1.8,a2.8,a3.8,a4.8,a5.8,b1.8,b2.8,
                     a1.9,a2.9,a3.9,a4.9,a5.9,b1.9,b2.9,
                     res.a1,res.a2,res.a3,res.a4,res.a5,res.b1,res.b2)

  
  return(trains)
 
}


