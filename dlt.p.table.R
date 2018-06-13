dlt.p.table <- function(dlt,
                        testPredictions.a1,
                        testPredictions.a2,
                        testPredictions.a3,
                        testPredictions.a4,
                        testPredictions.a5,
                        testPredictions.b1,
                        testPredictions.b2) {
  
  rows<-length(testPredictions.a1)-1
  data.f.ab<-tail(dlt,rows)
  
  a1.Predictions<-head(testPredictions.a1,rows)
  a2.Predictions<-head(testPredictions.a2,rows)
  a3.Predictions<-head(testPredictions.a3,rows)
  a4.Predictions<-head(testPredictions.a4,rows)
  a5.Predictions<-head(testPredictions.a5,rows)
  b1.Predictions<-head(testPredictions.b1,rows)
  b2.Predictions<-head(testPredictions.b2,rows)
  
  n.a<-0
  n.b<-0
  n.c<-0
  for (i in 1:rows) {
    temp.a<-0
    temp.b<-0
    temp.c<-0
    if (data.f.ab[i,]$a1==a1.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (data.f.ab[i,]$a2==a2.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (data.f.ab[i,]$a3==a3.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (data.f.ab[i,]$a4==a4.Predictions[i]) {
      temp.a<-temp.a+1
    }
    if (data.f.ab[i,]$a5==a5.Predictions[i]) {
      temp.a<-temp.a+1
    }
    n.a<-c(n.a,temp.a)
    if (data.f.ab[i,]$b1==b1.Predictions[i]) {
      temp.b<-temp.b+1
    }
    if (data.f.ab[i,]$b2==b2.Predictions[i]) {
      temp.b<-temp.b+1
    }
    n.a<-c(n.a,temp.a)
    n.b<-c(n.b,temp.b)
    if (temp.a==5&temp.b==2) {
      temp.c<-1
    }
    else if(temp.a==5&temp.b==1) {
      temp.c<-2
    }
    else if (temp.a==5|(temp.a==4&temp.b==2)) {
      temp.c<-3
    }
    else if ((temp.a==4&temp.b==1)|(temp.a==3&temp.b==2)) {
      temp.c<-4
    }
    else if (temp.a==4|(temp.a==3&temp.b==1)|(temp.a==2&temp.b==2)) {
      temp.c<-5
    }
    else if (temp.a==3|(temp.a==1&temp.b==2)|(temp.a==2&temp.b==1)|temp.b==2) {
      temp.c<-6
    }
    n.c<-c(n.c,temp.c)
  }
  table(n.a)
  table(n.b)
  print(table(n.c))
  print(c(tail(floor(testPredictions.a1),1),
          tail(floor(testPredictions.a2),1),
          tail(floor(testPredictions.a3),1),
          tail(floor(testPredictions.a4),1),
          tail(floor(testPredictions.a5),1),
          tail(floor(testPredictions.b1),1),
          tail(floor(testPredictions.b2),1)))
  
}