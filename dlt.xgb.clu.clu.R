rows<-dim(dlt)[1]-270

for(i in 1:90) {
  data<-head(dlt,rows)
  data<-tail(data,300)
  for.clu.xgb(data)
  rows<-rows+3
  print("==========================================================")
}


