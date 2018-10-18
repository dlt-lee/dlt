rows<-dim(dlt)[1]-9

for(i in 1:3) {
  data<-head(dlt,rows)
  for.clu.xgb(data)
  rows<-rows+3
}