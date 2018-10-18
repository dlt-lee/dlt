rows<-dim(dlt)[1]-27

for(i in 1:9) {
  data<-head(dlt,rows)
  for.clu.xgb(data)
  rows<-rows+3
}