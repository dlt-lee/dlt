data<-dlt
count<-dim(dlt)[1]
#dlt.xgboost.row <- function(data,count,n) {
  data.xgb.I    <-dlt.xgboost.I(data,count,n)
  data.xgb.II   <-dlt.xgboost.II(data,count,n)
  data.xgb.III  <-dlt.xgboost.III(data,count,n)
  data.xgb.VI   <-dlt.xgboost.VI(data,count,n)
  data.xgb.IX   <-dlt.xgboost.IX(data,count,n)
  data.xgb.XII  <-dlt.xgboost.XII(data,count,n)
  data.xgb.XV   <-dlt.xgboost.XV(data,count,n)
  data.xgb.XVIII<-dlt.xgboost.XVIII(data,count,n)
  data.xgb.XXI  <-dlt.xgboost.XXI(data,count,n)
  
  r<-dim(data.xgb.XXI)[1]
  
  #data.xgb.I<-tail(data.xgb.I,r)[1:r-1]
  
  ab.I    <-tail(data.xgb.I,1)
  ab.II   <-tail(data.xgb.II,1)
  ab.III  <-tail(data.xgb.III,1)
  ab.VI   <-tail(data.xgb.VI,1)
  ab.IX   <-tail(data.xgb.IX,1)
  ab.XII  <-tail(data.xgb.XII,1)
  ab.XV   <-tail(data.xgb.XV,1)
  ab.XVIII<-tail(data.xgb.XVIII,1)
  ab.XXI  <-tail(data.xgb.XXI,1)
  
  table(c(ab.I$a1,ab.II$a1,ab.III$a1,ab.VI$a1,ab.IX$a1,ab.XII$a1,ab.XV$a1,ab.XVIII$a1,ab.XXI$a1))
  table(c(ab.I$a2,ab.II$a2,ab.III$a2,ab.VI$a2,ab.IX$a2,ab.XII$a2,ab.XV$a2,ab.XVIII$a2,ab.XXI$a2))
  table(c(ab.I$a3,ab.II$a3,ab.III$a3,ab.VI$a3,ab.IX$a3,ab.XII$a3,ab.XV$a3,ab.XVIII$a3,ab.XXI$a3))
  table(c(ab.I$a4,ab.II$a4,ab.III$a4,ab.VI$a4,ab.IX$a4,ab.XII$a4,ab.XV$a4,ab.XVIII$a4,ab.XXI$a4))
  table(c(ab.I$a5,ab.II$a5,ab.III$a5,ab.VI$a5,ab.IX$a5,ab.XII$a5,ab.XV$a5,ab.XVIII$a5,ab.XXI$a5))
  table(c(ab.I$b1,ab.II$b1,ab.III$b1,ab.VI$b1,ab.IX$b1,ab.XII$b1,ab.XV$b1,ab.XVIII$b1,ab.XXI$b1))
  table(c(ab.I$b2,ab.II$b2,ab.III$b2,ab.VI$b2,ab.IX$b2,ab.XII$b2,ab.XV$b2,ab.XVIII$b2,ab.XXI$b2))
  
  
#}