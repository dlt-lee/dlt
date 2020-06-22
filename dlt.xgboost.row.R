data<-dlt

dlt.xgboost.row <- function(data,data.tr) {
  
  
  ab.1.row <-dlt.xgboost.I(data,data.tr)
  ab.2.row <-dlt.xgboost.II(data,data.tr)
  ab.3.row <-dlt.xgboost.III(data,data.tr)
  ab.6.row <-dlt.xgboost.VI(data,data.tr)
  ab.9.row <-dlt.xgboost.IX(data,data.tr)
  ab.12.row<-dlt.xgboost.XII(data,data.tr)
  ab.15.row <-dlt.xgboost.XV(data,data.tr)
  ab.18.row<-dlt.xgboost.XVIII(data,data.tr)
  ab.21.row<-dlt.xgboost.XXI(data,data.tr)
  ab.24.row<-dlt.xgboost.XXIV(data,data.tr)
  ab.27.row<-dlt.xgboost.XXVII(data,data.tr)
  ab.30.row<-dlt.xgboost.XXX(data,data.tr)
  ab.33.row<-dlt.xgboost.XXXIII(data,data.tr)
  ab.36.row<-dlt.xgboost.XXXVI(data,data.tr)
  ab.39.row<-dlt.xgboost.XXXIX(data,data.tr)
  ab.42.row<-dlt.xgboost.XXXXII(data,data.tr)
  
  
  a1.row<-c(tail(ab.1.row$a1,1),tail(ab.2.row$a1,1),tail(ab.3.row$a1,1),tail(ab.6.row$a1,1),
            tail(ab.9.row$a1,1),tail(ab.12.row$a1,1),tail(ab.15.row$a1,1),tail(ab.18.row$a1,1),
            tail(ab.21.row$a1,1),tail(ab.24.row$a1,1),tail(ab.27.row$a1,1),tail(ab.30.row$a1,1),
            tail(ab.33.row$a1,1),tail(ab.36.row$a1,1),tail(ab.39.row$a1,1),tail(ab.42.row$a1,1))
  
  a2.row<-c(tail(ab.1.row$a2,1),tail(ab.2.row$a2,1),tail(ab.3.row$a2,1),tail(ab.6.row$a2,1),
            tail(ab.9.row$a2,1),tail(ab.12.row$a2,1),tail(ab.15.row$a2,1),tail(ab.18.row$a2,1),
            tail(ab.21.row$a2,1),tail(ab.24.row$a2,1),tail(ab.27.row$a2,1),tail(ab.30.row$a2,1),
            tail(ab.33.row$a2,1),tail(ab.36.row$a2,1),tail(ab.39.row$a2,1),tail(ab.42.row$a2,1))
  
  a3.row<-c(tail(ab.1.row$a3,1),tail(ab.2.row$a3,1),tail(ab.3.row$a3,1),tail(ab.6.row$a3,1),
            tail(ab.9.row$a3,1),tail(ab.12.row$a3,1),tail(ab.15.row$a3,1),tail(ab.18.row$a3,1),
            tail(ab.21.row$a3,1),tail(ab.24.row$a3,1),tail(ab.27.row$a3,1),tail(ab.30.row$a3,1),
            tail(ab.33.row$a3,1),tail(ab.36.row$a3,1),tail(ab.39.row$a3,1),tail(ab.42.row$a3,1))
  
  a4.row<-c(tail(ab.1.row$a4,1),tail(ab.2.row$a4,1),tail(ab.3.row$a4,1),tail(ab.6.row$a4,1),
            tail(ab.9.row$a4,1),tail(ab.12.row$a4,1),tail(ab.15.row$a4,1),tail(ab.18.row$a4,1),
            tail(ab.21.row$a4,1),tail(ab.24.row$a4,1),tail(ab.27.row$a4,1),tail(ab.30.row$a4,1),
            tail(ab.33.row$a4,1),tail(ab.36.row$a4,1),tail(ab.39.row$a4,1),tail(ab.42.row$a4,1))
  
  a5.row<-c(tail(ab.1.row$a5,1),tail(ab.2.row$a5,1),tail(ab.3.row$a5,1),tail(ab.6.row$a5,1),
            tail(ab.9.row$a5,1),tail(ab.12.row$a5,1),tail(ab.15.row$a5,1),tail(ab.18.row$a5,1),
            tail(ab.21.row$a5,1),tail(ab.24.row$a5,1),tail(ab.27.row$a5,1),tail(ab.30.row$a5,1),
            tail(ab.33.row$a5,1),tail(ab.36.row$a5,1),tail(ab.39.row$a5,1),tail(ab.42.row$a5,1))
  
  b1.row<-c(tail(ab.1.row$b1,1),tail(ab.2.row$b1,1),tail(ab.3.row$b1,1),tail(ab.6.row$b1,1),
            tail(ab.9.row$b1,1),tail(ab.12.row$b1,1),tail(ab.15.row$b1,1),tail(ab.18.row$b1,1),
            tail(ab.21.row$b1,1),tail(ab.24.row$b1,1),tail(ab.27.row$b1,1),tail(ab.30.row$b1,1),
            tail(ab.33.row$b1,1),tail(ab.36.row$b1,1),tail(ab.39.row$b1,1),tail(ab.42.row$b1,1))
  
  b2.row<-c(tail(ab.1.row$b2,1),tail(ab.2.row$b2,1),tail(ab.3.row$b2,1),tail(ab.6.row$b2,1),
            tail(ab.9.row$b2,1),tail(ab.12.row$b2,1),tail(ab.15.row$b2,1),tail(ab.18.row$b2,1),
            tail(ab.21.row$b2,1),tail(ab.24.row$b2,1),tail(ab.27.row$b2,1),tail(ab.30.row$b2,1),
            tail(ab.33.row$b2,1),tail(ab.36.row$b2,1),tail(ab.39.row$b2,1),tail(ab.42.row$b2,1))
  
  
  
  ab.result.row<-data.frame(a1.row,a2.row,a3.row,a4.row,a5.row,b1.row,b2.row)
  
  
  sort(table(ab.result.row$a1.row))
  sort(table(ab.result.row$a2.row))
  sort(table(ab.result.row$a3.row))
  sort(table(ab.result.row$a4.row))
  sort(table(ab.result.row$a5.row))
  sort(table(ab.result.row$b1.row))
  sort(table(ab.result.row$b2.row))
  
  
  return(ab.result.row)
}






