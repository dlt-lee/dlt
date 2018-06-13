dlt.adjust <- function(dlt,
                       testPredictions.a1,
                       testPredictions.a2,
                       testPredictions.a3,
                       testPredictions.a4,
                       testPredictions.a5,
                       testPredictions.b1,
                       testPredictions.b2) {
  rows<-length(testPredictions.a1)-1
  data.f.ab<-tail(dlt,rows)
  #A1:
  a1.Predictions<-head(testPredictions.a1,rows)
  #a1.Predictions<-floor(a1.Predictions)
  a1.delta<-data.f.ab$a1-(a1.Predictions)
  barplot(table(a1.delta),main = "a1")
  #A2:
  a2.Predictions<-head(testPredictions.a2,rows)
  #a2.Predictions<-floor(a2.Predictions)
  a2.delta<-data.f.ab$a2-a2.Predictions
  barplot(table(a2.delta),main = "a2") 
  #A3:
  a3.Predictions<-head(testPredictions.a3,rows)
  #a3.Predictions<-floor(a3.Predictions)
  a3.delta<-data.f.ab$a3-a3.Predictions
  barplot(table(a3.delta),main = "a3") 
  #A4:
  a4.Predictions<-head(testPredictions.a4,rows)
  #a4.Predictions<-floor(a4.Predictions)
  a4.delta<-data.f.ab$a4-(a4.Predictions)
  barplot(table(a4.delta),main = "a4") 
  #A5:
  a5.Predictions<-head(testPredictions.a5,rows)
  #a5.Predictions<-floor(a5.Predictions)
  a5.delta<-data.f.ab$a5-(a5.Predictions)
  barplot(table(a5.delta),main = "a5") 
  #B1:
  b1.Predictions<-head(testPredictions.b1,rows)
  #b1.Predictions<-floor(b1.Predictions)
  b1.delta<-data.f.ab$b1-(b1.Predictions)
  barplot(table(b1.delta),main = "b1")
  #B2:
  b2.Predictions<-head(testPredictions.b2,rows)
  #b2.Predictions<-floor(b2.Predictions)
  b2.delta<-data.f.ab$b2-(b2.Predictions)
  barplot(table(b2.delta),main = "b2")
  
}