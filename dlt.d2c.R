dlt.d2c <- function(data,ch) {
  a.class<-
    c("01","02","03","04","05","06","07",
      "08","09","10","11","12","13","14",
      "15","16","17","18","19","20","21",
      "22","23","24","25","26","27","28",
      "29","30","31","32","33","34","35")
  b.class <-
    c("01","02","03","04","05","06",
      "07","08","09","10","11","12")
  ch.temp<-"class0"
  rows<-length(data)
  print(rows)
  for (i in 1:rows) {
    if(ch == 1) {
      a.temp<-a.class[data[i]]
      ch.temp<-c(ch.temp,a.temp)
    }
    if(ch == 2) {
      b.temp<-b.class[data[i]]
      ch.temp<-c(ch.temp,b.temp)
    }
  }
  
  return(ch.temp[-1])
}

