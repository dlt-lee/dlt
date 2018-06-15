dlt.d2c <- function(data,ch) {
  a.class<-
    c("a.class01","a.class02","a.class03","a.class04","a.class05","a.class06","a.class07",
      "a.class08","a.class09","a.class10","a.class11","a.class12","a.class13","a.class14",
      "a.class15","a.class16","a.class17","a.class18","a.class19","a.class20","a.class21",
      "a.class22","a.class23","a.class24","a.class25","a.class26","a.class27","a.class28",
      "a.class29","a.class30","a.class31","a.class32","a.class33","a.class34","a.class35")
  b.class <-
    c("b.class01","b.class02","b.class03","b.class04","b.class05","b.class06",
       "b.class07","a.class08","a.class09","a.class10","a.class11","a.class12")
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

