#attach(dlt)
#library(Rcmdr)
Sys.setlocale(category = "LC_CTYPE", locale = "chs")
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(mongolite)
library(lubridate)
library(gridExtra)
library(RMariaDB)
library(RMySQL)
library(DBI)
library(data.table)
library(foreach)
library("doParallel")

med_meeting<-dbConnect(MySQL(),host='rm-2zet5lw17as40fty28o.mysql.rds.aliyuncs.com',port=3306,
                      dbname='medo_master',user='snowball',password= 'MEDOsnow$%^&')


dbListTables(med_meeting)

dbSendQuery(med_meeting,'SET NAMES gbk')

rt_index<-as.data.frame(dbReadTable(med_meeting,"medmeeting_index"))
rt_raw<-as.data.frame(dbReadTable(med_meeting,"medmeeting_raw"))
rt_datil<-as.data.frame(dbReadTable(med_meeting,"medmeeting_detail"))

write.csv(rt_index,file = "rt_index.csv")
write.csv(rt_raw,file = "rt_raw.csv")
write.csv(rt_datil,file = "rt_datil.csv")

dbGetQuery(med_meeting,"select Event_ID from medmeeting_index")

Event_ID_detail<-names(table(rt_datil$Event_ID))
Event_ID__index<-rt_index$Event_ID

temp_id<-0
for ( id in rt_datil$Event_ID) {
  if (length(rt_index[which(rt_index$Event_ID==id),]$title)>0) {
    
  }
  else {
    temp_id<-c(temp_id,id)
  }
}
temp_id<-temp_id[-1]
names(table(temp_id))


dbDisconnect(med_meeting)


write.csv(table(rt_datil$session_topic),"t_sess.csv",row.names=FALSE)

event_title<-rt_datil[which(rt_datil$name=="-"),]$event_title


foreach(event_title) {
  print("line\n")
}







