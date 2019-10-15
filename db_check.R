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

#connect ot Mysql 
DB_mysql<-dbConnect(MySQL(),host='rm-2zet5lw17as40fty28o.mysql.rds.aliyuncs.com',port=3306,
                      dbname='medo_master',user='snowball',password= 'MEDOsnow$%^&')

#List table in MysQL
dbListTables(DB_mysql)

#Setquery to support chinese
dbSendQuery(DB_mysql,'SET NAMES gbk')

#Read data from DB and save as data.frame
mdm_index<-as.data.frame(dbReadTable(DB_mysql,"medmeeting_index"))
mdm_raw<-as.data.frame(dbReadTable(DB_mysql,"medmeeting_raw"))
mdm_datil<-as.data.frame(dbReadTable(DB_mysql,"medmeeting_detail"))

#Save data as local file
write.csv(mdm_index,file = "mdm_index.csv")
write.csv(mdm_raw,file = "mdm_raw.csv")
write.csv(mdm_datil,file = "mdm_datil.csv")
#########################################################################################
#disconnect DB
dbDisconnect(DB_mysql)
#########################################################################################
#Query data by sql
dbGetQuery(DB_mysql,"select Event_ID from medmeeting_index")
########################################################################################
# case 
# check leak event_id from mdm_datil to mdm_index
temp_id<-0
for ( id in mdm_datil$Event_ID) {
  if (length(mdm_index[which(mdm_index$Event_ID==id),]$title)>0) {}
  else {
    temp_id<-c(temp_id,id)
  }
}
temp_id<-temp_id[-1]
names(table(temp_id))
########################################################################################



write.csv(table(rt_datil$session_topic),"t_sess.csv",row.names=FALSE)

event_title<-rt_datil[which(rt_datil$name=="-"),]$event_title








