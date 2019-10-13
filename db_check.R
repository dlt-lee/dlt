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
library(DBI)
library(data.table)


med_meeting<-dbConnect(MySQL(),host='rm-2zet5lw17as40fty28o.mysql.rds.aliyuncs.com',port=3306,
                      dbname='medo_master',user='snowball',password= 'MEDOsnow$%^&')


dbListTables(med_meeting)

dbSendQuery(med_meeting,'SET NAMES gbk')

rt_index<-as.data.frame(dbReadTable(med_meeting,"medmeeting_index"))
rt_raw<-as.data.frame(dbReadTable(med_meeting,"medmeeting_raw"))
rt_datil<-as.data.frame(dbReadTable(med_meeting,"medmeeting_raw"))

write.csv(rt_index,file = "rt_index.csv")
write.csv(rt_raw,file = "rt_raw.csv")
write.csv(rt_datil,file = "rt_datil.csv")







dbDisconnect(med_meeting)


=