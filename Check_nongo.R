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

#Setquery to support chinese
dbSendQuery(DB_mysql,'SET NAMES gbk')


#List table in MysQL
dbListTables(DB_mysql)


#Read data from DB and save as data.frame
manual_association<-as.data.frame(dbReadTable(DB_mysql,"manual_association"))
manual_event<-as.data.frame(dbReadTable(DB_mysql,"manual_event"))
manual_guide<-as.data.frame(dbReadTable(DB_mysql,"manual_guide"))

#Save data as local file
write.csv(manual_association,file = "manual_association.csv",row.names=FALSE)
write.csv(manual_event,file = "manual_event.csv",row.names=FALSE)
write.csv(manual_guide,file = "manual_guide.csv",row.names=FALSE)


List_AN<-names(table(manual_guide$Guide_Name))
c_name<-c(0)
for (an in List_AN) {
  guide<-manual_guide[which(manual_guide$Guide_Name==an),]
  #print(event$Event_Name)
  Fre_type<-as.data.frame(table(guide$Guide_Type))
  if (length(Fre_type$Var1)>1) {
    c_name<-c(c_name,guide$Guide_Name)
    
  }
  
}
names(table(c_name[-1]))


list_event<-names(table(manual_event$Event_Name))
for (eventname in list_event) {
  event<-manual_event[which(manual_event$Event_Name==eventname),]
  Fre_sponsor<-as.data.frame(table(event$Event_Sponsor))
  if (length(Fre_sponsor$Var1)==1) {
    print(eventname)
  }
}





########################################################################################
#disconnect DB
dbDisconnect(DB_mysql)
#########################################################################################




