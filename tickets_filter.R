library("xlsx")
file_csv = "D:\\Downloads\\RechercheExport.csv"
file_xlsx = "D:\\Downloads\\RechercheExport .xlsx"
tickets<-data.frame(read.csv2(file_csv, header=TRUE, sep=";"))

tickets<-subset(tickets,tickets$Part.no...causing.=="8WD 919 806" |
                  tickets$Part.no...causing.=="8WD 919 806 A")
tickets<-tickets[which(tickets$Engineering.status != 3),]
tickets<-tickets[which(tickets$Engineering.status != 4),]
tickets<-tickets[which(tickets$Engineering.status != 5),]
tickets<-tickets[which(tickets$Engineering.status != 6),]
tickets<-tickets[which(tickets$Status != 6),]
tickets<-tickets[which(tickets$L.Status.1 != "solved"),]
tickets_EB<-tickets[which(tickets$supplier == "FF EB-AED"),]
tickets_ESOL<-tickets[which(tickets$supplier == "FF ESOL-INF"),]

wb = createWorkbook()
sheet = createSheet(wb, "RechercheExport")
addDataFrame(tickets, sheet=sheet,col.names = TRUE,row.names=FALSE)
sheet = createSheet(wb, "EB")
addDataFrame(tickets_EB, sheet=sheet,col.names = TRUE,row.names=FALSE)
sheet = createSheet(wb, "ESOL")
addDataFrame(tickets_ESOL, sheet=sheet,col.names = TRUE,row.names=FALSE)
saveWorkbook(wb, file_xlsx)
