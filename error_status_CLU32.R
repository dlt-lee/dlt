library("xlsx")
file_csv = "D:\\Downloads\\RechercheExport.csv"
file_xlsx = "D:\\Downloads\\ticks_clu32.xlsx"
tickets_org<-data.frame(read.csv2(file_csv, header=TRUE, sep=";"))
#library("Rcmdr")
tickets_clu32<-subset(tickets_org,tickets_org$Implementation.date=="KW: 32-2381" | 
                                  tickets_org$Implementation.date=="KW: 32-2473" |
                                  tickets_org$Implementation.date=="KW: 32-2479" |
                                  tickets_org$Implementation.date=="KW: 32-2491" |
                                  tickets_org$Implementation.date=="KW: 32-2492" |
                                  tickets_org$Implementation.date=="KW: 32-2493" |
                                  tickets_org$Implementation.date=="KW: 32-2497" |
                                  tickets_org$Implementation.date=="KW: 32-2947")
wb = createWorkbook()
sheet = createSheet(wb, "clu32")
addDataFrame(tickets_clu32, sheet=sheet,col.names = TRUE,row.names=FALSE)
saveWorkbook(wb, file_xlsx)

