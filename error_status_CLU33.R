library("xlsx")
file_csv = "C:\\Downloads\\RechercheExport.csv"
file_csv2 = "C:\\Downloads\\clu33_org.csv"
file_xlsx = "C:\\Downloads\\ticks_clu33.xlsx"
tickets_org<-data.frame(read.csv2(file_csv, header=TRUE, sep=";"))
###################################
wb = createWorkbook()
sheet = createSheet(wb, "All")
addDataFrame(tickets_org[,c(1,2,3,4,9,10,11,12,16,36,
                            38,39,40,48,49,53.59,60)], 
             sheet=sheet,col.names = TRUE,row.names=FALSE)
###################################
table(tickets_org$Implementation.date)
tickets_clu33_org<-subset(tickets_org,
                        tickets_org$Implementation.date=="KW: 33-2387" |
                        tickets_org$Implementation.date=="KW: 33-2492" |
                        tickets_org$Implementation.date=="KW: 33-2493" |
                        tickets_org$Implementation.date=="KW: 33-2497" |
                        tickets_org$Implementation.date=="KW: 33-2517" |
                        tickets_org$Implementation.date=="KW: 33-2653" |
                        tickets_org$Implementation.date=="KW: 33-2657" |
                        tickets_org$Implementation.date=="KW: 33-4297")
sheet = createSheet(wb, "clu33")
addDataFrame(tickets_clu33_org[,c(1,2,3,4,9,10,11,12,16,36,
                            38,39,40,48,49,53.59,60)], 
             sheet=sheet,col.names = TRUE,row.names=FALSE)
saveWorkbook(wb, file_xlsx)

write.csv2(tickets_clu33_org[,c(1,2,3,4,9,10,11,12,16,36,
                                38,39,40,48,49,53.59,60)],file_csv2)
tickets_clu33<-data.frame(read.csv2(file_csv2,header=TRUE, sep=";"))
clu33_open<-subset(tickets_clu33,tickets_clu33$Engineering.status==0 |
                                tickets_clu33$Engineering.status==1 |
                              tickets_clu33$Engineering.status==2)
table(tickets_clu33$Engineering.status)
table(clu33_open$VBV)
table(tickets_clu33$Responsible.Problem.Solver.User)
table(clu33_open$supplier)


