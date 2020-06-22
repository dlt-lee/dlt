library("xlsx")
file_csv = "D:\\Downloads\\RechercheExport.csv"
file_xlsx = "D:\\Downloads\\RechercheExport .xlsx"
tickets_org<-data.frame(read.csv2(file_csv, header=TRUE, sep=";"))
write.csv2(tickets_org[,c(1,2,49,55,59,60,3,4,5,6,7,8,9,10,
                      11,12,13,14,15,16,17,18,19,20,
                      21,22,23,24,25,26,27,28,29,30,
                      31,32,33,34,35,36,37,38,39,40,
                      41,42,43,44,45,46,47,48,50,51,
                      52,53,54,56,57,58,61,62,63,64,
                      65,66,67,68,69,70,71,72,73,74,
                      75,76,77,78)],file_csv)

