detail_name<-mdm_datil[which(mdm_datil$DID==""),]$name
table_name<-as.data.frame(table(detail_name))
single_name<-table_name[which(table_name$Freq>1),]$detail_name
single_name<-single_name[-1]

temp_line<-matrix(0,1,16)
for (nam in single_name) {
  #print(nam)
  #print(mdm_datil[which(mdm_datil$name==nam),])
  dup_DID<-as.matrix(table(mdm_datil[which(mdm_datil$name==nam),]$DID))
  if (dim(dup_DID)[1]>=2) {
    temp_line<-rbind(temp_line,as.matrix(mdm_datil[which(mdm_datil$name==nam),])) 
  }
  
}
write.csv(temp_line[-1,],"med_n_D.csv")
