dlt_sum_L1<-function(data) {
  data_filter<-dlt.data.reset(data)
  
  data_clu2<-dlt.xgb.clu.2(data_filter,data)
  data_clu2.a<-sort(as.matrix(data_clu2)[1:5])
  data_clu2.b<-sort(as.matrix(data_clu2)[6:7])
  data_clu2<-append(data_clu2.a,data_clu2.b)
  
  data_clu3<-dlt.xgb.clu.3(data_filter,data)
  data_clu3.a<-sort(as.matrix(data_clu3)[1:5])
  data_clu3.b<-sort(as.matrix(data_clu3)[6:7])
  data_clu3<-append(data_clu3.a,data_clu3.b)
  
  data_clu6<-dlt.xgb.clu.6(data_filter,data)
  data_clu6.a<-sort(as.matrix(data_clu6)[1:5])
  data_clu6.b<-sort(as.matrix(data_clu6)[6:7])
  data_clu6<-append(data_clu6.a,data_clu6.b)
  
  data_clu9<-dlt.xgb.clu.9(data_filter,data)
  data_clu9.a<-sort(as.matrix(data_clu9)[1:5])
  data_clu9.b<-sort(as.matrix(data_clu9)[6:7])
  data_clu9<-append(data_clu9.a,data_clu9.b)
  
  data_row1<-dlt.xgb.row.1(data_filter,data)
  data_row1.a<-sort(as.matrix(data_row1)[1:5])
  data_row1.b<-sort(as.matrix(data_row1)[6:7])
  data_row1<-append(data_row1.a,data_row1.b)
  
  data_row3<-dlt.xgb.row.3(data_filter,data)
  data_row3.a<-sort(as.matrix(data_row3)[1:5])
  data_row3.b<-sort(as.matrix(data_row3)[6:7])
  data_row3<-append(data_row3.a,data_row3.b)
  
  data_row6<-dlt.xgb.row.6(data_filter,data)
  data_row6.a<-sort(as.matrix(data_row6)[1:5])
  data_row6.b<-sort(as.matrix(data_row6)[6:7])
  data_row6<-append(data_row6.a,data_row6.b)
  
  data_row9<-dlt.xgb.row.9(data_filter,data)
  data_row9.a<-sort(as.matrix(data_row9)[1:5])
  data_row9.b<-sort(as.matrix(data_row9)[6:7])
  data_row9<-append(data_row9.a,data_row9.b)
  
  a1<-c(data_clu2[1],data_clu3[1],data_clu6[1],data_clu9[1],data_row1[1],data_row3[1],data_row6[1],data_row9[1])
  a2<-c(data_clu2[2],data_clu3[2],data_clu6[2],data_clu9[2],data_row1[2],data_row3[2],data_row6[2],data_row9[2])
  a3<-c(data_clu2[3],data_clu3[3],data_clu6[3],data_clu9[3],data_row1[3],data_row3[3],data_row6[3],data_row9[3])
  a4<-c(data_clu2[4],data_clu3[4],data_clu6[4],data_clu9[4],data_row1[4],data_row3[4],data_row6[4],data_row9[4])
  a5<-c(data_clu2[5],data_clu3[5],data_clu6[5],data_clu9[5],data_row1[5],data_row3[5],data_row6[5],data_row9[5])
  b1<-c(data_clu2[6],data_clu3[6],data_clu6[6],data_clu9[6],data_row1[6],data_row3[6],data_row6[6],data_row9[6])
  b2<-c(data_clu2[7],data_clu3[7],data_clu6[7],data_clu9[7],data_row1[7],data_row3[7],data_row6[7],data_row9[7])
  
  print(sort(table(a1)))
  print(sort(table(a2)))
  print(sort(table(a3)))
  print(sort(table(a4)))
  print(sort(table(a5)))
  print(sort(table(b1)))
  print(sort(table(b2)))
  
  data_result<-data.frame(a1,a2,a3,a4,a5,b1,b2)
  return(data_result)
  
  
  
  
}






























