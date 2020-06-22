dlt.convert <- function(number,data,c) {
  rows<-dim(data)[1]
  clu<-dim(data)[2]
  #ifelse(c==1,n<-35,n<-12)
  switch(c,n<-35,n<-12,n<-clu*6) 
  if (c==1|c==2) {
    m_dlt<-matrix(data = 0,nrow = rows,ncol = n)
    for (i in 1:rows){
      for (j in 1:clu){
        m_dlt[i,data[i,j]]<-1
      }
    }
  }
  if (c==3) {
    #    m_dlt<-matrix(data = 0,nrow = rows,ncol = n)
    t_data<-0
    for (i in 1:rows){
      for (j in 1:clu){
        t_data<-c(t_data,rev(as.integer(intToBits(data[i,j])))[27:32])
      }
    }
    t_data<-t_data[2:length(t_data)]
    m_dlt<-matrix(data = t_data,nrow = rows,ncol = n)
  }
  
  m<-cbind(number,m_dlt)
  return(m)
}

