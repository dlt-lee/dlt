data<-dlt
count<-dim(dlt)[1]

result.caret_cubist  <-dlt.caret_cubist(data,count)
result.cubist        <-dlt.cubist(data,count)
result.caret_gbm     <-dlt.caret_gbm(data,count)
result.caret_cf      <-dlt.caret_cf(data,count)
result.caret_rf      <-dlt.caret_rf(data,count)
result.ipred_bag     <-dlt.ipred_bag(data,count)
result.caret_treebag <-dlt.caret_treebag(data,count)
result.caret_ctree   <-dlt.caret_ctree(data,count)
result.caret_repart  <-dlt.caret_repart(data,count)
result.caret_knn     <-dlt.caret_knn(data,count)
result.caret_earth   <-dlt.caret_earth(data,count)
result.caret_svm     <-dlt.caret_svm(data,count)
result.caret_glm     <-dlt.caret_glm(data,count)
#result.dlt.caret_nnet<-dlt.caret_nnet(data,count)


#c(tail(result.caret_cubist[1],1),
#  tail(result.cubist[1],1),
#  tail(result.caret_gbm[1],1),
#  tail(result.caret_cf[1],1),
#  tail(result.caret_rf[1],1),
#  tail(result.ipred_bag[1],1),
#  tail(result.caret_treebag[1],1),
#  tail(result.caret_ctree[1],1),
#  tail(result.caret_repart[1],1),
#  tail(result.caret_knn[1],1),
#  tail(result.caret_earth[1],1),
#  tail(result.caret_svm[1],1),
#  tail(result.caret_glm[1],1),
#  tail(result.dlt.caret_nnet[1],1)
#  )










