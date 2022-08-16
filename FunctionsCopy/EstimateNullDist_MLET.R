EstimateNullDist_MLET <- function(tValues, clusterTimepoint, tStatMax){
  tValues = merge(tValues,clusterTimepoint, by = "timepoint")
  tValues = summarise(group_by(tValues,ClusterIdx), tStat = sum(value))
  
  tValues$Positive = unique(0)
  tValues$Negative = unique(0)
  tValues$Positive[tValues$value>threshold_t]=1
  tValues$Negative[tValues$value<(-1*threshold_t)]=1
  
  tValues$temp = ifelse(diff(c(0,tValues$Negative))==1,1,0)
  tValues$temp = cumsum(tValues$temp)
  tValues$Negative = tValues$Negative*tValues$temp
  
  tValues$temp = ifelse(diff(c(0,tValues$Positive))==1,1,0)
  tValues$temp = cumsum(tValues$temp)
  tValues$Positive = tValues$Positive*tValues$temp
  
  tValues = tValues[,c("timepoint","value","Positive","Negative")]
  return(tValues)
}
