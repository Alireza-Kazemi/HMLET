EstimateNullDist_MLET <- function(tValues, clusterTimepoint, tStatMax){
  tValues = merge(tValues,clusterTimepoint, by = "timepoint")
  tValues = as.data.frame(summarise(group_by(tValues,ClusterIdx), tStat = sum(value)))
  
  tValues$Positive = unique(0)
  tValues$Negative = unique(0)
  tValues$Positive[tValues$tStat>tStatMax]=1
  tValues$Negative[tValues$tStat<(-1*tStatMax)]=1

  return(tValues)
}
