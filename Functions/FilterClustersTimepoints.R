FilterClustersTimepoints <- function(data, clusterInf){
  data$ClusterIdx = unique("None")
  clusterInf$ClusterIdx = paste(clusterInf$Direction, clusterInf$index, sep = "_")
  for (cIdx in 1:nrow(clusterInf)){
    data$ClusterIdx[data$timepoint>=clusterInf$timeStart[cIdx] & 
                      data$timepoint<=clusterInf$timeEnd[cIdx]] =
      clusterInf$ClusterIdx[cIdx]
  }
  clusterMax = clusterInf$ClusterIdx[order(abs(clusterInf$tStatistic),decreasing = T)[1]]
  return(list(data, clusterMax))
}