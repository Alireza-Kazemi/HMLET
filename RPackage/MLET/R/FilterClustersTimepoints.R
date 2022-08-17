FilterClustersTimepoints <- function(data, clusterInf = clusterInf){
  data$ClusterIdx = unique(NA)
  clusterInf$ClusterIdx = paste(clusterInf$Direction, clusterInf$index, sep = "_")
  for (cIdx in 1:nrow(clusterInf)){
    data$ClusterIdx[data$timepoint>=clusterInf$timeStart[cIdx] & 
                      data$timepoint<=clusterInf$timeEnd[cIdx]] =
      clusterInf$ClusterIdx[cIdx]
  }
  return(data)
}