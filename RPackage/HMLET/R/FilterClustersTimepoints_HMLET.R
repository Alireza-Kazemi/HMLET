#' not being used.
#'
#'@export
FilterClusterstimePoints_HMLET <- function(data, clusterInf = clusterInf){
  data$ClusterIdx = unique(NA)
  clusterInf$ClusterIdx = paste(clusterInf$Direction, clusterInf$index, sep = "_")
  for (cIdx in 1:nrow(clusterInf)){
    data$ClusterIdx[data$timePoint>=clusterInf$timeStart[cIdx] &
                      data$timePoint<=clusterInf$timeEnd[cIdx]] =
      clusterInf$ClusterIdx[cIdx]
  }
  return(data)
}
