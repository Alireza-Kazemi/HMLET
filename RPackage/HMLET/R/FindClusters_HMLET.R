#' Cluster all samples whose t value is greater than threshold value by temporal adjacency.
#'
#' @param tValues dataframe of t values calculated from ComputeTValues_HMLET.
#' @param threshold_t optional probability threshold for statistical comparison, defaults to NA and will be computed based on number of trials when "between trials
#'                    permutation" is called or will be computed based on number of subjects when "between subjects permutation" is called.
#'                    Alpha = 0.025.
#' @return t value dataframe with appended cluster information.
#'
#'@export
FindClusters_HMLET <- function(tValues, threshold_t = threshold_t){
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

  tValues = tValues[,c("timePoint","value","Positive","Negative")]
  return(tValues)
}
