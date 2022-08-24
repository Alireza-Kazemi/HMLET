#' Generate unique random permutations
#'
#' @export

ClusterStats_MLET <- function(data, paired = T, detailed = F, threshold_t = NA){
  data = RemoveIncompleteTimePoints_MLET(data)
  if(is.na(threshold_t)){
    num_sub = length(unique(data$ID))
    threshold_t = qt(p=1-.05/2, df=num_sub-1)
  }
  resp_time = as.data.frame(summarise(group_by(data, ID, timepoint, condition), prop = mean(AOI, na.rm = T)))
  tValues = ComputeTValues_MLET(resp_time, paired = paired)
  tValues = FindClusters_MLET(tValues, threshold_t = threshold_t)
  clusterInf = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
  clusterInf = clusterInf[clusterInf$index!=0,]
  clusterInf$testName = unique(data$testName)

  if (detailed){
    clusterInf = as.data.frame(summarise(group_by(clusterInf,Direction,index),tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
	return(list(clusterInf, tValues))
  }else{
    clusterInf = as.data.frame(summarise(group_by(clusterInf, Direction, index), tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
	return(clusterInf)
  }
}
