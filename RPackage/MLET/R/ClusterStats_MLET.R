#' Generate unique random permutations
#'
#' @export

ClusterStats_MLET <- function(data, paired = T, detailed = F, threshold_t = NA){
  datSave = data

  clusterInfAll = NULL
  tValuesAll    = NULL

  for(testName in unique(datSave$testName)){
    data = datSave[datSave$testName == testName,]

    data = RemoveIncompleteTimePoints_MLET(data)
    if(is.na(threshold_t)){
      num_sub = length(unique(data$ID))
      threshold_t = qt(p=1-.05/2, df=num_sub-1)
    }
    resp_time = as.data.frame(summarise(group_by(data, ID, timepoint, condition), prop = mean(AOI, na.rm = T)))
    tValues = ComputeTValues_MLET(resp_time, paired = paired)
    tValues = FindClusters_MLET(tValues, threshold_t = threshold_t)
    tValues$testName = unique(data$testName)
    tValues = tValues[,c("testName",names(tValues)[names(tValues)!="testName"])]

    clusterInf = melt(tValues,id.vars = c("testName","timepoint","value"),variable.name = "Direction", value.name = "index")
    clusterInf = clusterInf[clusterInf$index!=0,]
    clusterInf = as.data.frame(summarise(group_by(clusterInf, testName, Direction, index),
                                         tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))

    clusterInfAll = rbind(clusterInfAll, clusterInf)
    tValuesAll    = rbind(tValuesAll, tValues)
  }

  if (detailed){
	  return(list(clusterInfAll, tValuesAll))
  }else{
	  return(clusterInfAll)
  }
}
