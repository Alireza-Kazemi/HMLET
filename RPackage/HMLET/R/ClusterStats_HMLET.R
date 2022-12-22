#' Adds cluster statistics to t value dataframe, includes all info for all tests.
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param detailed optional boolean to include cluster statistics and t value statistics within one dataframe.
#'                 Defaults to False to populate dataframe only with cluster statistics.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @param threshold_t optional probability threshold for statistical comparison computed based on number of subjects.
#'                    alpha = 0.025.
#' @return returns data frame with cluster statistics
#' @export

ClusterStats_HMLET <- function(data, paired = T, detailed = F, threshold_t = NA){
  if(is.factor(data$timepoint)){
    data$timeBin = data$timepoint
    data$timepoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
    warning(paste("\n    >> Timepoints are converted to numeric indexes: ",
                  "\n    >> ",c(paste(levels(data$timeBin),1:length(levels(data$timeBin)), sep = " -> ", collapse = "   ")), sep = ""))
  }


  datSave = data

  clusterInfAll = NULL
  tValuesAll    = NULL

  for(testName in unique(datSave$testName)){
    data = datSave[datSave$testName == testName,]

    data = RemoveIncompleteTimePoints_HMLET(data)
    if(is.na(threshold_t)){
      num_sub = length(unique(data$ID))
      threshold_t = qt(p=1-.05/2, df=num_sub-1)
    }
    resp_time = as.data.frame(summarise(group_by(data, ID, timepoint, condition), prop = mean(AOI, na.rm = T)))
    tValues = ComputeTValues_HMLET(resp_time, paired = paired)
    tValues = FindClusters_HMLET(tValues, threshold_t = threshold_t)

    sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
    sdat = sdat[sdat$index!=0,]
    if(nrow(sdat)!=0){
      sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value, na.rm=T)))
      tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
    }else{
      tValueTemp = data.frame(Positive =  0, Negative =0)
    }

    tValues$testName = unique(data$testName)
    tValues = tValues[,c("testName",names(tValues)[names(tValues)!="testName"])]

    clusterInf = melt(tValues,id.vars = c("testName","timepoint","value"),variable.name = "Direction", value.name = "index")
    clusterInf = clusterInf[clusterInf$index!=0,]
    if(nrow(clusterInf)==0){
      warning(paste("\n    >> In test data: ",unique(data$testName),
                    "\n    >> No significant cluster found based on threshold Alpha = ",
                    threshold_t, sep = ""))
    }
    else{
    clusterInf = as.data.frame(summarise(group_by(clusterInf, testName, Direction, index),
                                         tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
    }

    clusterInfAll = rbind(clusterInfAll, clusterInf)
    tValuesAll    = rbind(tValuesAll, tValues)
  }

  if (detailed){
	  return(list(clusterInfAll, tValuesAll))
  }else{
	  return(clusterInfAll)
  }
}
