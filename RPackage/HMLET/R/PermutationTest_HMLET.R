#' Permutation Tests General Routine.
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param samples optional integer for number of samples, defaults to 2000.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode, False for a two-sample T-Test; defaults to True.
#' @param permuteTrialsWithinSubject optional boolean to run permutation test pipeline for conducting permutation tests between trials or between subjects, defaults to False.
#' @param threshold_t optional probability threshold for statistical comparison, defaults to NA and will be computed based on number of trials when "between trials
#'                    permutation" is called or will be computed based on number of subjects when "between subjects permutation" is called.
#'                    Alpha = 0.025.
#' @import combinat
#' @import dplyr
#' @import ggplot2
#' @import miceadds
#' @import purrr
#' @import reshape2
#' @import rray
#' @import tidyr
#' @import vdiffr
#' @import stats
#' @import utils
#'
#' @return a list of permutation tests in which distribution of desired statistic under the null hypothesis is estimated in a large number of permutations of the original data
#' @export
#'
PermutationTest_HMLET <- function(data, samples = 2000, paired = T, permuteTrialsWithinSubject = F,   threshold_t = NA){
  if(is.factor(data$timePoint)){
    data$timeBin = data$timePoint
    data$timePoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
    warning(paste("\n    >> timePoints are converted to numeric indexes: ",
                  "\n    >> ",c(paste(levels(data$timeBin),1:length(levels(data$timeBin)), sep = " -> ", collapse = "   ")), sep = ""))
  }
  datSave = data

  clusterInfAll = NULL
  statValueDistAll = NULL
  statValuesAll    = NULL
  dataAll       = NULL

  for(testName in unique(datSave$testName)){
    print(paste("          -------->","Running permutation test for:",testName,"<--------"))
    data = datSave[datSave$testName == testName,]

    if(is.na(threshold_t)){
      # Threshold should be based on the permutation sampling and/or subjects
      num_sub = length(unique(data$ID))
      threshold_t = stats::qt(p=1-.05/2, df=num_sub-1)
    }
    data = RemoveIncompletetimePoints_HMLET(data)


    res = ClusterStats_HMLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
    clusterInf = res[[1]]
    statValues = res[[2]]

    if(nrow(clusterInf)>0){
      print("Creating unique permutation labels:")
      if(permuteTrialsWithinSubject){
        statValueDist = TrialLevelPermutationTestWithin_HMLET(data, samples = samples, paired = paired, threshold_t = threshold_t)

      }else{
        statValueDist = SubjectLevelPermutationTestWithin_HMLET(data, samples = samples, paired = paired, threshold_t = threshold_t)
      }
      statValueDist$testName = unique(testName)
      statValueDist = statValueDist[,c("testName",names(statValueDist)[names(statValueDist)!="testName"])]

      clusterInf$pValue = unique(NA)
      clusterInf$significant = unique("")
      for(i in 1:nrow(clusterInf)){
        clusterInf$pValue[i]=mean(as.numeric(abs(statValueDist$NullDist)>abs(clusterInf$tStatistic[i])))
        clusterInf$significant[i] = ifelse(clusterInf$pValue[i]<0.05,"*",ifelse(clusterInf$pValue[i]<0.1,".",""))
      }
    }else{
      clusterInf = NULL
      statValueDist = NULL
    }

    clusterInfAll = rbind(clusterInfAll, clusterInf)
    statValueDistAll = rbind(statValueDistAll, statValueDist)
    statValuesAll    = rbind(statValuesAll, statValues)
    dataAll       = rbind(dataAll, data)
  }
  return(list(clusterStat = clusterInfAll,
              statNULL = statValueDistAll,
              stastatValues = statValuesAll,
              samples = samples,
              filteredData = dataAll))
}
