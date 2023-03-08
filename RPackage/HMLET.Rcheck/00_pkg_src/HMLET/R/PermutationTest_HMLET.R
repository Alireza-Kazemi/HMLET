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
#' @return a list of permutation tests in which distribution of desired statistic under the null hypothesis is estimated in a large number of permutations of the original data
#' @export
#'
PermutationTest_HMLET <- function(data, samples = 2000, paired = T, permuteTrialsWithinSubject = F,   threshold_t = NA){
  if(is.factor(data$timepoint)){
    data$timeBin = data$timepoint
    data$timepoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
    warning(paste("\n    >> Timepoints are converted to numeric indexes: ",
                  "\n    >> ",c(paste(levels(data$timeBin),1:length(levels(data$timeBin)), sep = " -> ", collapse = "   ")), sep = ""))
  }
  datSave = data

  clusterInfAll = NULL
  tValueDistAll = NULL
  tValuesAll    = NULL
  dataAll       = NULL

  for(testName in unique(datSave$testName)){
    print(paste("          -------->","Running permutation test for:",testName,"<--------"))
    data = datSave[datSave$testName == testName,]

    if(is.na(threshold_t)){
      # Treshold should be based on the permutation sampling
      num_sub = length(unique(data$ID))
      threshold_t = qt(p=1-.05/2, df=num_sub-1)
    }
    data = RemoveIncompleteTimePoints_HMLET(data)


    res = ClusterStats_HMLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
    clusterInf = res[[1]]
    tValues = res[[2]]

    if(nrow(clusterInf)>0){
      print("Creating unique permutation labels:")
      if(permuteTrialsWithinSubject){
        tValueDist = TrialLevelPermutationTestWithin_HMLET(data, samples = samples, paired = paired, threshold_t = threshold_t)

      }else{
        tValueDist = SubjectLevelPermutationTestWithin_HMLET(data, samples = samples, paired = paired, threshold_t = threshold_t)
      }
      tValueDist$testName = unique(testName)
      tValueDist = tValueDist[,c("testName",names(tValueDist)[names(tValueDist)!="testName"])]

      clusterInf$pValue = unique(NA)
      for(i in 1:nrow(clusterInf)){
        clusterInf$pValue[i]=mean(as.numeric(abs(tValueDist$NullDist)>abs(clusterInf$tStatistic[i])))
      }
    }else{
      clusterInf = NULL
      tValueDist = NULL
    }

    clusterInfAll = rbind(clusterInfAll, clusterInf)
    tValueDistAll = rbind(tValueDistAll, tValueDist)
    tValuesAll    = rbind(tValuesAll, tValues)
    dataAll       = rbind(dataAll, data)
  }
  return(list(clusterInfAll, tValueDistAll, tValuesAll, samples, dataAll))
}
