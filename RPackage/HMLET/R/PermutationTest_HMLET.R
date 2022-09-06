#' Permutation Tests General Routine
#'
#'
#' @import dplyr
#' @import miceadds
#' @import purrr
#' @import rray
#' @import tidyr
#' @import combinat
#' @export
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
