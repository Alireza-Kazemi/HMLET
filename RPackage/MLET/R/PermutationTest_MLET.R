#' Permutation Test General Routine
#'
#'
#' @import dplyr
#' @import miceadds
#' @import purrr
#' @import rray
#' @import tidyr
#' @import utils
#' @import combinat
#' @export
PermutationTest_MLET <- function(data, samples = 2000, paired = T, permuteTrialsWithinSubject = F,   threshold_t = NA){
  if(is.na(threshold_t)){
    # Treshold should be based on the permutation sampling
    num_sub = length(unique(data$ID))
    threshold_t = qt(p=1-.05/2, df=num_sub-1)
  }


  res = ClusterStats_MLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
  clusterInf = res[[1]]
  tValues = res[[2]]

  if(nrow(clusterInf)<1) {stop("No Cluster is found based on this the current threshold")}

  print("Creating unique permutation labels:")
  if(permuteTrialsWithinSubject){
    tValueDist = TrialLevelPermutationTestWithin_MLET(data, samples = samples, paired = paired, threshold_t = threshold_t)

  }else{
    tValueDist = SubjectLevelPermutationTestWithin_MLET(data, samples = samples, paired = paired, threshold_t = threshold_t)
  }



  clusterInf$pValue = unique(NA)
  for(i in 1:nrow(clusterInf)){
    clusterInf$pValue[i]=mean(as.numeric(abs(tValueDist$NullDist)>abs(clusterInf$tStatistic[i])))
    # if(clusterInf$tStatistic[i]>=0){
    #   clusterInf$pValue[i]=mean(as.numeric(tValueDist$NullDist>clusterInf$tStatistic[i]))
    # }else{
    #   clusterInf$pValue[i]=mean(as.numeric(tValueDist$NullDist<clusterInf$tStatistic[i]))
    # }
  }

  return(list(clusterInf, tValueDist, tValues, samples))
}
