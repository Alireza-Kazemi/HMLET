PermutationTest_MLET <- function(data, samples = 2000, paired = T, permuteTrialsWithinSubject = F,   threshold_t = NA){
  
  res = ClusterStats_MLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
  clusterInf = res[[1]]
  tValues = res[[2]]
  
  if(nrow(clusterInf)<1) {stop("No Cluster is found based on this the current threshold")}
  
  if(is.na(threshold_t)){
    # Treshold should be based on the permutation sampling
    num_sub = length(unique(data$ID))  
    threshold_t = qt(p=1-.05/2, df=num_sub-1)
  }
  print("Creating unique permutation labels:")
  if(permuteTrialsWithinSubject){
    tValueDist = TrialLevelPermutationTestWithin_MLET(data, clusterInf = clusterInf, samples = samples, paired = paired, threshold_t = threshold_t)
    
  }else{
    tValueDist = SubjectLevelPermutationTestWithin_MLET(data, clusterInf = clusterInf, samples = samples, paired = paired, threshold_t = threshold_t)
  }
  
  
	
  clusterInf$pValue = unique(NA)
  for(i in 1:nrow(clusterInf)){
    if(clusterInf$tStatistic[i]>=0){
      clusterInf$pValue[i]=mean(as.numeric(tValueDist$NullDist>clusterInf$tStatistic[i]))
    }else{
      clusterInf$pValue[i]=mean(as.numeric(tValueDist$NullDist<clusterInf$tStatistic[i]))
    }
  }
  
  return(list(clusterInf, tValueDist, tValues, samples))
}