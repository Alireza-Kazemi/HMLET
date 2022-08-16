PermutationTest_MLET <- function(data, samples = 2000, paired = T, permuteTrialsWithinSubject = F,   threshold_t = NA){
  if(is.na(threshold_t)){
    # Treshold should be based on the permutation sampling
    num_sub = length(unique(data$ID))  
    threshold_t = qt(p=1-.05/2, df=num_sub-1)
  }
  print("Creating unique permutation labels:")
  if(permuteTrialsWithinSubject){
    tValueDist = TrialLevelPermutationTestWithin_MLET(data, samples = samples, paired = paired, threshold_t = threshold_t)
    
  }else{
    tValueDist = SubjectLevelPermutationTestWithin_MLET(data, samples = samples, paired = paired, threshold_t = threshold_t)
  }
  
  res = ClusterStats_MLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
  sdat = res[[1]]
  tValues = res[[2]]
	
  sdat$pValue = unique(NA)
  for(i in 1:nrow(sdat)){
    if(sdat$tStatistic[i]>=0){
      sdat$pValue[i]=mean(as.numeric(tValueDist$NullDist>sdat$tStatistic[i]))
    }else{
      sdat$pValue[i]=mean(as.numeric(tValueDist$NullDist<sdat$tStatistic[i]))
    }
  }
  
  return(list(sdat, tValueDist, tValues, samples))
}