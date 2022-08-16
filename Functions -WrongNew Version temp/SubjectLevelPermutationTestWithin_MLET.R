SubjectLevelPermutationTestWithin_MLET <- function(data, samples = 2000 , paired = T, threshold_c){
  # resp_time = as.data.frame(summarise(group_by(data,ID,timepoint,condition),prop = mean(AOI)))
  resp_time = as.data.frame(summarise(group_by(data, ID, timepoint, condition, 
                                               ClusterIdx),prop = mean(AOI)))
  labels = unique(resp_time[,c("ID","timepoint","condition")])
  labels = ComputeSubjectLevelPerm_MLET(labels, n = samples)
  
  #----------------------------- Perform Permutation tests
  print("Estimate tStatistic distribution:")
  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  
  clusterTimepoint = unique(resp_time[,c("timepoint","ClusterIdx")])
  tValueDist = NULL
  for (itt in 1:(samples)){
    resp_time$condition = labels[,paste("perm",itt,sep = "")]
    tValues = ComputeTValues_MLET(resp_time,paired = paired)
    tValueTemp =  data.frame(permIdx = itt ,NullDist = sum(tValues$value))#EstimateNullDist_MLET(tValues, clusterTimepoint, tStatMax = threshold_c)
    # sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
    # sdat = sdat[sdat$index!=0,]
    if(nrow(tValueTemp)==0){
      stop("Error!") # ------------> Debug
      #   sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value)))
      #   tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
      # }else{
      #   tValueTemp = data.frame(Positive =  0, Negative =0)
    }
    tValueDist = rbind(tValueDist,tValueTemp)
    setTxtProgressBar(pb,itt/samples)
  }
  # tValueDist$Positive = ifelse(tValueDist$Positive>=0,tValueDist$Positive,0)
  # tValueDist$Negative = ifelse(tValueDist$Negative<=0,tValueDist$Negative,0)
  # tValueDist$NullDist = ifelse(abs(tValueDist$Positive)>abs(tValueDist$Negative),tValueDist$Positive,tValueDist$Negative)
  close(pb)
  return(tValueDist)
}


  