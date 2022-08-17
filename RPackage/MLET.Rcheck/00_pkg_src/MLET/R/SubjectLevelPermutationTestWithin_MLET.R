#' SubjectLevelPermutationTestWithin_MLET
#'
SubjectLevelPermutationTestWithin_MLET <- function(data, samples = 2000 , paired = T, threshold_t = NA){

  resp_time = as.data.frame(summarise(group_by(data,ID,timepoint,condition),prop = mean(AOI)))
  labels = unique(resp_time[,c("ID","timepoint","condition")])
  labels = ComputeSubjectLevelPerm_MLET(labels, n = samples)

  #----------------------------- Perform Permutation tests
  print("Estimate tStatistic distribution:")
  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)

  tValueDist = NULL
  for (itt in 1:(samples)){
    resp_time$condition = labels[,paste("perm",itt,sep = "")]
    tValues = ComputeTValues_MLET(resp_time,paired = paired)
    tValues = FindClusters_MLET(tValues, threshold_t = threshold_t)
    sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
    sdat = sdat[sdat$index!=0,]
    if(nrow(sdat)!=0){
      sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value)))
      tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
    }else{
      tValueTemp = data.frame(Positive =  0, Negative =0)
    }
    tValueDist = rbind(tValueDist,tValueTemp)
    setTxtProgressBar(pb,itt/samples)
  }
  tValueDist$Positive = ifelse(tValueDist$Positive>=0,tValueDist$Positive,0)
  tValueDist$Negative = ifelse(tValueDist$Negative<=0,tValueDist$Negative,0)
  tValueDist$NullDist = ifelse(abs(tValueDist$Positive)>abs(tValueDist$Negative),tValueDist$Positive,tValueDist$Negative)
  close(pb)
  return(tValueDist)
}
