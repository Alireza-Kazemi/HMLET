#' SubjectLevelPermutationTestWithin_HMLET
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param samples optional number of resampling data, how many permutations needed.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @param threshold_t optional probability threshold for statistical comparison computed based on number of subjects.
#'                    alpha = 0.025.
#' @return returns t value distribution presented in a data frame.
#' @export
SubjectLevelPermutationTestWithin_HMLET <- function(data, samples = 2000 , paired = T, threshold_t = NA){

  resp_time = as.data.frame(summarise(group_by(data,ID,timepoint,condition), prop = mean(AOI, na.rm=T)))
  labels = unique(resp_time[,c("ID","timepoint","condition")])
  labels = ComputeSubjectLevelPerm_HMLET(labels, n = samples)

  #----------------------------- Perform Permutation tests
  print("Estimate tStatistic distribution:")
  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)

  tValueDist = NULL
  for (itt in 1:(samples)){
    resp_time$condition = labels[,paste("perm",itt,sep = "")]
    tValues = ComputeTValues_HMLET(resp_time,paired = paired)
    tValues = FindClusters_HMLET(tValues, threshold_t = threshold_t)
    sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
    sdat = sdat[sdat$index!=0,]
    if(nrow(sdat)!=0){
      sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value, na.rm=T)))
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
