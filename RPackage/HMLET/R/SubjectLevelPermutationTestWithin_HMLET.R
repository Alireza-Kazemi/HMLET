#' Conduct subject level permutation tests between trials.
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param samples optional number of resampling data, how many permutations needed.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @param threshold_t optional probability threshold for statistical comparison computed based on number of subjects.
#'                    alpha = 0.025.
#' @return returns t value distribution presented in a data frame.
#' @export
SubjectLevelPermutationTestWithin_HMLET <- function(data, samples = 2000 , paired = T, threshold_t = NULL){

  resp_time = as.data.frame(dplyr::summarise(dplyr::group_by(data,ID,timePoint,condition), prop = mean(AOI, na.rm=T)))
  labels = unique(resp_time[,c("ID","timePoint","condition")])
  if(paired) {
    labels = ComputeSubjectLevelPermLabels_HMLET(labels, n = samples)
  }else{
    tmp = labels %>% dplyr::group_by(ID) %>%
      dplyr::summarise(N=dplyr::n()) %>% as.data.frame()
    if(max(tmp$N)>1){
      stop("Some participants has more than 1 condition.\nYou may need to use Paired test for valid results.")
    }
    subjLevelLabels = unique(labels[,c("ID","condition")])
    condLevels = levels(factor(labels$condition))
    subjLevelPerms = UniquePermutations_HMLET(subjLevelLabels$condition, uniqueLabels = condLevels, n = samples)
    subjLevelPerms = as.data.frame(subjLevelPerms)
    names(subjLevelPerms) = gsub("V","perm",names(subjLevelPerms))
    subjLevelPerms$ID = subjLevelLabels$ID
    labels = merge(labels,subjLevelPerms,by="ID")
  }


  #----------------------------- Perform Permutation tests
  print("Estimate tStatistic distribution:")
  pb = utils::txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)

  tValueDist = NULL
  for (itt in 1:(samples)){
    resp_time$condition = labels[,paste("perm",itt,sep = "")]
    tValues = ComputeTValues_HMLET(resp_time,paired = paired)
    tValues = FindClusters_HMLET(tValues, threshold_t = threshold_t)
    sdat = reshape2::melt(tValues,id.vars = c("timePoint","value"),variable.name = "Direction", value.name = "index")
    sdat = sdat[sdat$index!=0,]
    if(nrow(sdat)!=0){
      sdat = as.data.frame(dplyr::summarise(dplyr::group_by(sdat,Direction,index),tStatistic = sum(value, na.rm=T)))
      tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
    }else{
      tValueTemp = data.frame(Positive =  0, Negative =0)
    }
    tValueDist = rbind(tValueDist,tValueTemp)
    utils::setTxtProgressBar(pb,itt/samples)
  }
  tValueDist$Positive = ifelse(tValueDist$Positive>=0,tValueDist$Positive,0)
  tValueDist$Negative = ifelse(tValueDist$Negative<=0,tValueDist$Negative,0)
  tValueDist$NullDist = ifelse(abs(tValueDist$Positive)>abs(tValueDist$Negative),tValueDist$Positive,tValueDist$Negative)
  close(pb)
  return(tValueDist)
}
