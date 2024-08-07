#' Conduct trial level permutation tests between trials.
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param samples optional number of resampling data, how many permutations needed.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @param threshold_t optional probability threshold for statistical comparison computed based on number of trials.
#'                    alpha = 0.025.
#' @return returns t value distribution presented in a data frame.
#' @export
TrialLevelPermutationTestWithin_HMLET <- function(data, samples = 2000, paired = T, threshold_t = NULL){
  # Always has to be paired if t-test is being used.
  print("Compute unique permutation labels for trials within each subject:")
  labels = unique(data[,c("ID","trial","condition")])
  condLevels = levels(factor(labels$condition))
  labelsPerm = NULL
  for (sID in unique(labels$ID)){
    labelTemp = labels[labels$ID==sID,]
    L = UniquePermutations_HMLET(labelTemp[,"condition"], uniqueLabels = condLevels, n = samples)
    L = data.frame(L)
    names(L) = 1:ncol(L)
    labelTemp = cbind(labelTemp,L)
    names(labelTemp) = c(names(labelTemp)[1:3],paste("perm",names(labelTemp)[-(1:3)],sep = ""))
    labelsPerm = rbind(labelsPerm,labelTemp)
  }
  labels = merge(labels,labelsPerm, by = c("ID","trial","condition"))

  #----------------------------- Perform Permutation tests
  print("Estimate tStatistic distribution:")
  pb = utils::txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)

  tValueDist = NULL
  for (itt in 1:(samples)){
    datItt = merge(data,labels[,c(1,2,3,3+itt)],by = c("ID","trial","condition"),all.x=T)
    if(nrow(data)!=nrow(datItt)){
      print(paste("Error in itt =", itt))
    }
    datItt$condition = datItt[, paste("perm",itt,sep = "")]
    resp_time = as.data.frame(dplyr::summarise(dplyr::group_by(datItt,ID,timePoint,condition),prop = mean(AOI, na.rm=T)))
    tValues = ComputeTValues_HMLET(resp_time, paired = paired)
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
