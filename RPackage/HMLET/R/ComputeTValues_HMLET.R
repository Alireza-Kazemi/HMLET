#' Compute t values for permutation tests.
#'
#' @param respTime dataframe for responses in temporal order.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @return t values in the format of a data frame.
#'@export
ComputeTValues_HMLET <- function(respTime, paired = TRUE){
  respTime = respTime[order(respTime$ID,respTime$timePoint,respTime$condition),]
  #--> change to keep the t-value computation consistent with the threshold value for unbalanced data.
  nSubj = length(unique(respTime$ID))
  #--<|
  if(paired){
    respTime = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,ID,timePoint), d = -diff(prop)))
    # --> change to keep the t-value computation consistent with the threshold value for unbalanced data.
    # tValues = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,timePoint), value = sum(d)/sqrt((n()*sum(d^2)-sum(d)^2)/(n()-1)) ))
    tValues = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,timePoint), value = sum(d)/sqrt((nSubj*sum(d^2)-sum(d)^2)/(nSubj-1)) ))
    #--<|
  }else{
    temp = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,timePoint,condition), N = n(), M = mean(prop), SD = sd(prop)))
    # --> change to keep the t-value computation consistent with the threshold value for unbalanced data.
    # tValues = as.data.frame(dplyr::summarise(dplyr::group_by(temp,timePoint), value = (-diff(M)/sqrt(sum(SD^2/N))) ))
    tValues = as.data.frame(dplyr::summarise(dplyr::group_by(temp,timePoint), value = (-diff(M)/sqrt(sum(SD^2/nSubj))) ))
    #--<|
  }
  return(tValues)
}
