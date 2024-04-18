#' Compute t values for permutation tests.
#'
#' @param respTime dataframe for responses in temporal order.
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @return t values in the format of a data frame.
#'@export
ComputeTValues_HMLET <- function(respTime, paired = TRUE){
  respTime = respTime[order(respTime$ID,respTime$timePoint,respTime$condition),]
  if(paired){
    respTime = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,ID,timePoint), d = -diff(prop)))
    # --> changed to keep the t-value computation consistent with the threshold value for unbalanced data.
    # The assumption is that each time point is tested independently
    tValues = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,timePoint), value = sum(d)/sqrt((dplyr::n()*sum(d^2)-sum(d)^2)/(dplyr::n()-1)) ))
    # nObservations = length(unique(respTime$ID))
    # tValues = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,timePoint), value = sum(d)/sqrt((nObservations*sum(d^2)-sum(d)^2)/(nObservations-1)) ))
    #--<|
  }else{
    temp = as.data.frame(dplyr::summarise(dplyr::group_by(respTime,timePoint,condition), N = dplyr::n(), M = mean(prop), SD = sd(prop)))
    # --> changed to keep the t-value computation consistent with the threshold value for unbalanced data.
    tValues = as.data.frame(dplyr::summarise(dplyr::group_by(temp,timePoint), value = (-diff(M)/sqrt(sum(SD^2/N))) ))
    # nObservations = length(unique(respTime$ID))
    # tValues = as.data.frame(dplyr::summarise(dplyr::group_by(temp,timePoint), value = (-diff(M)/sqrt(sum(SD^2/nObservations))) ))
    #--<|
  }
  tValues$value[is.infinite(tValues$value)] = 0
  return(tValues)
}
