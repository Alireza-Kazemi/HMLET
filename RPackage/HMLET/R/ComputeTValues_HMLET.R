#' ComputeTValues_HMLET
#'
#' @param respTime <TODO> ask
#' @param paired optional boolean to control T-tests. True is for statistical comparisons in paired mode,
#'               False for a two-sample T-Test; defaults to True.
#' @return a list of t values
#'@export
ComputeTValues_HMLET <- function(respTime, paired = TRUE){
  respTime = respTime[order(respTime$ID,respTime$timepoint,respTime$condition),]
  if(paired){
    respTime = as.data.frame(summarise(group_by(respTime,ID,timepoint), d = -diff(prop)))
    tValues = as.data.frame(summarise(group_by(respTime,timepoint), value = sum(d)/sqrt((n()*sum(d^2)-sum(d)^2)/(n()-1)) ))
  }else{
    temp = as.data.frame(summarise(group_by(respTime,timepoint,condition), N = n(), M = mean(prop), SD = sd(prop)))
    tValues = as.data.frame(summarise(group_by(temp,timepoint), value = (-diff(M)/sqrt(sum(SD^2/N))) ))
  }
  return(tValues)
}
