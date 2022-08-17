#' Generate unique random permutations
#'
#'@export
ComputeTValues_MLET <- function(respTime, paired = TRUE){
  respTime = respTime[order(respTime$ID,respTime$timepoint,respTime$condition),]
  if(paired){
    # respTime = reshape2::dcast(respTime,ID+timepoint~condition, value.var = "prop")
    # respTime$d = respTime$C1 - respTime$C2
    respTime = as.data.frame(summarise(group_by(respTime,ID,timepoint), d = -diff(prop)))
    tValues = as.data.frame(summarise(group_by(respTime,timepoint), value = sum(d)/sqrt((n()*sum(d^2)-sum(d)^2)/(n()-1)) ))
  }else{
    temp = as.data.frame(summarise(group_by(respTime,timepoint,condition), N = n(), M = mean(prop), SD = sd(prop)))
    tValues = as.data.frame(summarise(group_by(temp,timepoint), value = (-diff(M)/sqrt(sum(SD^2/N))) ))
  }
  return(tValues)
}
