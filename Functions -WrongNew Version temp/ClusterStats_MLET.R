ClusterStats_MLET <- function(data, paired = T, detailed = F, threshold_t = NA){
  if(is.na(threshold_t)){
    num_sub = length(unique(data$ID))  
    threshold_t = qt(p=1-.05/2, df=num_sub-1)
  }
  resp_time = as.data.frame(summarise(group_by(data, ID, timepoint, condition), prop = mean(AOI)))
  tValues = ComputeTValues_MLET(resp_time, paired = paired)
  tValues = FindClusters_MLET(tValues, threshold_t = threshold_t) 
  sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
  sdat = sdat[sdat$index!=0,]
  if (detailed){
    sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
	return(list(sdat, tValues))
  }else{
    sdat = as.data.frame(summarise(group_by(sdat, Direction, index), tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
	return(sdat)
  }
}