#' Remove Time points with missing conditions
#'
#' This function is currently only applicable for within participant manipulations
#'
#' @export
RemoveIncompleteTimePoints_MLET <- function(data){
  rmIdx = as.data.frame(summarise(group_by(data,ID,timepoint,condition), prop = mean(AOI, na.rm=T)))
  rmIdx = reshape2::dcast(rmIdx, ID+timepoint~condition, value.var = "prop")
  rmIdx = rmIdx[!complete.cases(rmIdx),]
  if(nrow(rmIdx)>0){
    warning("Time points with missing conditions removed!")
    rmIdx = paste(rmIdx$ID,rmIdx$timepoint,sep = "_")
    dIdx = paste(data$ID,data$timepoint,sep = "_")
    data = data[!(dIdx %in% rmIdx),]
  }
  return(data)
}
