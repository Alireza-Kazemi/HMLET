#' Remove Time points with missing conditions
#'
#' This function is currently only applicable for within participant manipulations
#' @param data dataframe containing temporal data.
#' @return data with no missing conditions.
#'
#' @export
RemoveIncompleteTimePoints_HMLET <- function(data){
  rmIdx = as.data.frame(summarise(group_by(data,ID,timepoint,condition), prop = mean(AOI, na.rm=T)))
  rmIdx = reshape2::dcast(rmIdx, ID+timepoint~condition, value.var = "prop")
  rmIdx = rmIdx[!complete.cases(rmIdx),]
  if(nrow(rmIdx)>0){
    rmIdx = paste(rmIdx$ID,rmIdx$timepoint,sep = "_")
    dIdx = paste(data$ID,data$timepoint,sep = "_")
    data = data[!(dIdx %in% rmIdx),]
    warning(paste("\n    >> In test data: ",unique(data$testName),
                  "\n    >> Time points with missing conditions removed! (N =",
                  length(dIdx[dIdx %in% rmIdx]),")", sep = ""))
  }
  return(data)
}
