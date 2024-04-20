#' Add cluster information and stats to the corresponding time points in the initial dataframe.
#'
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param clusterInf data frane with cluster information, output from ClusterStats_HMLET or PermutationTest_HMLET
#'
#' @return data frame compatible for permutation tests and other HMLET modules
#' @export
AddClusterInfotoData_HMLET <- function(data, clusterInf){
  if(is.factor(data$timePoint)){
    data$timeBin = data$timePoint
    data$timePoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
    warning(paste("\n    >> timePoints are converted to numeric indexes: ",
                  "\n    >> ",c(paste(levels(data$timeBin),1:length(levels(data$timeBin)), sep = " -> ", collapse = "   ")), sep = ""))
  }


  data$clusterDirection = NA
  data$clusterIndex = NA
  data$clusterTime = NA
  if("significant" %in% names(clusterInf)){
    data$clusterSig = NA
    data$clusterpValue = NA
  }
  for (index in seq(1,nrow(clusterInf)) ){
    timeStart = clusterInf[index,"timeStart"]
    timeEnd = clusterInf[index,"timeEnd"]

    idx = (data$timePoint >= timeStart) & (data$timePoint <= timeEnd)
    idx = idx & (data$testName == clusterInf[index,"testName"])

    data$clusterDirection[idx] = as.character(clusterInf[index,"Direction"])
    data$clusterIndex[idx] = clusterInf[index,"index"]
    data$clusterTime[idx] = paste(timeStart,"->",timeEnd, sep = "")
    if("significant" %in% names(clusterInf)){
      data$clusterSig[idx] = as.character(clusterInf[index,"significant"])
      data$clusterpValue[idx] = clusterInf[index,"pValue"]
    }

  }
  return(data)
}
