#' Collapse time points in specified time bins.
#'
#' @param data dataframe containing temporal data
#' @param groupingColumns optional array of strings for grouping variables, defaults to NULL
#' @param timeBinWidth optional integer specified time interval for time bins, defaults to 250
#' @param timeMax optional integer, defaults to 3000
#' @param FixatedOn string for column name of specific AOI in dataframe to be fixated on
#' @param timepoint optional string representing the name of the column for timepoints, defaults to "timepoint"
#' @param AOIs optional <DATAYPE?> for areas of interest, defaults to NULL
#' @param timeForward optional boolean to sort timebins, defaults to True for ascending order.
#' @param aggregateFun optional function for aggregation, defaults to mean
#'
#' @return temporal data with sorted time points into time bins
#'
#' @export

# explanation for factor function
# what is an AOI?

CreateTimeBinData_HMLET<- function(data, groupingColumns = NULL, timeBinWidth =  250, timeMax = 3000, FixatedOn,
                             timepoint = "timepoint", AOIs = NULL , timeForward = T, aggregateFun = mean){

  # This is just a check to make sure time bin size is not out of order
  minTimeStep = round(abs(diff(data[, timepoint])),2)
  minTimeStep = minTimeStep[minTimeStep>0]
  minTimeStep = min(minTimeStep, na.rm = T)
  if(timeBinWidth<minTimeStep){
    warning(paste("\n    >> Time bin width (",timeBinWidth,") is smaller than timepoint steps",
                  "\n    >> Time bin width is set to smallest non-zero timepoint step  =",
                  minTimeStep, sep = ""))
    timeBinWidth = minTimeStep

  }


  # QUESTION: what is happening here and what does the factor function do?
  # this part is computing the time Bins their name and order.
  data = data[data[, timepoint]<timeMax, ]
  data$timeBin = ceiling(data[, timepoint]/timeBinWidth-1)*timeBinWidth+timeBinWidth/2
  timeBinsOrder = unique(data$timeBin)
  timeBinsOrder = timeBinsOrder[order(timeBinsOrder)]
  if(timeForward){
    data$timeBin = factor(data$timeBin,levels = timeBinsOrder, labels = paste(round(timeBinsOrder-timeBinWidth/2,2),
                                                                              round(timeBinsOrder+timeBinWidth/2,2),sep = "->"))
  }else{
    timeBinsOrder = timeBinsOrder[length(timeBinsOrder):1]
    data$timeBin = factor(data$timeBin,levels = timeBinsOrder, labels = paste(round(timeBinsOrder-timeBinWidth/2,2),
                                                                              round(timeBinsOrder+timeBinWidth/2,2),sep = "->"))
  }

  data$timeBinIndex = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))

  # AOIs
  if(is.null(AOIs)){
    AOIs = unique(data[,c(FixatedOn)])
    AOIs = AOIs[!is.na(AOIs)]
  }

  for (AOIName in AOIs){
    data[,paste("AOI", AOIName, sep = "_")] = unique(0)
    data[, paste("AOI", AOIName, sep = "_")][data[, FixatedOn]==AOIName] = unique(1)
  }

  # I moved this comment to here because the aggregation is taking place in the following lines
  # applies the aggregating function on binary AOIs within time bins per each level of the grouping variables defined by the user.
  if(!is.null(groupingColumns)){
    groupingColumns = c(groupingColumns, "timeBin", "timeBinIndex")
    data = data %>%
      group_by_at(groupingColumns) %>%
      summarise_at(c(paste("AOI", AOIs, sep="_")), aggregateFun, na.rm = TRUE) %>%
      as.data.frame()
  }

  return(data)
}
