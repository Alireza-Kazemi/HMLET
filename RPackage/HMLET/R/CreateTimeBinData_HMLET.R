#' Collapse time points in specified time bins
#'
#' @export

CreateTimeBinData_HMLET<- function(data, groupingColumns = NULL, timeBinWidth =  250, timeMax = 3000, FixatedOn,
                             timepoint = "timepoint", AOIs = NULL , timeForward = T, aggregateFun = mean){

  minTimeStep = round(abs(diff(data[, timepoint])),2)
  minTimeStep = minTimeStep[minTimeStep>0]
  minTimeStep = min(minTimeStep, na.rm = T)
  if(timeBinWidth<minTimeStep){
    warning(paste("\n    >> Time bin width (",timeBinWidth,") is smaller than timepoint steps",
                  "\n    >> Time bin width is set to smallest non-zero timepoint step  =",
                  minTimeStep, sep = ""))
    timeBinWidth = minTimeStep

  }
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

  if(is.null(AOIs)){
    AOIs = unique(data[,c(FixatedOn)])
    AOIs = AOIs[!is.na(AOIs)]
  }

  for (AOIName in AOIs){
    data[,paste("AOI", AOIName, sep = "_")] = unique(0)
    data[, paste("AOI", AOIName, sep = "_")][data[, FixatedOn]==AOIName] = unique(1)
  }

  if(!is.null(groupingColumns)){
    groupingColumns = c(groupingColumns, "timeBin", "timeBinIndex")
    data = data %>%
      group_by_at(groupingColumns) %>%
      summarise_at(c(paste("AOI", AOIs, sep="_")), aggregateFun, na.rm = TRUE)
  }

  return(data)
}
