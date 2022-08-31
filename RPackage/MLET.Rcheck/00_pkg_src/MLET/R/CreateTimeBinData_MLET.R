#' Collapse time points in specified time bins
#'
#' @export

CreateTimeBinData_MLET<- function(data, groupingColumns = NULL, timeBinWidth =  250, timeMax = 3000, FixatedOn,
                             timepoint = "timepoint", AOIs = NULL , timeForward = T, aggregateFun = mean){

  data = data[data[, timepoint]<timeMax, ]
  data$timeBin = floor(data[, timepoint]/timeBinWidth)*timeBinWidth+timeBinWidth/2
  timeBinsOrder = unique(data$timeBin)
  timeBinsOrder = timeBinsOrder[order(timeBinsOrder)]
  if(timeForward){
    data$timeBin = factor(data$timeBin,levels = timeBinsOrder, labels = paste(timeBinsOrder-timeBinWidth/2,
                                                                              timeBinsOrder+timeBinWidth/2,sep = "-"))
  }else{
    timeBinsOrder = timeBinsOrder[length(timeBinsOrder):1]
    data$timeBin = factor(data$timeBin,levels = timeBinsOrder, labels = paste(timeBinsOrder-timeBinWidth/2,
                                                                              timeBinsOrder+timeBinWidth/2,sep = "-"))
  }

  data$timepoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))

  if(is.null(AOIs)){
    AOIs = unique(data[,c(FixatedOn)])
    AOIs = AOIs[!is.na(AOIs)]
  }

  for (AOIName in AOIs){
    data[,paste("AOI", AOIName, sep = "_")] = unique(0)
    data[, paste("AOI", AOIName, sep = "_")][data[, FixatedOn]==AOIName] = unique(1)
  }

  if(!is.null(groupingColumns)){
    groupingColumns = c(groupingColumns, "timeBin", "timepoint")
    data = data %>%
      group_by_at(groupingColumns) %>%
      summarise_at(c(paste("AOI", AOIs, sep="_")), aggregateFun, na.rm = TRUE)
  }

  return(data)
}
