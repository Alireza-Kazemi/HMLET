#' Export to CSV file for MATLAB GUI
#'
#'
#'
#' @export
ExportDataForGUI_HMLET <- function(data, ID = "ID", trial = "trial", timepoint = "timepoint",
                                   timeMax = NULL, dataPointDuration, response,
                                   fixation, condition, testName = NULL,
                                   gazeX, gazeY, gazeXRelative = NULL, gazeYRelative = NULL,
                                   miscVars = NULL,
                                   fileName = "HMLET_DataforMATLAB.csv", path = getwd()){

  if(is.null(timeMax)){
    timeMax = max(data[,timepoint])
  }
  if(is.null(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }
  if(is.null(gazeXRelative) | is.null(gazeYRelative)){
    data$gazeXRelative = unique(NA)
    data$gazeYRelative = unique(NA)
  }
  if(is.character(dataPointDuration)){
    data$duration = data[,dataPointDuration]
  }else if(is.numeric(dataPointDuration)){
    data$duration = unique(dataPointDuration)
  }
  data = data[data[, timepoint]<timeMax, ]

  data = data[,c(testName, ID, trial, timepoint, condition, response, "duration",
                 gazeX, gazeY, gazeXRelative, gazeYRelative, fixation, miscVars)]
  names(data) = c("testName", "ID", "trial", "timepoint", "condition", "response", "duration",
                 "gazeX", "gazeY", "gazeXRelative", "gazeYRelative", "fixation", miscVars)


  # data = CreateTimeBinData_HMLET(data, groupingColumns = NULL,
  #                                timeBinWidth =  unique(data$duration),
  #                                timeMax = timeMax, FixatedOn = "fixation",
  #                                timepoint = "timepoint", AOIs = NULL ,
  #                                timeForward = timeForward, aggregateFun = NULL)




  write.csv(data, paste(path, fileName, sep = .Platform$file.sep), row.names = F)
  print(paste("Data is saved in",getwd(),.Platform$file.sep,fileName, sep=""))

}
