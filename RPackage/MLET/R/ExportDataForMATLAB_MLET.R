#' Export to CSV file for MATLAB GUI
#'
#'
#'
#' @export
ExportDataForMATLAB_MLET <- function(data, ID = "ID", trial = "trial", timepoint = "timepoint",
                                     timeMax = 3000, samplingDuration, timeForward = T,
                                     fixation, condition, testName = NULL,
                                     gazeX, gazeY, gazeXRelative = NULL, gazeYRelative = NULL,
                                     miscVars = NULL,
                                     fileName = "ETDataforMATLAB.csv", path = getwd()){

  if(is.null(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }
  if(is.null(gazeXRelative) | is.null(gazeYRelative)){
    data$gazeXRelative = unique(NA)
    data$gazeYRelative = unique(NA)
  }
  if(is.character(samplingDuration)){
    data$duration = data[,samplingDuration]
  }else if(is.numeric(samplingDuration)){
    data$duration = unique(samplingDuration)
  }

  data = data[,c(testName, ID, trial, timepoint, condition, "duration",
                 gazeX, gazeY, gazeXRelative, gazeYRelative, fixation, miscVars)]
  names(data) = c("testName", "ID", "trial", "timepoint", "condition", "duration",
                 "gazeX", "gazeY", "gazeXRelative", "gazeYRelative", "fixation", miscVars)


  data = CreateTimeBinData_MLET(data, groupingColumns = NULL, timeBinWidth =  unique(data$duration),
                                timeMax = timeMax, FixatedOn = "fixation",
                                timepoint = "timepoint", AOIs = NULL ,
                                timeForward = timeForward, aggregateFun = mean)




  write.csv(data, paste(path, fileName, sep = .Platform$file.sep), row.names = F)
  print(paste("Data is saved in",getwd(),.Platform$file.sep,fileName, sep=""))

}
