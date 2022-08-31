#' Export to CSV file for MATLAB GUI
#'
#'
#'
#' @export
ExportDataForMATLAB_MLET <- function(data, ID = "ID", trial = "trial", timepoint = "timepoint",
                                     timeMax, samplingRate,
                                     gazeX, gazeY, gazeXRelative = NULL, gazeYRelative = NULL,
                                     AOINames, condition, testName = NULL,
                                     fileName = "ETDataforMATLAB.csv", path = getwd()){

  if(is.null(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }
  if(is.null(gazeXRelative) | is.null(gazeYRelative)){
    data$gazeXRelative = unique(NA)
    data$gazeYRelative = unique(NA)
  }

  data = data[,c(testName, ID, trial, timepoint, condition, gazeX, gazeY, gazeXRelative, gazeYRelative, AOINames,)]
  names(dat) = c("testName", "ID", "trial", "timepoint", "condition", "AOI")
  try(write.csv(data, paste(path, fileName, sep = .Platform$file.sep)))
  if(is.null(A)){
    print(paste("Data is saved in",getwd(),.Platform$file.sep,fileName, sep=""))
  }
}
