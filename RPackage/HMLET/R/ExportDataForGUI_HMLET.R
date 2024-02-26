#' ExportDataForGUI_HMLET
#' Export samples of trial with maximum time point to CSV file for MATLAB GUI.
#'
#' @param data dataframe containing temporal data.
#' @param ID optional string for column name that represents IDs within data frame, defaults to "ID."
#' @param trial string for column name that represents trials within data frame, for example "TrialNum."
#' @param timePoint string for column name that represents time intervals, for example "timeStamp."
#' @param timeMax optional integer for maximum time in temporal order, samples are left out if duration is longer. Default is NULL
#' @param dataPointDuration optional integer or character for specified duration of samples, used for time bins in CreateTimeBinData_HMLET.
#' @param response the column in which participants' responses are stored e.g., HIT, Miss, FA, and CR.
#' @param fixation data for specific AOI in dataframe to be fixated on. #TODO: ASK
#' @param condition string for column name that specifies a condition within the data frame.
#' @param testName optional string for name of data -- used as condition name or test names to compare permutation test
#'                 results between different tests/conditions later, defaults to NULL.
#' @param gazeX integer X-coordinate of gaze point on the screen according to top left corner.
#' @param gazeY integer Y-coordinate of gaze point on the screen according to top left corner.
#' @param gazeXRelative optional integer X-coordinate of gaze point relative to an arbitrary center.
#' @param gazeYRelative optional integer Y-coordinate of gaze point relative to an arbitrary center.
#' @param miscVars optional list of strings containing column names of additional variables to be included in exported file
#' @param fileName optional string for output csv file, defaults to "HMLET_DataforGUI.csv."
#' @param path optional path to identify the destination location default: current working directory
#'
#' @export
ExportDataForGUI_HMLET <- function(data, ID = "ID", trial = "trial", timePoint = "timePoint",
                                     timeMax = NULL, dataPointDuration, response,
                                     fixation, condition, testName = NULL,
                                     gazeX, gazeY, gazeXRelative = NULL, gazeYRelative = NULL,
                                     miscVars = NULL,
                                     fileName = "HMLET_DataforGUI.csv", path = getwd()){

  if(is.null(timeMax)){
    timeMax = max(data[,timePoint])
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
  data = data[data[, timePoint]<=timeMax, ]

  data = data[,c(testName, ID, trial, timePoint, condition, response, "duration",
                 gazeX, gazeY, gazeXRelative, gazeYRelative, fixation, miscVars)]
  names(data) = c("testName", "ID", "trial", "timePoint", "condition", "response", "duration",
                 "gazeX", "gazeY", "gazeXRelative", "gazeYRelative", "fixation", miscVars)


  write.csv(data, paste(path, fileName, sep = .Platform$file.sep), row.names = F)
  print(paste("Data is saved in ",path,.Platform$file.sep,fileName, sep=""))
}
