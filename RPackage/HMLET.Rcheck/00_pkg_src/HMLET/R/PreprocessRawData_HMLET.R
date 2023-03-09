#' Pre-Process data to extract AOIs. \cr
#' Creates a csv file that formats all data into a table. \cr
#' Should be Completed later
#' @param data dataframe containing temporal data.
#' @param ID optional string for column name that represents IDs within data frame, defaults to "ID."
#' @param trial string for column name that represents trials within data frame, for example "TrialNum."
#' @param timepoint string for column name that represents time intervals, for example "timeStamp."
#' @param GazeX # TODO: ASK
#' @param GazeY # TODO: ASK
#' @param fileName optional string for output csv file, defaults to "ETDataforMATLAB.csv."
#' @param path specifies working directory of project.
#' @export
PreprocessRawData_HMLET <- function(data, ID = "ID", trial = "trial", timepoint = "timepoint",
                                   GazeX = "GazeX_Relative", GazeY = "GazeY_Relative",
                                   AOINames = NULL, fileName = "ETDataforMATLAB.csv", path = getwd()){

  try(write.csv(data, paste(path, fileName, sep = .Platform$file.sep)))
  if(is.null(A)){
    print(paste("Data is saved in",getwd(),.Platform$file.sep,fileName, sep=""))
  }
}
