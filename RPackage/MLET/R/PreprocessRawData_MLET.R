#' Pre-Process data to extract AOIs
#'
#'
#'
#' @export
PreprocessRawData_MLET <- function(data, GazeX = "GazeX_Relative", GazeY = "GazeY_Relative",
                                     AOINames = NULL, fileName = "ETDataforMATLAB.csv", path = getwd()){

  try(write.csv(data, paste(path, fileName, sep = .Platform$file.sep)))
  if(is.null(A)){
    print(paste("Data is saved in",getwd(),.Platform$file.sep,fileName, sep=""))
  }
}
