#' Return a data frame compatible for permutation tests routine.
#'
#'
#' @param testName Name of this data -- can be used as condition name or test names to compare permutation test results between different tests/conditions later
#' @param data data frame with temporal data for permutation tests routine.
#' @param ID optional string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that represents trials within data frame.
#' @param timePoint string for column name that includes time point or index values in numeric form.
#' @param timeBinName optional string for the column name that includes time bin names, defaults to NULL.
#' @param condition string for column name that specifies a condition within the data frame.
#' @param gazeMeasure string for column name which includes numerical value of the gaze measure at each time point such as gazeproportion.
#' @param conditionLevels optional array of string column names to denote all the values of conditions, defaults to NULL.
#' @param targetAOI optional string specify the name of the target AOI which should be set as 1 in the gazeMeasure variable, defaults to NULL.
#' @param testName optional string for name of data -- used as condition name or test names to compare permutation test
#'                 results between different tests/conditions later, defaults to NULL.
#'
#' @import dplyr
#' @import miceadds
#' @import purrr
#' @import ggplot2
#' @import rray
#' @import tidyr
#' @import vdiffr
#' @import combinat
#' @import reshape2
#' @return data frame compatible for permutation tests and other HMLET modules
#' @export
PermutationTestDataPrep_HMLET <- function(data, ID = "ID", trial, timePoint, condition, gazeMeasure,
                                  conditionLevels = NULL, targetAOI = NULL, testName = NULL, timeBinName = NULL){
  if(is.null(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }

  if(is.null(timeBinName)){
    dat = data[, c(testName, ID, trial, timePoint, condition, gazeMeasure)]
    names(dat) = c("testName", "ID", "trial", "timePoint", "condition", "AOI")
  }
  else{
    dat = data[, c(testName, ID, trial, timePoint, condition, gazeMeasure, timeBinName)]
    names(dat) = c("testName", "ID", "trial", "timePoint", "condition", "AOI", "timeBinName")
  }
  if(!is.null(conditionLevels)){
    dat$condition = factor(dat$condition,levels = conditionLevels)
  }
  if(!is.null(targetAOI)){
    dat$AOI = ifelse(dat$AOI==targetAOI, 1, 0)
  }
  if(!is.numeric(dat$AOI)){
    warning("gazeMeasure column must be in numeric form")
  }
  if(!is.numeric(dat$timePoint)){
    warning("timePoint column must be in numeric form")
  }

  return(dat)
}
