#' Return a data frame compatible for permutation tests routine.
#'
#'
#' @param data data frame with temporal data for permutation tests routine.
#' @param ID optional string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that represents trials within data frame.
#' @param timepoint string for column name that represents time intervals.
#' @param condition string for column name that specifies a condition within the data frame.
#' @param gazeInAOI string for column name which includes name of the AOI that the subject is looking at each timepoint.
#' @param conditionLevels optional array of string column names to denote all the values of conditions, defaults to NULL.
#' @param targetAOI optional string for column name to focus on specific AOI, defaults to NULL.
#' @param testName optional string for name of data -- used as condition name or test names to compare permutation test
#'                 results between different tests/conditions later, defaults to NULL.
#' @import combinat
#' @import dplyr
#' @import ggplot2
#' @import miceadds
#' @import purrr
#' @import reshape2
#' @import rray
#' @import tidyr
#' @import vdiffr
#'
#' @return data frame compatible for permutation tests and other HMLET modules
#' @export
PermutationTestDataPrep_HMLET <- function(data, ID = "ID", trial, timepoint, condition, gazeInAOI,
                                  conditionLevels = NULL, targetAOI = NULL, testName = NULL, timeBinName = NULL){
  if(is.null(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }

  if(is.null(timeBinName)){
    dat = data[, c(testName, ID, trial, timepoint, condition, gazeInAOI)]
    names(dat) = c("testName", "ID", "trial", "timepoint", "condition", "AOI")
  }
  else{
    dat = data[, c(testName, ID, trial, timepoint, condition, gazeInAOI, timeBinName)]
    names(dat) = c("testName", "ID", "trial", "timepoint", "condition", "AOI", "timeBinName")
  }
  if(!is.null(conditionLevels)){
    dat$condition = factor(dat$condition,levels = conditionLevels)
  }
  if(!is.null(targetAOI)){
    dat$AOI = ifelse(dat$AOI==targetAOI, 1, 0)
  }
  if(!is.numeric(dat$AOI)){
    warning("AOI column must be in numeric form")
  }
  if(!is.numeric(dat$timepoint)){
    warning("timepoint column must be in numeric form")
  }

  return(dat)
}
