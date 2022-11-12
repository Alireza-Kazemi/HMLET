#' Return a data frame compatible for permutation tests routine
#'
#'
#' @param data data frame with temporal data for permutation tests routine.
#' @param ID optional string for column name that represents IDs within data frame, defaults to "ID"
#' @param trial string for column name that represents trials within data frame, for example "TrialNum"
#' @param timepoint string for column name that represents time intervals, for example "timeStamp"
#' @param condition string for column name that specifies a condition within the data frame
#' @param gazeInAOI string for column name for specific AOI in dataframe. # TODO: CONFIRM
#' @param conditionLevels optional array of string column names to denote all the values of conditions, defaults to NULL
#' @param targetAOI optional string for column name to focus on specific AOI, defaults to NULL
#' @param testName optional string for name of data -- used as condition name or test names to compare permutation test results between different tests/conditions later, defaults to NULL
#' @import dplyr
#' @import miceadds
#' @import purrr
#' @import rray
#' @import tidyr
#' @import combinat
#'
#' @return data frame compatible for permutation tests and other HMLET modules
#' @export
PermutationTestDataPrep_HMLET <- function(data, ID = "ID", trial, timepoint, condition, gazeInAOI,
                                  conditionLevels = NULL, targetAOI = NULL, testName = NULL){
  if(is.null(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }

  dat = data[, c(testName, ID, trial, timepoint, condition, gazeInAOI)]
  names(dat) = c("testName", "ID", "trial", "timepoint", "condition", "AOI")
  if(!is.null(conditionLevels)){
    dat$condition = factor(dat$condition,levels = conditionLevels)
  }
  if(!is.null(targetAOI)){
    dat$AOI = ifelse(dat$AOI==targetAOI, 1, 0)
  }
  if(!is.numeric(dat$AOI)){
    warning("AOI column must be in numeric form")
  }

  return(dat)
}
