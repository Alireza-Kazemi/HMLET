#' Return a data frame compatible for permutation tests routine
#'
#'
#' @param testName Name of this data -- can be used as condition name or test names to compare permutation test results between different tests/conditions later
#' @import dplyr
#' @import miceadds
#' @import purrr
#' @import rray
#' @import tidyr
#' @import combinat
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
