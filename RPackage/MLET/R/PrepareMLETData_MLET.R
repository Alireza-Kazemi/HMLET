#' Prepare data columns
#'
#'
#' @param testName Name of this data -- can be used as condition name or test names to compare permutation test results between different tests/conditions later
#' @export
PrepareMLETData_MLET <- function(data, ID, trial, timepoint, condition, AOI,
                                  conditionLevels = NA, targetAOI = NA, testName = NA){
  if(anyNA(testName)){
    data$testName = unique("PermutationTest0")
    testName = "testName"
  }

  dat = data[, c(ID, trial, timepoint, condition, AOI, testName)]
  names(dat) = c("ID", "trial", "timepoint", "condition", "AOI", "testName")
  dat = dat[,c("testName", "ID", "trial", "timepoint", "condition", "AOI")]
  if(!anyNA(conditionLevels)){
    dat$condition = factor(dat$condition,levels = conditionLevels)
  }
  if(!anyNA(targetAOI)){
    dat$AOI = ifelse(dat$AOI==targetAOI, 1, 0)
  }
  if(!is.numeric(dat$AOI)){
    warning("AOI column must be in numeric form")
  }

  return(dat)
}
