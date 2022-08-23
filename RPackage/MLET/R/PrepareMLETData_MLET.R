#' Prepare dataa columns
#'
#'
#'
#' @export
PrepareMLETdataa_MLET <- function(data, ID, trial, timepoint, condition, AOI,
                                  conditionLevels = NA, targetAOI = NA){
  dat = data[, c(ID, trial, timepoint, condition, AOI)]
  names(dat) = c("ID", "trial", "timepoint", "condition", "AOI")
  if(!is.na(conditionLevels)){
    dat$condition = factor(dat$condition,levels = conditionLevels)
  }
  if(!is.na(targetAOI)){
    dat$AOI = ifelse(dat$AOI %in% targetAOI, 1, 0)
  }
  if(!is.numeric(dat$AOI)){
    warning("AOI column must be in numeric form")
  }

  return(dataa)
}
