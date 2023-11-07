#' Label trials whether participant was disproportionately or equally looking toward items
#'
#'
#' @param testName Name of this data -- can be used as condition name or test names to compare permutation test results between different tests/conditions later
#' @param data data frame with temporal data for permutation tests routine.
#' @param ID optional string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that represents trials within data frame.
#' @param AOIName string for column name which includes name of the AOI that the subject is looking at each timepoint.
#' @param comparingAOI names of the two AOIs that are their looking proportion should be compared.
#' @param targetAOI names of the two AOIs that are their looking proportion should be compared.
#' @param dispropotionCriterion optional string for column name to focus on specific AOI, defaults to NULL.

#' @return data frame as original data with an extracolumn with labels
#' @export
RelativeViewingProportionLabels_HMLET <- function(data, ID = "ID", trial, AOIName,
                                                  comparingAOI = c("Target","Lure"),
                                                  targetAOI = "Target", dispropotionCriterion =10){

  groupingColumns = c(groupingColumns, "timeBin", "timeBinIndex")
  data = data %>%
    dplyr::group_by_at(groupingColumns) %>%
    dplyr::summarise_at(c(paste("AOI", AOIs, sep="_")), aggregateFun, na.rm = TRUE) %>%
    as.data.frame()

  if(!is.numeric(dat[,AOIName])){
    warning("AOIName column must be in numeric form")
  }
  if(!is.numeric(dat$timepoint)){
    warning("timepoint column must be in numeric form")
  }

  return(dat)
}
