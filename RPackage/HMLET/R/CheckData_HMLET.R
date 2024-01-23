#' Compute the sampling time duration \cr
#'
#' @description
#' This function computs the sampling duration for each trial. Grouping variable
#' has to be defined to be unique for all datapoints in one trial.
#' @param data long format dataframe containing temporal data.
#' @param groupingColumns string for the column(s) name(s) for grouping variable(s) to uniquely identify datapoints within each trial
#' @param timePoint string for the column name that represents time stamps for each data point, for example "timeStamp."
#' @export
Preprocess_CheckData_HMLET <- function(data, groupingColumns, timePoint){

  dSave = data

  data = data %>%
    dplyr::group_by_at(groupingColumns) %>%
    dplyr::mutate(N=n()), aggregateFun, na.rm = TRUE) %>%
  as.data.frame()


}
