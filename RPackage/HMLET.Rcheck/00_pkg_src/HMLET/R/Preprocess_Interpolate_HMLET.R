#' Interpolate missing Gaze Points
#'
#' @description
#' Gaze Coordinates may have missing values due to blinks, head movement, etc.
#' This function fill the NA Gaze points with linear interpolation. You can specify
#' how many consecutive NAs should be interpolated by passing maxNAToFill parameter.
#'
#' @details
#' The output is the same dataframe as input with three new columns:
#' "GazeX_Interpolated": includes all original GazeX coordinates and interpolated values
#' "GazeY_Interpolated": includes all original GazeY coordinates and interpolated values
#' "Interpolated": is a flag that is 1 for samples that are interpolated and 0 otherwise.
#' Note: If your dataframe has columns with same names make sure to rename them before using this function.
#'
#' @param data long format dataframe containing temporal data.
#' @param ID string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that identify unique trials within data frame. Can be numerical or categorical.
#' @param timePoint string for column name that represents time stamps of each sample.
#' @param GazeX string for column name that includes X coordinate of gazepoints
#' @param GazeY string for column name that includes Y coordinate of gazepoints
#' @param maxNAToFill numeric value to denote the max number of consecutive NAs that should be interpolated, defaults to Inf
#' @export
Preprocess_Interpolate_HMLET <- function(data, ID = "ID", trial, timePoint,
                                         GazeX, GazeY,maxNAToFill = Inf){

  d = data

  d$Interpolated = unique(0)
  d$Interpolated[is.na(d$GazeX)|is.na(d$GazeY)] = unique(1)

  d$timeStamp_HMLETDummy = d[[timePoint]]
  d$GazeY_Interpolated = d[[GazeY]]
  d$GazeX_Interpolated = d[[GazeX]]
  d$GazeY_HMLETDummy = d[[GazeY]]
  d$GazeX_HMLETDummy = d[[GazeX]]
  d = d %>% dplyr::group_by_at(c(ID, trial)) %>%
            mutate(GazeY_Interpolated = zoo::na.approx(GazeY_HMLETDummy,x = timeStamp_HMLETDummy,
                                                       na.rm =F, maxgap = maxNAToFill),
                   GazeX_Interpolated = zoo::na.approx(GazeX_HMLETDummy,x = timeStamp_HMLETDummy,
                                                       na.rm =F, maxgap = maxNAToFill)) %>%
            as.data.frame()

  d = d[,!(names(d) %in% c("GazeX_HMLETDummy","GazeY_HMLETDummy","timeStamp_HMLETDummy"))]
  return(d)
}
