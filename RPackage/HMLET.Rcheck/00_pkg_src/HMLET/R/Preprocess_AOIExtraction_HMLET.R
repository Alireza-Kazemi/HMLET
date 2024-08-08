#' AOI Extraction
#'
#' @description
#' AOIs can be computed in rectangular or elliptic forms.
#' This function uses gaze coordinates to categorize them into specified AOIs
#'
#' @details
#' The output is the same dataframe as input with one new column:
#' AOIs: Includes the name of the AOI at each time point.
#'
#' @param data long format dataframe containing temporal data.
#' @param ID string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that identify unique trials within data frame. Can be numerical or categorical.
#' @param timePoint string for column name that represents time stamps of each sample.
#' @param GazeX string for column name that includes X coordinate of gazepoints
#' @param GazeY string for column name that includes Y coordinate of gazepoints
#' @param AOI_Names List of Strings for AOIs names
#' @param ContentLabel Label for gaze points that are outside all of the AOIs, defaults to "Content"
#' @param AOIs_Center_X List of X coordinates for the center of all AOIs
#' @param AOIs_Center_Y List of Y coordinates for the center of all AOIs
#' @param AOIs_Radius_X List of width sizes of all AOIs
#' @param AOIs_Radius_Y List of Height sizes of all AOIs
#' @param EllipticAOI Binary value indicates to compute AOIs as circules with Radius equal to AOIs_Radius_X, defaults to FALSE
#' @export
Preprocess_AOIExtraction_HMLET <- function(data, ID = "ID", trial, timePoint,
                                           GazeX, GazeY,
                                           AOI_Names, ContentLabel = "Content",
                                           AOIs_Center_X, AOIs_Center_Y,
                                           AOIs_Radius_X, AOIs_Radius_Y, EllipticAOI = FALSE){
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
