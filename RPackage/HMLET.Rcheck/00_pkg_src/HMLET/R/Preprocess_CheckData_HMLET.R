#' Check Data for Critical Issues
#'
#' @description
#' Raw eye-tracking data comes with time stamps of each sample and Gaze Coordinates. These time stamps
#' has to have consistent and unique time interval across all datapoints.
#' Gaze Coordinates may have missing values due to blinks, head movement, etc.
#' This function reports issues about inconsistent sampling intervals and missing Gaze coordinates.
#'
#' @param data long format dataframe containing temporal data.
#' @param ID string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that identify unique trials within data frame. Can be numerical or categorical.
#' @param timePoint string for column name that represents time stamps of each sample.
#' @param GazeX string for column name that includes X coordinate of gazepoints
#' @param GazeY string for column name that includes Y coordinate of gazepoints
#' @param missRateCap Specify the percentage of missing gazepoints cap to be counted as high Miss Rate
#' @export
Preprocess_CheckData_HMLET <- function(data, ID, trial, timePoint,
                                       GazeX, GazeY,
                                       missRateCap = 50){

  #------------------------- Verify time stamps are unique
  d = data %>%
    dplyr::group_by_at(c(ID, trial, timePoint)) %>%
    dplyr::summarise(N=n(),.groups = "drop") %>%
    as.data.frame()
  if(nrow(d[d$N>1,])>0){
    print("Sample of duplicated time stamps:")
    print(d[d$N>1,])
    print("---------------------------------")
    stop("Time stamps are not unique!
         Please check your data.
         ----------------------")
  }

  #------------------------- Estimate the sampling interval
  d = data
  temp = ComputeSamplingInterval_HMLET(data = d,ID = ID, trial = trial,
                                       timePoint = timePoint, userCallFlag = F)
  print("Estimated sampling interval(s):")
  print(sort(unique(temp$interval)))
  if(length(unique(temp$interval))>1){
    stop("\nSampling interval are inconsistent!
    You may need to use function\"Preprocess_FixSampling_HMLET\" to fix samplings!
    --------------------------------------------------------------------------------")
  }
  #------------------------- Verify sampling interval is consistent
  d = data
  d$timeStamp = d[[timePoint]]
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::reframe(intervals = unique(floor(diff(timeStamp*100,na.rm=T)+0.5)/100)) %>%
    as.data.frame()
  if(length(unique(d$intervals))>1){
    print("Inconsistent sampling intervals:")
    print(sort(unique(d$intervals)))
    print("-----------------------------")
    warning("\nTime stamps are inconsistent!
    Small variations in the sampling interval can be due to rounding error while
    bigger variations are due to missing timepoints.
    You may need to use function\"Preprocess_FixSampling_HMLET\" to fix samplings!
    --------------------------------------------------------------------------------")
  }

  #------------------------- Verify Missing Rate
  d = data
  d$timeStamp = d[[timePoint]]
  d$GazeX = d[[GazeX]]
  d$GazeY = d[[GazeY]]
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::summarise(N=n(),nNA = max(sum(is.na(GazeX)),sum(is.na(GazeY))),.groups = "drop")%>%
    as.data.frame()

  d$missRate = d$nNA/d$N*100
  d$highMissRate = ifelse(d$missRate>=missRateCap,1,0)
  d = d %>%
    dplyr::group_by_at(c(ID)) %>%
    dplyr::summarise(nTrials = n(),
                     nTarialsMoreThanCap=sum(highMissRate),
                     AverageMissed = mean(missRate),.groups = "drop") %>%
    as.data.frame()
  return(d)
}
