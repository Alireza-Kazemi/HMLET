#' Compute the sampling time interval
#'
#' @description
#' This function estimates the sampling interval for each trial based on the time
#' differences between each time point and uses mode to find the interval.
#' Note that your dataset has to have a unique consistent time interval between all samples.
#' In case, several time intervals is estimated for your dataset it use "Preprocess_FixSampling_HMLET"
#' function and pass the highest sampling interval to fix the inconsistency.
#'
#' @param data long format dataframe containing temporal data.
#' @param ID string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that represents trials within data frame. Can be numerical or categorical.
#' @param timePoint string for column name that represents time stamps of each sample.
#' @param appendFlag optional flag specifies whether the interval(s) being appended to the data or returned separately, defaults to TRUE.
#' @param userCallFlag optional flag to print out data when users are calling this function, defaults to TRUE.
#' @export

ComputeSamplingInterval_HMLET <- function(data, ID = "ID", trial, timePoint, appendFlag = T, userCallFlag = T){

  d = data
  d$timeStamp_HMLETDummy = d[[timePoint]]
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::summarise(interval = floor(mean(diff(timeStamp_HMLETDummy)*100)+0.5)/100,.groups = "drop") %>%
    dplyr::group_by_at(c(ID, "interval")) %>%
    dplyr::summarise(N=dplyr::n(),.groups = "drop") %>%
    dplyr::group_by_at(ID) %>%
    dplyr::summarise(interval = interval[which.max(N)],.groups = "drop") %>%
    as.data.frame()

  d = d[,!(names(d) %in% "timeStamp_HMLETDummy")]
  intervals = sort(unique(d$interval))


  if(userCallFlag){
    print("Unique sampling intervals estimated across participants:")
    print(sort(unique(intervals)))
    if(length(unique(intervals))>1){
      warning("There should be only 1 sampling interval.
               You have inconsistent sampling intervals.
               You may need to use function \"Preprocess_FixSampling_HMLET\" to fix samplings!")
    }
  }

  if(appendFlag){
    data$tempOrder_HMLET = seq(from = 1, to = nrow(data), by = 1)
    data = data[,!(names(data) %in% c("interval"))]
    data = merge(data,d,by = ID, all.x = T)
    data = data[order(data$tempOrder_HMLET),]
    data = data[,!(names(data) %in% c("tempOrder_HMLET"))]
    return(data)
  }else{
    return(intervals)
  }
}
