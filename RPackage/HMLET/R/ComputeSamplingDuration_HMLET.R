#' Compute the sampling time duration
#'
#' @description
#' This function computs the sampling duration for each trial. Grouping variable
#' has to be defined to be unique for all datapoints in one trial.
#' @param data long format dataframe containing temporal data.
#' @param ID string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that represents trials within data frame.
#' @param timePoint string for column name that represents time intervals.
#' @param appendFlag optional flag specifies whether the interval(s) being appended to the data or returned separately, defaults to TRUE.
#' @param userCallFlag optional flag to print out data when users are calling this function, defaults to TRUE.
#' @export

ComputeSamplingInterval_HMLET <- function(data, ID, trial, timePoint, appendFlag = T, userCallFlag = T){

  d = data
  d$timeStamp = d[,timePoint]
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::summarise(interval = round(mean(diff(timeStamp)),2)) %>%
    dplyr::group_by_at(c(ID, "interval")) %>%
    dplyr::summarise(N=n(),.groups = "drop") %>%
    dplyr::group_by_at(ID) %>%
    dplyr::summarise(interval = interval[which.max(N)],.groups = "drop") %>%
    as.data.frame()

  intervals = sort(unique(d$interval))


  if(userCallFlag){
    print("Unique sampling intervals estimated across participants:")
    print(sort(unique(intervals)))
    if(length(unique(intervals))>1){
      warning("There should be only 1 sampling interval.
               You have inconsistent sampling intervals.
               You may need to use function\"Preprocess_FixSampling_HMLET\" to fix samplings!")
    }
  }

  if(appendFlag){
    data$tempOrder_HMLET = seq(from = 1, to = nrow(data), by = 1)
    data = merge(data,d,by = ID, all.x = T)
    data = data[order(data$tempOrder_HMLET),]
    data = data[,!(names(data) %in% c("tempOrder_HMLET"))]
    return(data)
  }else{
    return(intervals)
  }
}
