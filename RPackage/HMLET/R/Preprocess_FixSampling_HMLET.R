#' Fix Sampling Inconsistencies
#'
#' @description
#' Raw eye-tracking data comes with time stamps of each sample. These time stamps
#' has to have consistent and unique time interval across all datapoints.
#' If across participants there are different sampling intervals this function downsample
#' all datapoint to the biggest sampling interval. This function also relabel all
#' time stamps with a consistent time stamps to be comparable across trials and participants.
#' If there is a gap between samples of a trial this function fill in the gap with
#' NA values for Gaze coordinates.
#' difference between each two consecutive samples within a trial.
#' Note that your dataset has to have a unique consistent time interval between all samples.
#' In case, several time intervals is estimated for your dataset it use "Preprocess_FixSampling_HMLET"
#' function and pass the highest sampling interval to fix the inconsistency.
#'
#' @details
#' The output is the same dataframe as input with three new columns:
#' "time": includes new time stamps restarting to (0+sampling interval) in each trial
#' "sampleIdx": includes index for each sample restarting to 1 in each trial
#' "interval": includes the unique sampling interval which is equal to the "time"
#' Note: If your dataframe has columns with same names make sure to rename them before using this function.
#'
#' @param data long format dataframe containing temporal data.
#' @param ID string for column name that represents IDs within data frame, defaults to "ID".
#' @param trial string for column name that identify unique trials within data frame. Can be numerical or categorical.
#' @param timePoint string for column name that represents time stamps of each sample.
#' @param GazeX string for column name that includes X coordinate of gazepoints
#' @param GazeY string for column name that includes Y coordinate of gazepoints
#' @param samplingInterval Specify the expected sampling interval
#' @param fillGenratedRows Optional flag to specify whether the new rows generated to fill temporal jumps should be filled or not. This function uses a 'down-up' order (each value filled with its preceding value; otherwise, with its following value) to fill these columns. Defaults to TRUE.
#' @param ignoreColumns Optional string(s) to specify columns that shouldn't be filled, defaults to NULL.
#' @param removeShortTrials Optional flag to specify whether short trials has to be removed or not, defaults to T.
#' @param shortTrialsThreshold Specify the minimum number of samples that should be available within a trial to be counted as a valid trial. defaults to 2
#' @export
Preprocess_FixSampling_HMLET <- function(data, ID = "ID", trial, timePoint,
                                         GazeX, GazeY,
                                         samplingInterval = NULL,
                                         fillGenratedRows = T,
                                         ignoreColumns = NULL,
                                         removeShortTrials = T,
                                         shortTrialsThreshold = 2){

  d = data

  # Trials with only 1 sample has to be removed
  d$GazeX_HMLETdummy = d[[GazeX]]
  d$GazeY_HMLETdummy = d[[GazeY]]
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::mutate(nSamples = dplyr::n())%>%
    as.data.frame()
  dtemp = d[d$nSamples<2,c(ID, trial)]
  d = d[d$nSamples>=2,]
  if(nrow(dtemp)>0){
    dtemp = unique(dtemp)
    dtemp = dtemp %>% dplyr::group_by_at(ID) %>%
            dplyr::summarise(nInvalidTrials = dplyr::n(),.groups = "drop")%>%
            as.data.frame()
    print("------------------------------------------------------------------------------------------------")
    print(paste("Invalid trials with only 1 sample are removed:",sep = ""))
    print("Summary of the number of invalid trials per participant:")
    print(dtemp)
    print("--------------------------------------------------------")
  }
  d = d[,!(names(d)%in%c("GazeX_HMLETdummy","GazeY_HMLETdummy","nSamples"))]

  if (is.null(samplingInterval)){
    #------------------------- Estimate the sampling interval
    temp = ComputeSamplingInterval_HMLET(data = d,ID = ID, trial = trial,
                                         timePoint = timePoint, userCallFlag = F,
                                         appendFlag = F)
    if (length(temp)==1){
      samplingInterval = as.numeric(temp)
    }else{
      print("Different sampling intervals estimated across participants:")
      print(sort(unique(temp)))
      stop("Only one sampling interval has to be used, preferebly the biggest one.
           Please rerun this function while manually set the \"samplingInterval\" parameter to the desired value.")
    }
    print("Sampling interval is set to be:")
    print(samplingInterval)
  }
  #------------------------- Sort data and create indexes
  d = ComputeSamplingInterval_HMLET(data = d,ID = ID, trial = trial,
                                    timePoint = timePoint, userCallFlag = F,
                                    appendFlag = T)
  d = d[order(d[[ID]],d[[trial]],d[[timePoint]]),]
  d$time = d[[timePoint]]
  d$overalIdx = seq(1,nrow(d))
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::mutate(time = time-min(time),timeDiff = c(0,diff(time)),sampleIdx = seq(from = 1,to = dplyr::n(), by = 1)) %>%
    as.data.frame()
  d$time = ceiling(d$time/d$interval)*d$interval

  #------------------------- Create new complete time indexes
  d2 = d %>%
    dplyr::group_by_at(c(ID, trial, "interval")) %>%
    dplyr::reframe(timeNew = seq(from = min(time),to = max(time), by = unique(interval))) %>%
    dplyr::group_by_at(c(ID, trial,"interval")) %>%
    dplyr::mutate(sampleIdxNew = seq(from = 1,to = dplyr::n(), by = 1)) %>%
    as.data.frame()

  #------------------------- Merge the complete data and remove extra columns
  d$timeMerge = round((ceiling(d$time/d$interval)*d$interval)+0.005,2)
  d2$timeMerge = round(ceiling(d2$timeNew/d2$interval)*d2$interval+0.005,2)
  d2 = d2[order(d2[,ID],d2[,trial],d2$sampleIdxNew),]
  d = merge(d2,d,by = c(ID, trial, "timeMerge","interval"), all.x = T)
  d$time = d$timeMerge
  d$sampleIdx = d$sampleIdxNew
  d = d[,!(names(d) %in% c("overalIdx","timeMerge","timeNew","sampleIdxNew","timeDiff"))]
  d$time = floor((d$time+d$interval)*10000+0.5)/10000 # fix starting points.
  d = d[order(d[[ID]],d[[trial]],d$time),]
  #------------------------ Resample Data to the samplingInterval


  d$sampleIdxNew = ceiling(d$time/max(unique(d$interval)))  # round function has issues in R round(0.5)=0 but should be 1
  d = d %>%
    dplyr::group_by_at(c(ID, trial,"sampleIdxNew")) %>%
    dplyr::mutate(remove = max(sampleIdx)) %>%
    as.data.frame()
  d$remove = ifelse(d$remove==d$sampleIdx,0,1)
  d = d[d$remove==0,]
  d$interval = round(samplingInterval+0.000005,4)

  # ---------Debugging
  # d = d %>%
  #   dplyr::group_by_at(c(ID, trial)) %>%
  #   dplyr::mutate(test = c(1,diff(sampleIdxNew))) %>%
  #   as.data.frame()
  # d[d$test!=1,]

  d = d[,!(names(d)%in%c("sampleIdxNew","remove"))]

  # ReCreate all time points
  d = d %>%
    dplyr::group_by_at(c(ID, trial)) %>%
    dplyr::mutate(sampleIdx = seq(from = 1,to = dplyr::n(), by = 1)) %>%
    as.data.frame()
  d$time = floor((d$sampleIdx*d$interval)*10000+0.5)/10000
  d = d[order(d[[ID]],d[[trial]],d$time),]

  if (fillGenratedRows){
    columnNames = names(d)
    columnNames = setdiff(columnNames,c(ID, trial,timePoint,GazeX, GazeY,"time","interval","sampleIdx",ignoreColumns))
    d = d %>% dplyr::group_by_at(c(ID, trial)) %>%
      tidyr::fill(tidyr::all_of(columnNames), .direction="downup")
  }

  if(removeShortTrials){
    d$GazeX_HMLETdummy = d[[GazeX]]
    d$GazeY_HMLETdummy = d[[GazeY]]
    d = d %>%
      dplyr::group_by_at(c(ID, trial)) %>%
      dplyr::mutate(nSamples = sum(!(is.na(GazeX_HMLETdummy)|is.na(GazeY_HMLETdummy))))%>%
      as.data.frame()
    dtemp = d[d$nSamples<shortTrialsThreshold,c(ID, trial)]
    d = d[d$nSamples>=shortTrialsThreshold,]
    if(nrow(dtemp)>0){
      dtemp = unique(dtemp)
      dtemp = dtemp %>% dplyr::group_by_at(ID) %>%
        dplyr::summarise(nInvalidTrials = dplyr::n(),.groups = "drop")%>%
        as.data.frame()
      print("------------------------------------------------------------------------------------------------")
      print(paste("Invalid trials with less than ",shortTrialsThreshold," non-NA gaze points are removed:",sep = ""))
      print("Summary of the number of invalid trials per participant:")
      print(dtemp)
      print("--------------------------------------------------------")
    }
    d = d[,!(names(d)%in%c("GazeX_HMLETdummy","GazeY_HMLETdummy","nSamples"))]
  }

  return(d)
}
