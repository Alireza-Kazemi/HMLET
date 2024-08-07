% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Preprocess_FixSampling_HMLET.R
\name{Preprocess_FixSampling_HMLET}
\alias{Preprocess_FixSampling_HMLET}
\title{Fix Sampling Inconsistencies}
\usage{
Preprocess_FixSampling_HMLET(
  data,
  ID = "ID",
  trial,
  timePoint,
  GazeX,
  GazeY,
  samplingInterval = NULL,
  fillGenratedRows = T,
  ignoreColumns = NULL,
  removeShortTrials = T,
  shortTrialsThreshold = 2
)
}
\arguments{
\item{data}{long format dataframe containing temporal data.}

\item{ID}{string for column name that represents IDs within data frame, defaults to "ID".}

\item{trial}{string for column name that identify unique trials within data frame. Can be numerical or categorical.}

\item{timePoint}{string for column name that represents time stamps of each sample.}

\item{GazeX}{string for column name that includes X coordinate of gazepoints}

\item{GazeY}{string for column name that includes Y coordinate of gazepoints}

\item{samplingInterval}{Specify the expected sampling interval}

\item{fillGenratedRows}{Optional flag to specify whether the new rows generated to fill temporal jumps should be filled or not. This function uses a 'down-up' order (each value filled with its preceding value; otherwise, with its following value) to fill these columns. Defaults to TRUE.}

\item{ignoreColumns}{Optional string(s) to specify columns that shouldn't be filled, defaults to NULL.}

\item{removeShortTrials}{Optional flag to specify whether short trials has to be removed or not, defaults to T.}

\item{shortTrialsThreshold}{Specify the minimum number of samples that should be available within a trial to be counted as a valid trial. defaults to 2}
}
\description{
Raw eye-tracking data comes with time stamps of each sample. These time stamps
has to have consistent and unique time interval across all datapoints.
If across participants there are different sampling intervals this function downsample
all datapoint to the biggest sampling interval. This function also relabel all
time stamps with a consistent time stamps to be comparable across trials and participants.
If there is a gap between samples of a trial this function fill in the gap with
NA values for Gaze coordinates.
difference between each two consecutive samples within a trial.
Note that your dataset has to have a unique consistent time interval between all samples.
In case, several time intervals is estimated for your dataset it use "Preprocess_FixSampling_HMLET"
function and pass the highest sampling interval to fix the inconsistency.
}
\details{
The output is the same dataframe as input with three new columns:
"time": includes new time stamps restarting to (0+sampling interval) in each trial
"sampleIdx": includes index for each sample restarting to 1 in each trial
"interval": includes the unique sampling interval which is equal to the "time"
Note: If your dataframe has columns with same names make sure to rename them before using this function.
}
