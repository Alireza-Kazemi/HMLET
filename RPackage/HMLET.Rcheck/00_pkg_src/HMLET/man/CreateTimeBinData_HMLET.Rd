% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateTimeBinData_HMLET.R
\name{CreateTimeBinData_HMLET}
\alias{CreateTimeBinData_HMLET}
\title{Collapse time points in specified time bins}
\usage{
CreateTimeBinData_HMLET(
  data,
  groupingColumns = NULL,
  timeBinWidth = 250,
  timeMax = 3000,
  FixatedOn,
  timepoint = "timepoint",
  AOIs = NULL,
  timeForward = T,
  aggregateFun = mean
)
}
\description{
Collapse time points in specified time bins
}
