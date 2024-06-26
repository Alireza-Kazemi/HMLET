% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotNullDistribution_HMLET.R
\name{PlotNullDistribution_HMLET}
\alias{PlotNullDistribution_HMLET}
\title{Plot Null Distribution.}
\usage{
PlotNullDistribution_HMLET(resultList, smoothingBandWidth = NULL)
}
\arguments{
\item{resultList}{list of the dataframes that is already prepared by PrepareMLETData_HMLET
or a list that is the result of PermutationTest_HMLET.}

\item{smoothingBandWidth}{The smoothing bandwidth which determines the granularity of distribution estimation, defaults to sd/3}
}
\value{
null distribution plot handle.
}
\description{
Plot Null Distribution.
}
