% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ComputeSubjectLevelPerm_HMLET.R
\name{ComputeSubjectLevelPerm_HMLET}
\alias{ComputeSubjectLevelPerm_HMLET}
\title{Provides unique permutation indices within each subject.}
\usage{
ComputeSubjectLevelPerm_HMLET(labels, n = 1)
}
\arguments{
\item{labels}{labels from the data frame.}

\item{n}{optional integer for number of permutations to resample data.}
}
\value{
labels of unique permutation indices to use for subject level permutation test.
}
\description{
Provides unique permutation indices within each subject.
}
