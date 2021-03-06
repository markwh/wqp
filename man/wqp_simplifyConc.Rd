% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wqData.R
\name{wqp_simplifyConc}
\alias{wqp_simplifyConc}
\title{Simplify wqp concentration data}
\usage{
wqp_simplifyConc(wqpData, average = c("none", "depth", "time"),
  allowDuplicates = FALSE, redundantUnits = c("purge", "keep"))
}
\arguments{
\item{wqpData}{processed WQP data result}

\item{average}{How to average the observations if at all. "depth" will
average observations over depth; "time" will aggregate to daily resolution.}

\item{allowDuplicates}{If \code{FALSE}, remove duplicates using \code{unique}
following simplification.}
}
\description{
Simplify wqp concentration data
}
\details{
Note the following behavior when averaging. The is.bdl column
is also averaged, and therefore is no longer a logical vector, but a
decimal on [0, 1] indicating what proportion of averaged observations were bdl.
Some columns are dropped when averaging, namely the ones being averaged over,
while one is added, giving the number of observations being averaged.
}

