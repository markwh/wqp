% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wqData.R
\name{makeRcData}
\alias{makeRcData}
\title{Make datasets for use in rcgam and similar functions}
\usage{
makeRcData(simpleConc, simpleFlow, bdl.threshold = 0, type = c("rcData",
  "raw"))
}
\arguments{
\item{simpleConc}{a data.frame returned by wqp_simplifyConc}

\item{simpleFlow}{a data.frame returned by wqp_simplifyFlow}

\item{bdl.threshold}{Days with greater a larger fraction than bdl.threshold bdl values will be marked as bdl.}
}
\description{
Returns a list of rcData objects.
}

