# Utility functions

#' ripped off from http://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Simple timing of function execution
#'
#' Time the execution of a function, returning the function's output and messaging the time difference
#' @param expr Unevaluated function call.
#' @export

timeit <- function(expr) {
  t1 <- Sys.time()
  out <- evalq(expr)
  t2 <- Sys.time()
  time <- difftime(t2, t1, units = "auto")
  message(sprintf("Finished in %.4g %s.", time, attr(time, "units")))

  if (!is.null(out)) {
    attr(out, "timeElapsed") <- time
  } else {
    out <- time
  }

  out
}

#' Convert timezones of a datetime character vector
#'
#' Conversion for a single timezone
#'
#' toUTC_oneTZ allows conversion for observations in a single timezone to UTC,
#' while toUTC allows for multiple starting timezones to be specified.
#' @param timestring A character vector convertible to POSIXct format
#' @param tzstring A character vector specifying time zones of timestring.
#' For toUTC_oneTZ, this must be of length one. For toUTC, this must have length
#' equal to that of timestring.
#' @param ... Arguments passed to as.POSIXct. (e.g. \code{format})
#' @seealso \code{\link{timezones}} for how to specify timezones, \code{\link{as.POSIXct}}
#' @export

toUTC_oneTZ <- function(timestring, tzstring, ...) {
  stopifnot(length(tzstring) == 1)
  psx1 <- as.POSIXct(timestring, tz = tzstring, ...)
  psx2 <- as.POSIXct(format(psx1, tz = "UTC", usetz = TRUE), tz = "UTC")
  psx2
}

#' @export
#' @describeIn toUTC_oneTZ Conversion allowing for multiple timezones

toUTC <- function(timestring, tzstring, ...) {
  stopifnot(length(timestring) == length(tzstring))
  tzstring <- as.factor(tzstring)
  timelist <- split(timestring, f = tzstring)

  mapfun <- function(timestring, tzstring) {
    out <- toUTC_oneTZ(timestring = timestring, tzstring = tzstring, ...)
  }

  utcList <- Map(mapfun, timestring = timelist, tzstring = levels(tzstring))
  out <- unsplit(utcList, f = tzstring)

  out
}

#' #' Compute seasonal harmonic functions
#' #'
#' #' Returns orthogonal harmonic functions from degree 1 to `degree` over a
#' #' specified set of dates, such that all have a common period of 1 year.
#' #' Based on the `stats::poly` function.
#' #'
#' #' returns a matrix with (2 * degree) columns, representing the powers-of-2 multiples of
#' #' sin and cosine functions with 1-year period.
#' #'
#' #' @param t a Date or POSIXt vector at which to evaluate the harmonic series.
#' #' @param degree the degree of the highest-degree harmonic function
#' #' @details Useful for adding seasonal harmonic terms in regression functions.
#' #' Uses 1 January as origin. Incorporates leap days exactly as `lubridate::leap_year()`
#' #' @return An object of class "sharm", which is a matrix with a `degree` attribute
#' #' corresponding to the maximum degree of the harmonic series.
#' #' @export
#'
#' sharm <- function(t, degree = 1) {
#'   if (!is(t, "Date") && !is(t, "POSIXt"))
#'     stop("t must be a Date or time object")
#'   if (degree < 1)
#'     stop("'degree' must be at least 1")
#'   if (anyNA(t))
#'     stop("missing values are not allowed in 'sharm'")
#'   n <- degree + 1
#'
#'   year = as.numeric(format(t, "%Y"))
#'   isly = (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)) # Thanks Hadley!
#'   denom = ifelse(isly, 15811200, 15768000)
#'
#'   degs = 2L^((1L:degree) - 1)
#'
#'   sins = matrix(vapply(degs, function(x) sinpi(x * jsec(t) / denom),
#'                        numeric(length(t))),
#'                 ncol = degree)
#'   colnames(sins) = paste0("sin", degs)
#'   coss = matrix(vapply(degs, function(x) cospi(x * jsec(t) / denom),
#'                        numeric(length(t))),
#'                 ncol = degree)
#'   colnames(coss) = paste0("cos", degs)
#'
#'   Z <- cbind(sins, coss)
#'   attr(Z, "degree") <- degree
#'   class(Z) <- c("sharm", "matrix")
#'
#'   Z
#' }
#'
#' #' obtain "julian seconds" (number of seconds since start of the year)
#' #` @export
#' jsec = function(t) {
#'   vapply(t,
#'          function(x) eval(parse(text = format(x, "(%j - 1) * 3600 * 24 + %H * 3600 + %M * 60 + %S"))),
#'          numeric(1)
#'   )
#' }
#'
#'
#' #' discount factor described in Wang et al., 2011
#' #'
#' #' Applies an exponential smooth function with parameter p
#' #'
#' #' @param x timeseries to exponentially smooth
#' #' @param d parameter of smooth. Must be strictly between 0 and 1. See Wang et al., 2011
#' #' @export
#' #'
#'
#' discount <- function(x, d) {
#'   stopifnot(d > 0 && d < 1)
#'
#'   dnumer <- Reduce(function(a, b) d * (a + b / d), d * x, accumulate = TRUE)
#'   dd <- d^(1 : (length(x)))
#'   ddenom <- cumsum(dd)
#'
#'   out <- dnumer / ddenom
#'   out
#' }
#'
#' #' Calculate antecedent dry days
#' #'
#' #' @param x Daily time series of rainfall amount.
#' #' @param thresh threshold precipitation amount for determining what is "dry"
#'
#' adry <- function(x, thresh) {
#'   below <- x < thresh
#'
#'   foo <- rle(as.numeric(below))
#'   dd <- foo$values == 1
#'   dds <- lapply(foo$lengths[dd], function(x) seq(1:x))
#'   wets <- lapply(foo$lengths[!dd], function(x) rep(0, x))
#'
#' }
