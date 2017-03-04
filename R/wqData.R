# wqData.R
# Functions defining wqData objects and operations thereupon


# Getting data ------------------------------------------------------------

#' Retrieve data.frame of all characteristic names available from WQP
#' @export
getWQPCodes <- function(name = c("statecode", "countycode", "Sitetype",
                                 "Organization", "sampleMedia",
                                 "Characteristictype", "Characteristicname",
                                 "providers")) {
  name <- match.arg(name)
  charListURL <- paste0("http://www.waterqualitydata.us/Codes/",
                        name,
                        "?mimeType=json")
  out <- jsonlite::fromJSON(charListURL)[["codes"]]
  out
}

#' Lookup characteristic names available from WQP
#' @param pattern text string to match
#' @param ... other arguments passed to grepl
charNameLookup <- function(pattern, ignore.case = TRUE, ...) {
  names <- getWQPCodes("Characteristicname")
  out <- names[grepl(pattern = pattern, x = names$value,
                     ignore.case = ignore.case, ...), ]
  out
}

#' Get and show bounding box
#'
#' @param west Minimum longitude in decimal degrees
#' @param south Minimum latitude in decimal degrees
#' @param east Maximum longitude in decimal degrees
#' @param north Maximum latitude in decimal degrees
#' @param plot Generate a plot? Requires leaflet package.
bbox <- function(west, south, east, north, plot = TRUE) {
  out <- paste(c(west, south, east, north), collapse = ",")
  if (plot) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      stop("Package 'leaflet' is required to use plot. Please install it.")
    }
    l1 <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addRectangles(lng1 = west, lat1 = north,
                    lng2 = east, lat2 = south)
    print(l1)
  }
  out
}

#' Improvements on dataRetrieval functions.
#' Gets data for rating-curve models. That is, constituent concentration and flows on river monitoring stations.
#' returns an object of class rcData
#' @param bBox bounding box, can use bbox()
#' @param charname characteristic name. See charNameLookup
getConcData <- function(bBox = NULL, charname = NULL, chartype = NULL,
                        statecode = NULL, ...) {
  if (!is.null(charname))
    charname = paste(charname, collapse = ";")
  ret1 <- dataRetrieval::readWQPdata(bBox = bBox, siteType = "Stream",
              characteristicName = URLencode(charname),
              sampleMedia = "Water", ...)
  ret2 <- wqp_checkClasses(ret1)
  ret2
}

#' Simple flow retrieval from WQP sites
#'
#' @param siteids Character vector of site identifiers
#' @param n Maximum number of sites per retrieval. Full retrieval will be done in batches of n.
#' @importFrom dplyr bind_rows
#' @export
getFlowData <- function(siteids, n = 100) {
  flowchars <- c("Flow", "Flow rate, instantaneous",
                 "Flow runoff", "Flow, runoff", "Storm water flow",
                 "Stream flow, instantaneous", "Stream flow, mean. daily",
                 "Discharge, River/Stream")
  # flowcodes <- c("00060", "00061", "00059", "50051")
  flowchars <- paste(flowchars, collapse = ";")

  siteids <- vapply(siteids, URLencode, character(1))

  f <- rep(1:(ceiling(length(siteids) / n)), each = n)[1:length(siteids)]
  siteIDsets <- split(siteids, f = f)
  urls <- lapply(siteIDsets, dataRetrieval::constructWQPURL,
                 parameterCd = flowchars, "", "", FALSE)
  # urls2 <- lapply(siteIDsets, dataRetrieval::constructWQPURL,
  #                 parameterCd = flowcodes, "", "", FALSE)
  # urls <- c(urls1, urls2)
  # return(urls)
  retlist <- list()
  for (i in 1:length(urls)) {
    message(sprintf("WQP retrieval %s of %s.", i, length(urls)))
    retlist[[i]] <- dataRetrieval::importWQP(obs_url = urls[[i]])
  }

  out <- retlist %>%
  lapply(wqp_checkClasses) %>%
    bind_rows()

  out
}



#' Takes an object returned by readWQPData (or getConcData) and returns an
#' object of class rcData
wqpToRcData <- function(concdata, flowdata) {


  concdf <- concdata %>%
    transmute(Date = ActivityStartDate,
              char = CharacteristicName,
              frac = ResultSampleFractionText,
              station = MonitoringLocationIdentifier,
              conc = ifelse(is.bdl,
                            DetectionQuantitationLimitMeasure.MeasureValue,
                            ResultMeasureValue),
              conc.units = ResultMeasure.MeasureUnitCode,
              is.bdl)
  flowdf <- flowdata %>%
    transmute(Date = ActivityStartDate,
              # flowchar = CharacteristicName,
              station = MonitoringLocationIdentifier,
              flow = ResultMeasureValue,
              flow.units = ResultMeasure.MeasureUnitCode) %>%
    group_by_(~station) %>%
    dplyr::mutate(lqbar = mean(log(flow), na.rm = TRUE),
           lqsd = sd(log(flow), na.rm = TRUE))

  combodf <- inner_join(concdf, flowdf, by = c("Date", "station"))

  f <- with(combodf, make.names(paste(station, char, frac, conc.units)))
  rawlist <- split(combodf, f)
  means <- vapply(rawlist, function(x) x$lqbar[1], numeric(1))
  sds <- vapply(rawlist, function(x) x$lqsd[1], numeric(1))
  out <- Map(makeModelData, rawData = rawlist, qbar = means, qsd = sds) %>%
    lapply(arrange_, "Date")

}



# Units -------------------------------------------------------------------

#' @export
m3s_cfs <- function(x) x * 35.3147

#' @export
cfs_m3s <- function(x) x / 35.3147

#' @export
convertToCFS <- function(flow, units) {
  units <- tolower(units)
  mults <- c("ft3/sec" = 1,
             "gal/min" = 0.002229,
             "cfs" = 1,
             "ft3/s" = 1,
             "g/sec" = 0.13368)
  cfs <- flow * mults[units]
  cfs
}

#' Convert to milligrams per liter
#' @export
convertToMg_L <- function(conc, units) {
  mults <- c("ug/l" = 0.001,
             "ppb" = 0.001,
             "mg/l" = 1)
}

#' Wrapper for ud.convert that accepts vector inputs and multiple conversion candidates.
#' @param x Numeric vector of values
#' @param from Units of x values
#' @param to Units to convert to.
#' @param inconvertibles Should units that cannot be converted be preserved
#' or omitted from the result?
#' @return A data.frame with columns x and units
#' @details If length(to) > 0, units the first is tried first, followed by the second, etc.
#'  If units in from are already contained in to, these units are not converted.
#' @importFrom udunits2 ud.convert ud.are.convertible
#' @importFrom assertthat assert_that
#' @export

convertUnits <- function(x, from, to, inconvertibles = c("preserve", "omit")) {

  inconvertibles <- match.arg(inconvertibles)

  if (length(from) == 1)
    from <- rep(from, length(x))

  assert_that(length(x) == length(from),
              is(from, "character"),
              is(to, "character"))

  from <- validateUnits(from)
  to <- validateUnits(to)

  # Perform a SINGLE conversion with nice error handling
  conv <- function(to_) {
    ret1 <- function(x_, from_) {
      ret2 <- try(ud.convert(x_, from_, to_), silent = TRUE)
      ret2 <- ifelse(is(ret2, "try-error"), NA_real_, ret2)
    }
    ret1
  }

  # vectorized conversion
  conv2 <- function(x_, from_, to_) {
    unlist(Map(conv(to_), x_ = x_, from_ = from_))
  }

  inds <- is.na(match(from, to)) # indices of original units vector that don't match conversion candidates
  out <- data.frame(x = x, units = from, stringsAsFactors = FALSE)

  # map unique(from) to unit from to
  ufrom <- unique(from)
  whichCanConvert <- function(tounit)
    ufrom[vapply(ufrom, ud.are.convertible, logical(1), u2 = tounit)]

  convList <- lapply(to, whichCanConvert) # Which are able to be converted to what

  for (i in 1:length(convList)) {
    if (length(convList[[i]]) == 0)
      next
    inds <- from %in% convList[[i]]
    out[inds, ]$x = conv2(x[inds], from[inds], to[i])
    out[inds, ]$units <- to[i]
  }

  if (inconvertibles == "omit")
    out <- out[out$units %in% to, ]

  out
}

#' replacement function for commonly given, but "improper" units,
#' e.g. "CFS", which *should* be ft3/s
#' @export
validateUnits <- function(unit) {
  # unit <- tolower(unit)
  replacement <- c("cfs" = "ft3/s", "CFS" = "ft3/s",
                   "gpm" = "gallon/min",
                   "gal/min" = "gallon/min",
                   "mgd" = "Mgallon/day", "MGD" = "Mgallon/day")

  matches <- match(unit, names(replacement))
  matchna <- is.na(matches)
  out <- unit
  out[!matchna] <- replacement[matches[!matchna]]

  convertible <- vapply(out, udunits2::ud.is.parseable, logical(1))
  if (sum(!convertible) > 0)
    warning(paste("Unrecognized units:",
               paste(unique(out[!convertible]), collapse = ", ")))

  out
}


# WQP data checks ---------------------------------------------------------



#' Look up missing units, possibly converting them to a supplied list of candidates.
#'
#' Replaces missing units by:
#' 1. Looking up using USGSPCode
#' 2. If value is zero, assigns units as most commonly used units in dataset
#' @param convertTo Optional character vector specifying the units to be converted to.
#' If more than one is provided, they are tried in the order they are given.
#' The udunits2 package is used for conversion.
#' @param wqpData data.frame returned by readWQPData
#' @param convertTo vector of candidates for unit conversion.
#' @param inconvertibles Should units that cannot be converted be preserved
#' or omitted from the result?
#' @param depthUnits Units to which to convert height/depth measurements, if desired.
#' @importFrom dplyr group_by_ ungroup
#' @export
wqp_checkUnits <- function(wqpData, convertTo = NULL,
                           inconvertibles = c("preserve", "omit"),
                           depthUnits = NULL) {

  inconvertibles <- match.arg(inconvertibles)
  out <- wqpData

  badrows <- is.na(out$ResultMeasure.MeasureUnitCode) |
    out$ResultMeasure.MeasureUnitCode == ""

  # 1. lookup units using USGS pcode
  ptbl <- dataRetrieval::pCodeToName
  badpcodes <- out$USGSPCode[badrows]
  badunits <- ptbl$measureunitcode[match(badpcodes, ptbl$parm_cd)]

  colind <- which(names(out) == "ResultMeasure.MeasureUnitCode")
  out[which(badrows), colind] <- badunits

  # indices of bad missing units that were not found using usgspcode.
  stillbad <- is.na(out$ResultMeasure.MeasureUnitCode) |
    out$ResultMeasure.MeasureUnitCode == ""

  # 2. for zeros, assign to most commonly used units for same constituent, fraction, station
  isZero <- out$ResultMeasureValue == 0 &
    !is.na(out$ResultMeasureValue)

  # function to find most common unit
  lookupUnitMode <- function(wd) {
    wd_smry <- wd %>%
      group_by_(~CharacteristicName,
               ~ResultSampleFractionText,
               ~MonitoringLocationIdentifier) %>%
      dplyr::summarize(n = n())
    wqp_modes <- out %>%
      dplyr::filter(CharacteristicName %in% wd_smry$CharacteristicName,
                    ResultSampleFractionText %in% wd_smry$ResultSampleFractionText,
                    MonitoringLocationIdentifier %in% wd_smry$MonitoringLocationIdentifier,
                    !is.na(ResultMeasure.MeasureUnitCode),
                    ResultMeasure.MeasureUnitCode != "") %>%
      group_by_(~CharacteristicName,
               ~ResultSampleFractionText,
               ~MonitoringLocationIdentifier) %>%
      dplyr::summarize(mode = Mode(ResultMeasure.MeasureUnitCode)) %>%
      ungroup()
    wd1 <- dplyr::left_join(wd, wqp_modes, by = c("MonitoringLocationIdentifier",
                                           "CharacteristicName",
                                           "ResultSampleFractionText")) %>%
      dplyr::mutate(ResultMeasure.MeasureUnitCode = mode) %>%
      dplyr::select(-mode)
    wd1$ResultMeasure.MeasureUnitCode
  }

  # replace using mode if some units are still missing.
  if (sum(stillbad & isZero) > 0)
    out[stillbad & isZero, ]$ResultMeasure.MeasureUnitCode <-
    lookupUnitMode(out[stillbad & isZero, ])

  # 3. Perform unit conversion if necessary
  if (!is.null(convertTo)) {
    convertedVals <- convertUnits(x = out$ResultMeasureValue,
                                  from = out$ResultMeasure.MeasureUnitCode,
                                  to = convertTo)
    convertedDLs <- convertUnits(
      x = out$DetectionQuantitationLimitMeasure.MeasureValue,
      from = out$DetectionQuantitationLimitMeasure.MeasureUnitCode,
      to = convertTo)

    out$ResultMeasureValue <- convertedVals$x
    out$ResultMeasure.MeasureUnitCode <- convertedVals$units


    out$DetectionQuantitationLimitMeasure.MeasureValue <-
      convertedDLs$x
    out$DetectionQuantitationLimitMeasure.MeasureUnitCode <-
      convertedDLs$units
  }


  if (inconvertibles == "omit")
    out <- out[out$ResultMeasure.MeasureUnitCode %in% convertTo, ]

  out <- wqp_setAttrs(out, attributes(wqpData), check = "units")
  out
}


#' Filters wqpData to include only allowed fractions
#' @param silent If FALSE (default) report how many rows and which fractions were omitted.
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkFraction <- function(wqpData, silent = FALSE) {
  allowedFractions <- c("Total", "Dissolved", "Recoverable", "Suspended", "Total Recoverable")
  keeprows <- wqpData$ResultSampleFractionText %in% allowedFractions
  out <- wqpData[keeprows, ]

  if (!silent && sum(keeprows) < nrow(wqpData)) {
    message(sprintf("Omitting %s rows with the following reported fractions: %s",
                    sum(!keeprows),
                    paste(unique(wqpData$ResultSampleFractionText[!keeprows]),
                          collapse = ", ")))
  }

  out <- wqp_setAttrs(out, attributes(wqpData), check = "fraction")

  out
}

#' Make sure all columns are present and classes are correct.
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkClasses <- function(wqpData) {
  reference <- c("OrganizationIdentifier" = "character",
                 "OrganizationFormalName" = "character",
                 "ActivityIdentifier" = "character",
                 "ActivityTypeCode" = "character",
                 "ActivityMediaName" = "character",
                 "ActivityMediaSubdivisionName" = "character",
                 "ActivityStartDate" = "Date",
                 "ActivityStartTime.Time" = "character",
                 "ActivityStartTime.TimeZoneCode" = "character",
                 "ActivityEndDate" = "Date",
                 "ActivityEndTime.Time" = "character",
                 "ActivityEndTime.TimeZoneCode" = "character",
                 "ActivityDepthHeightMeasure.MeasureValue" = "numeric",
                 "ActivityDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ActivityDepthAltitudeReferencePointText" = "character",
                 "ActivityTopDepthHeightMeasure.MeasureValue" = "numeric",
                 "ActivityTopDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ActivityBottomDepthHeightMeasure.MeasureValue" = "numeric",
                 "ActivityBottomDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ProjectIdentifier" = "character",
                 "ActivityConductingOrganizationText" = "character",
                 "MonitoringLocationIdentifier" = "character",
                 "ActivityCommentText" = "character",
                 "SampleAquifer" = "character",
                 "HydrologicCondition" = "character",
                 "HydrologicEvent" = "character",
                 "SampleCollectionMethod.MethodIdentifier" = "character",
                 "SampleCollectionMethod.MethodIdentifierContext" = "character",
                 "SampleCollectionMethod.MethodName" = "character",
                 "SampleCollectionEquipmentName" = "character",
                 "ResultDetectionConditionText" = "character",
                 "CharacteristicName" = "character",
                 "ResultSampleFractionText" = "character",
                 "ResultMeasureValue" = "numeric",
                 "ResultMeasure.MeasureUnitCode" = "character",
                 "MeasureQualifierCode" = "character",
                 "ResultStatusIdentifier" = "character",
                 "StatisticalBaseCode" = "character",
                 "ResultValueTypeName" = "character",
                 "ResultWeightBasisText" = "character",
                 "ResultTimeBasisText" = "character",
                 "ResultTemperatureBasisText" = "character",
                 "ResultParticleSizeBasisText" = "character",
                 "PrecisionValue" = "character",
                 "ResultCommentText" = "character",
                 "USGSPCode" = "character",
                 "ResultDepthHeightMeasure.MeasureValue" = "numeric",
                 "ResultDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ResultDepthAltitudeReferencePointText" = "character",
                 "SubjectTaxonomicName" = "character",
                 "SampleTissueAnatomyName" = "character",
                 "ResultAnalyticalMethod.MethodIdentifier" = "character",
                 "ResultAnalyticalMethod.MethodIdentifierContext" = "character",
                 "ResultAnalyticalMethod.MethodName" = "character",
                 "MethodDescriptionText" = "character",
                 "LaboratoryName" = "character",
                 "AnalysisStartDate" = "Date",
                 "ResultLaboratoryCommentText" = "character",
                 "DetectionQuantitationLimitTypeName" = "character",
                 "DetectionQuantitationLimitMeasure.MeasureValue" = "numeric",
                 "DetectionQuantitationLimitMeasure.MeasureUnitCode" = "character",
                 "PreparationStartDate" = "Date",
                 "ProviderName" = "character",
                 "ActivityStartDateTime" = "POSIXct",
                 "ActivityEndDateTime" = "POSIXct")

  out <- wqpData

  missingCols <- setdiff(names(reference), names(out))
  if (length(missingCols) > 0)
    out[missingCols] <- NA

  extraCols <- setdiff(names(out), names(reference))
  if (length(extraCols) > 0)
    warning(paste("The following columns are unrecognized and have been removed:",
                  paste(extraCols, collapse = ", ")))

  out <- out[names(reference)]

  wrongClass <- !mapply(FUN = is, out, reference[names(out)])
  if (sum(wrongClass) > 0) {
    fixClass <- function(x, class)
      get(paste0("as.", class))(x)
    out[wrongClass] <- Map(fixClass, x = out[wrongClass],
                               class = reference[wrongClass])
  }
  out <- wqp_setAttrs(out, attributes(wqpData), check = "classes")
  out
}

#' Check for detection limit behavior
#'
#' This function performs the following:
#' 1. Adds is.bdl column
#' 2. Converts detection-limit units to MeasureValue units.
#' 3. Augments DetectionQuantitationLimitMeasure.MeasureValue,
#' DetectionQuantitationLimitMeasure.MeasureUnitCode.
#' 4. If detection limit not reported, assumes it is at largest non-bdl value for that dataset
#' @param wqpData data.frame returned by readWQPData
#' @importFrom udunits2 ud.convert
#' @export
wqp_checkBDL <- function(wqpData) {

  out <- wqpData

  # Make detection limit and value have same units
  mismatches <- out$ResultMeasure.MeasureUnitCode !=
    out$DetectionQuantitationLimitMeasure.MeasureUnitCode &
    !is.na(out$ResultMeasure.MeasureUnitCode) &
    !is.na(out$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  if (sum(mismatches) > 0) {
    out$DetectionQuantitationLimitMeasure.MeasureUnitCode[mismatches] <-
      out$ResultMeasure.MeasureUnitCode[mismatches]
    out$DetectionQuantitationLimitMeasure.MeasureValue[mismatches] <-
      with(out[mismatches, ], mapply(ud.convert,
                         x = DetectionQuantitationLimitMeasure.MeasureValue,
                         u1 = DetectionQuantitationLimitMeasure.MeasureUnitCode,
                         u2 = ResultMeasure.MeasureUnitCode))
  }


  nonDetectStrings <- c("Present Below Quantification Limit", "*Non-detect",
                        "Not Detected", "Detected Not Quantified",
                        "*Present <QL")
  detectStrings <- c("*Present >QL", "Present Above Quantification Limit")

  # Look for unrecognized strings, omit these rows if discovered.
  strangeStrings <- setdiff(unique(na.omit(out$ResultDetectionConditionText)),
                            c(nonDetectStrings, detectStrings))
  if(length(strangeStrings) > 0) {
    warning(sprintf("Unrecognized detection limit condition text: %s. \
                    Omitting these rows.",
                  paste(strangeStrings, collapse = ", ")))
    out <- out[! (out$ResultDetectionConditionText %in%
                            strangeStrings), ]

  }

  islt <- function(x, y)
    !is.na(x) & !is.na(y) & x < y
  belowReportedLimit <- islt(out$ResultMeasureValue,
                             out$DetectionQuantitationLimitMeasure.MeasureValue)
  reportedBelowLimit <- out$ResultDetectionConditionText %in% nonDetectStrings
  # browser()
  out$is.bdl <- belowReportedLimit |
    reportedBelowLimit |
    out$ResultMeasureValue == 0 # Treat zeros as BDL, since these are concentrations


  # rows that have no useable detection limit info, yet are reported as BDL
  findBadRows <- function(wd) {
    noLimit <- with(wd,
                    is.na(DetectionQuantitationLimitMeasure.MeasureValue) |
                      is.na(DetectionQuantitationLimitMeasure.MeasureUnitCode) |
                      DetectionQuantitationLimitMeasure.MeasureUnitCode == "")
    wd$is.bdl & noLimit & !is.na(wd$is.bdl)
  }
  # May also be possible to get detection limit from ResultCommentText, but I
  # see only 1 observation out of 902 badrows where this would help.


  # set detection limit to maximum reported limit
  lookupDetLim <- function(wd) {
    wd_smry <- wd %>%
      group_by_(~CharacteristicName,
               ~ResultSampleFractionText,
               ~MonitoringLocationIdentifier) %>%
      dplyr::summarize(n = n())

    # maximum detection limit reported for each dataset
    goodrows <- out$is.bdl & !badrows # reported as below detection limit and have useable info
    wqp_maxdl <- out[goodrows, ] %>%
      dplyr::filter(CharacteristicName %in% wd_smry$CharacteristicName,
                    ResultSampleFractionText %in% wd_smry$ResultSampleFractionText,
                    MonitoringLocationIdentifier %in% wd_smry$MonitoringLocationIdentifier) %>%
      group_by_(~CharacteristicName,
               ~ResultSampleFractionText,
               ~MonitoringLocationIdentifier,
               ~ResultMeasure.MeasureUnitCode) %>%
      dplyr::summarize(maxdl = max(DetectionQuantitationLimitMeasure.MeasureValue)) %>% # May need better way to check units
      ungroup()
    # replace missing values with maximum reported
    wd1 <- dplyr::left_join(wd, wqp_maxdl, by = c("MonitoringLocationIdentifier",
                       "CharacteristicName",
                       "ResultSampleFractionText",
                       "ResultMeasure.MeasureUnitCode")) %>%
      dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureValue = maxdl,
             DetectionQuantitationLimitMeasure.MeasureUnitCode = ResultMeasure.MeasureUnitCode,
             DetectionQuantitationLimitTypeName = "Inferred from maximum reported in dataset") %>%
      dplyr::select(-maxdl)
    wd1
  }

  # Set detection limit to minimum non-bdl value
  lookupMinDet <- function(wd) {
    wd_smry <- wd %>%
      group_by_(~CharacteristicName,
               ~ResultSampleFractionText,
               ~MonitoringLocationIdentifier) %>%
      dplyr::summarize(n = n())

    # Minimum non-BDL value for each dataset
    wqp_mindet <- out %>%
      dplyr::filter(!is.bdl,
                    CharacteristicName %in% wd_smry$CharacteristicName,
                    ResultSampleFractionText %in% wd_smry$ResultSampleFractionText,
                    MonitoringLocationIdentifier %in% wd_smry$MonitoringLocationIdentifier,
                    !is.na(ResultMeasure.MeasureUnitCode),
                    ResultMeasure.MeasureUnitCode != "") %>%
      group_by_(~CharacteristicName,
               ~ResultSampleFractionText,
               ~MonitoringLocationIdentifier,
               ~ResultMeasure.MeasureUnitCode) %>%
      dplyr::summarize(mindet = min(ResultMeasureValue)) %>%
      ungroup()
    # replace missing values with minimum reported above detection limit
    wd1 <- dplyr::left_join(wd, wqp_mindet, by = c("MonitoringLocationIdentifier",
                                            "CharacteristicName",
                                            "ResultSampleFractionText",
                                            "ResultMeasure.MeasureUnitCode")) %>%
      dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureValue = mindet,
             DetectionQuantitationLimitMeasure.MeasureUnitCode = ResultMeasure.MeasureUnitCode,
             DetectionQuantitationLimitTypeName = "Maximum reported above limit in dataset.") %>%
      dplyr::select(-mindet)
    wd1
  }
  # replace detlim, detlim units with maximum reported detection limit
  badrows <- findBadRows(out)
  if (sum(badrows) > 0)
    out[badrows, ] <- lookupDetLim(out[badrows, ])

  # Replace remaining bad rows with minimum non-BDL value
  stillbad <- findBadRows(out)
  if (sum(stillbad) > 0)
    out[stillbad, ] <- lookupMinDet(out[stillbad, ])
  out <- wqp_setAttrs(out, attributes(wqpData), "BDL")

  out
}


#' Check depth measurements
#'
#' Assures that depth units are same and reference text is familiar.
#'
#' @param wqpData data.frame returned by readWQPData
#' @param units Desired units for depth measurements
#' @export

wqp_checkDepth <- function(wqpData, units = "m") {

  out <- wqpData

  # Look for unrecognized strings, omit these rows if discovered.
  surfaceStrings <- c("Surface", "From Surface", "SURFACE", "S")
  strangeStrings <- c(out$ActivityDepthAltitudeReferencePointText,
                      out$ResultDepthAltitudeReferencePointText) %>%
    na.omit() %>%
    unique() %>%
    setdiff(surfaceStrings)

  if(length(strangeStrings) > 0) {
    warning(sprintf("Unrecognized depth reference text: %s. \
                    Omitting these rows.",
                    paste(strangeStrings, collapse = ", ")))
    out <- out[! (out$ResultDepthAltitudeReferencePointText %in%
                    strangeStrings), ]
    out <- out[! (out$ActivityDepthAltitudeReferencePointText %in%
                    strangeStrings), ]
  }

  # Convert depths to desired units
  depthValCols <- grep("DepthHeightMeasure.MeasureValue", names(out),
                       fixed = TRUE)
  depthUnitCols <- grep("DepthHeightMeasure.MeasureUnitCode",
                        names(out), fixed = TRUE)
  newDepth <- Map(convertUnits, x = out[depthValCols], from = out[depthUnitCols],
                  to = rep(units, length(depthValCols)))
  out[depthValCols] <- lapply(newDepth, `[[`, "x")
  out[depthUnitCols] <- lapply(newDepth, `[[`, "units")

  out <- wqp_setAttrs(out, attributes(wqpData), check = "depth")
}


#' Check activity types
#'
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkActivity <- function(wqpData) {
  knownTypes <- c("Sample-Routine", "Sample", "Not determined",
                  "Field Msr/Obs", "Sample-Composite Without Parents",
                  "Sample-Integrated Cross-Sectional Profile",
                  "Sample-Integrated Vertical Profile",
                  "Sample-Integrated Horizontal Profile",
                  "Sample-Field Split",
                  "Field Msr/Obs-Portable Data Logger")

  out <- wqpData
  # Omit quality control samples
  out <- out[!grepl("Quality Control", out$ActivityTypeCode), ]
  unrecTypes_conc <- setdiff(out$ActivityTypeCode, knownTypes)
  if(length(unrecTypes_conc) > 0)
    warning(paste("Unknown activity types present in data:",
                  paste(unrecTypes_conc, collapse = "; ")))
  out <- wqp_setAttrs(out, attributes(wqpData), check = "activity")
  out
}


#' Helper functions for checkTZ
#'
#' returns timezones (Olson names) using geonames API.

geonamesTZ <- function(lat, lon, geonamesUser) {

  force(geonamesUser)

  oneTZ <- function(lat, lon) {
    url <- sprintf("http://api.geonames.org/timezoneJSON?lat=%s&lng=%s&radius=10&username=%s",
                   lat, lon, geonamesUser)
    # browser()
    res <- GET(url)
    httr::stop_for_status(res)

    out <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$timezoneId
    if (is.null(out))
      stop("No timezone recognized for location.")
    out
  }

  out <- unlist(Map(oneTZ, lat = lat, lon = lon))
  out
}

#' Lookup a state via the FIPS state code
#'
#' @param code FIPS state code
#' @param abb State abbreviation (i.e. postal code)
#' @export
statecodeLookup <- function(code = NULL, abb = NULL) {

  x <- code
  arg <- "Numeric.code"
  if (is.null(x)) {
    x <- abb
    arg <- "Alpha.code"
  }

  if (is.null(x))
    return(scTable)

  scTable[match(x, scTable[[arg]]),]
}

timezoneLookup <- function(lat, lon, statecode, geonamesUser) {

  state <- statecodeLookup(statecode)$Name
  state <- gsub("District of Columbia", "Washington, D.C.", state)
  stateOffsets <- tzTable$singleOffset[match(state, tzTable$State)]
  stateOlson <- olsonTbl$olson[match(stateOffsets, olsonTbl$offset)]

  if (sum(is.na(stateOlson)) > 0) {
    nas <- is.na(stateOlson)
    stateOlson[nas] <- geonamesTZ(lat = lat[nas], lon = lon[nas],
                                  geonamesUser = geonamesUser)
  }

  stateOlson
}

#' Check timezones for sites in wqpData
#'
#' Uses geonames API, for which you need a key. Uses mine for now.
#' @param wqpData data.frame returned by readWQPData
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export

wqp_checkTZ <- function(wqpData, geonamesUser = "markwh") {


  oneTZ <- function(lat, lon) {
    url <- sprintf("http://api.geonames.org/timezoneJSON?lat=%s&lng=%s&radius=10&username=%s",
                   lat, lon, geonamesUser)
    res <- GET(url)
    httr::stop_for_status(res)

    out <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$timezoneID
    if (is.null(out))
      stop("No timezone recognized for location.")
    out
  }

  stainfo <- attr(wqpData, "siteInfo")

  if (is.null(stainfo))
    stop("wqpData needs a 'siteInfo' attribute.")

  # add timezones in siteInfo attribute table
  tzs <- timezoneLookup(lat = stainfo$dec_lat_va, lon = stainfo$dec_lon_va,
                  statecode = stainfo$StateCode, geonamesUser = geonamesUser)
  stainfo$timezone <- tzs
  attr(wqpData, "siteInfo") <- stainfo

  # Get timezones for entire dataset
  out <- wqpData
  wqpTZ <- tzs[match(out$MonitoringLocationIdentifier,
                     stainfo$MonitoringLocationIdentifier)]

  # replace missing endDates with startDates
  out[["ActivityEndDate"]][is.na(out[["ActivityEndDate"]])] <-
    out[["ActivityStartDate"]][is.na(out[["ActivityEndDate"]])]

  # Which rows have no useable time information and must be treated as daily-resolution?
  isDaily <- is.na(out$ActivityStartTime.Time) |
    is.na(out$ActivityStartTime.TimeZoneCode)

  # Replace time info for such rows with bounds of entire day
  out[isDaily, ][["ActivityStartTime.Time"]] <- "00:00:00"
  out[isDaily, ][["ActivityEndTime.Time"]] <- "23:59:59"
  out[!isDaily, ][["ActivityEndTime.Time"]] <-
    out[!isDaily, ][["ActivityStartTime.Time"]]
  dstr <- "%Y-%m-%d"
  tstr <- "%H:%M:%S"
  startdt <- paste(format(out[["ActivityStartDate"]], dstr),
                   out[["ActivityStartTime.Time"]])
  enddt <- paste(format(out[["ActivityEndDate"]], dstr),
                 out[["ActivityEndTime.Time"]])
  out[["ActivityStartDateTime"]] <-
    toUTC(startdt, tzstring = wqpTZ,
          format = paste(dstr, tstr))
  out[["ActivityEndDateTime"]] <-
    toUTC(enddt, tzstring = wqpTZ,
          format = paste(dstr, tstr))

  out <- wqp_setAttrs(out, attributes = attributes(wqpData), check = "timezone")
  out
}



#' Check for and remove duplicates in a WQP data.frame
#'
#' unlike \code{unique}, this preserves attributes and a record of the check.
#' @param wqpData data.frame returned by readWQPData
#' @export

wqp_checkDuplicates <- function(wqpData) {
  out <- wqpData

  out <- unique(out)
  if (nrow(out) < nrow(wqpData))
    message(sprintf("Removing %s duplicate rows", nrow(wqpData) - nrow(out)))

  out <- wqp_setAttrs(out, attributes(wqpData), check = "duplicates")
}

# Simplify wqp data structures --------------------------------------------

#' Simplify wqp concentration data
#' @param wqpData processed WQP data result
#' @param average How to average the observations if at all. "depth" will
#' average observations over depth; "time" will aggregate to daily resolution.
#' @param allowDuplicates If \code{FALSE}, remove duplicates using \code{unique}
#' following simplification.
#' @details Note the following behavior when averaging. The is.bdl column
#' is also averaged, and therefore is no longer a logical vector, but a
#' decimal on [0, 1] indicating what proportion of averaged observations were bdl.
#' Some columns are dropped when averaging, namely the ones being averaged over,
#' while one is added, giving the number of observations being averaged.
#'
#' @export
wqp_simplifyConc <- function(wqpData, average = c("none", "depth", "time"),
                             allowDuplicates = FALSE,
                             redundantUnits = c("purge", "keep")) {

  average <- match.arg(average)
  redundantUnits <- match.arg(redundantUnits)

  checks <- attr(wqpData, "checks")
  neededChecks <- c("classes", "activity", "fraction", "units", "BDL")
  if (length(setdiff(neededChecks, checks)) > 0) {
    stop(paste("Please perform the following checks before simplifying:",
               paste(neededChecks, collapse = ", ")))
  }

  colmap <- c("Date" = "ActivityStartDate",
              "datetime" = "ActivityStartDateTime",
              "station" = "MonitoringLocationIdentifier",
              "char" = "CharacteristicName",
              "frac" = "ResultSampleFractionText",
              "conc" = "ResultMeasureValue",
              "conc.units" = "ResultMeasure.MeasureUnitCode",
              "conc.flag" = "ResultStatusIdentifier",
              "depth" = "ActivityDepthHeightMeasure.MeasureValue",
              "depth.units" = "ActivityDepthHeightMeasure.MeasureUnitCode",
              "detlim" = "DetectionQuantitationLimitMeasure.MeasureValue",
              "is.bdl" = "is.bdl")
  out <- wqpData[colmap]
  names(out) <- names(colmap)

  if (!allowDuplicates) {
    nr1 <- nrow(out)
    out <- unique(out)
    nr2 <- nrow(out)
    if (nr1 > nr2)
      message(sprintf("Omitting %s duplicate rows.", nr1 - nr2))
  }

  if (average != "none") {
    avgCols <- c("depth", "depth.units", "conc", "conc.flag", "is.bdl")
    if (average == "time")
      avgCols <- c(avgCols, "datetime")
    keepCols <- setdiff(names(colmap), avgCols)
    # print(keepCols)
    # browser()
    out <- out %>%
      group_by_(.dots = keepCols) %>%
      dplyr::summarize_(conc = ~mean(conc, na.rm = TRUE),
                 conc.flag = ~paste(conc.flag, collapse = ";"),
                 is.bdl = ~mean(is.bdl),
                 n_avg = ~n())
  }
  # browser()
  if (redundantUnits == "purge") {
    nr1 <- nrow(out)
    out <- out %>%
      group_by_(~station, ~char, ~frac) %>%
      dplyr::filter_(~conc.units == Mode(conc.units)) %>%
      ungroup()
    nr2 <- nrow(out)
    if (nr1 > nr2)
      message(sprintf("Omitting %s rows with redundant units.", nr1 - nr2))
  }

  out <- wqp_setAttrs(out, attributes(wqpData))
}

#' Unsimplify wqp concentration data
#' @param wqpData processed WQP data result
#' @export
wqp_complicateConc <- function(simpleConc) {
  colmap <- c("Date" = "ActivityStartDate",
              "datetime" = "ActivityStartDateTime",
              "station" = "MonitoringLocationIdentifier",
              "char" = "CharacteristicName",
              "frac" = "ResultSampleFractionText",
              "conc" = "ResultMeasureValue",
              "conc.units" = "ResultMeasure.MeasureUnitCode",
              "conc.flag" = "ResultStatusIdentifier",
              "detlim" = "DetectionQuantitationLimitMeasure.MeasureValue",
              "is.bdl" = "is.bdl")
  out <- simpleConc
  names(out) <- colmap
  # out <- wqp_checkClasses(out)
  out
}

#' Simplify wqp flow data
#' @param wqpData processed WQP data result
#' @param allowDuplicates If \code{FALSE}, remove duplicates using \code{unique}
#' following simplification.
#' @export
wqp_simplifyFlow <- function(wqpData, average = c("none", "time"),
                             allowDuplicates = FALSE) {
  average = match.arg(average)
  checks <- attr(wqpData, "checks")
  neededChecks <- c("units", "classes", "activity")
  if (length(setdiff(neededChecks, checks)) > 0) {
    stop(paste("Please perform the following checks before simplifying:",
               paste(neededChecks, collapse = ", ")))
  }

  colmap <- c("Date" = "ActivityStartDate",
              "datetime" = "ActivityStartDateTime",
              "station" = "MonitoringLocationIdentifier",
              "flowchar" = "CharacteristicName",
              "flow" = "ResultMeasureValue",
              "flow.units" = "ResultMeasure.MeasureUnitCode",
              "flow.flag" = "ResultStatusIdentifier")
  out <- wqpData[colmap]
  names(out) <- names(colmap)

  # Omit NA flow values
  nr1 <- nrow(out)
  out <- out[!is.na(out[["flow"]]) & out[["flow"]] != 0,]
  nr2 <- nrow(out)
  if (nr1 > nr2)
    message(sprintf("Omitting %s rows with zero or missing flow data.", nr1 - nr2))

  if (!allowDuplicates) {
    nr1 <- nrow(out)
    out <- unique(out)
    nr2 <- nrow(out)
    if (nr1 > nr2)
      message(sprintf("Omitting %s duplicate rows.", nr1 - nr2))
  }

  if (average == "time") {
    avgCols <- c("flow", "datetime", "flow.flag")
    keepCols <- setdiff(names(colmap), avgCols)
    out <- out %>%
      group_by_(.dots = keepCols) %>%
      dplyr::summarize_(flow = ~mean(flow, na.rm = TRUE),
                 flow.flag = ~paste(flow.flag, collapse = ";"),
                 n_avg = ~n())
  }

  out <- wqp_setAttrs(out, attributes(wqpData))
  out
}



#' Map wqpData monitoring stations
#'
#' @export

wqp_mapStations <- function(wqpData){
  sites <- attr(wqpData, "siteInfo")
  out <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(lng = ~dec_lon_va, lat = ~dec_lat_va, data = sites)

  out
}

#' Sets attributes of object to attributes only when such attributes aren't present in object already.
wqp_setAttrs <- function(object, attributes, check = NULL) {
  toset <- attributes[setdiff(names(attributes), names(attributes(object)))]

  attributes(object) <- c(attributes(object), toset)

  if (!is.null(check)) {
    attr(object, "checks") <- unique(c(attr(object, "checks"), check))
  }

  object
}


#' Make datasets for use in rcgam and similar functions
#'
#' Returns a list of rcData objects.
#'
#' @param simpleConc a data.frame returned by wqp_simplifyConc
#' @param simpleFlow a data.frame returned by wqp_simplifyFlow
#' @param bdl.threshold Days with greater a larger fraction than bdl.threshold bdl values will be marked as bdl.
makeRcData <- function(simpleConc, simpleFlow, bdl.threshold = 0,
                       type = c("rcData", "raw")) {

  type <- match.arg(type)

  rawdat <- simpleConc %>%
    dplyr::inner_join(simpleFlow, by = c("Date", "station")) %>%
    dplyr::mutate_(is.bdl = ~is.bdl > bdl.threshold)

  # Flow moments for conversion
  qsmry <- simpleFlow %>%
    group_by(station) %>%
    dplyr::summarize_(qbar = ~mean(log(flow), na.rm = TRUE),
               qsd  = ~sd(log(flow), na.rm = TRUE),
               n = ~sum(!is.na(flow)))

  rcid <- rawdat %>%
    transmute_(~station, ~char, ~frac, ~conc.units,
               rcID = ~make.names(paste(station, char, frac,
                                        conc.units, sep = "_"))) %>%
    dplyr::left_join(qsmry, by = "station")

  rawlist <- split(rawdat, f = rcid$rcID)

  # flow moments by dataset ID
  qMoms <- names(rawlist) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames("rcID") %>%
    dplyr::left_join(unique(rcid), by = "rcID")
  # browser()
  out <- Map(makeModelData, rawData = rawlist,
             qbar = qMoms$qbar, qsd = qMoms$qsd)
  if (type == "raw")
    out <- rawlist
  out
}
