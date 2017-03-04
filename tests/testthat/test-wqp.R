context("wqp data retrievals and checks")

test_that("unit conversion works as expected", {

  expect_error(convertUnits(1, c("abc", "def"), "ghi"))
  expect_equal(convertUnits(1, "mg/L", "ug/L"),
               data.frame(x = 1000, units = "ug/L", stringsAsFactors = FALSE))

  expect_equal(convertUnits(1, "abc", "abc"),
               data.frame(x = 1, units = "abc", stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("def")),
               data.frame(x = c(1, 1), units = c("mg/L", "def"),
                          stringsAsFactors = FALSE))

  # omitting inconvertibles
  t1 <- convertUnits(c(1, 1), c("mg/L", "def"), to = c("def"),
               inconvertibles = "omit")
  t2 <- data.frame(x = c(1), units = c("def"),
                          stringsAsFactors = FALSE)
  expect_equal(t1[[1]], t2[[1]])
  expect_equal(t1[[2]], t2[[2]])

  # preserve inconvertibles
  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L")),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L", "def")),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "mg/kg"), to = c("ug/L", "ug/kg")),
               data.frame(x = c(1000, 1000), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))
  expect_equal(convertUnits(c(NA, NA), c("mg/l", "mg/kg"), to = c("ug/L", "ug/kg")),
               data.frame(x = c(NA_real_, NA_real_), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))
})

test_that("timing function works", {
  expect_true(round(timeit(Sys.sleep(1)), 3) == 1)
  expect_true(round(attr(timeit(1 + 1), "time"), 3) == 0)
})

test_that("wqp unit check works", {
  data(aluminumData)
  testdata <- aluminumData
  convto <- c("mg/l", "mg/kg")
  checked <- wqp_checkUnits(testdata, convertTo = convto, inconvertibles = "omit")

  units0 <- testdata$ResultMeasure.MeasureUnitCode
  vals0 <- testdata$ResultMeasureValue
  units1 <- checked$ResultMeasure.MeasureUnitCode
  vals1 <- checked$ResultMeasureValue

  dlunits0 <- testdata$DetectionQuantitationLimitMeasure.MeasureUnitCode
  dlvals0 <- testdata$DetectionQuantitationLimitMeasure.MeasureValue
  dlunits1 <- checked$DetectionQuantitationLimitMeasure.MeasureUnitCode
  dlvals1 <- checked$DetectionQuantitationLimitMeasure.MeasureValue

  expect_true(all(units1 %in% convto))
  expect_true(all(dlunits1 %in% c(convto, NA)))

  # units in checked must be one of convto
  # values in checked may be coerced to NA if unit conversion failed
  # units in testdata may be NA if not reported for zero values

  # expect zero-value units to be converted
  expect_gt(sum(vals0 == 0 & is.na(units0), na.rm = TRUE),
            sum(vals1 == 0, na.rm = TRUE))
  # expect rows with pcodes to all be converted

})

test_that("depthHeight are converted", {
  data(aluminumData)
  testdata <- aluminumData[1:3, ]
  convto <- c("mg/l", "mg/kg")

  depths <- runif(3)
  depthUnits <- c("feet", "ft", "km")

  testdata$ResultDepthHeightMeasure.MeasureValue <- depths
  testdata$ActivityDepthHeightMeasure.MeasureValue <- depths
  testdata$ActivityTopDepthHeightMeasure.MeasureValue <- depths
  testdata$ActivityBottomDepthHeightMeasure.MeasureValue <- depths

  testdata$ResultDepthHeightMeasure.MeasureUnitCode <- depthUnits
  testdata$ActivityDepthHeightMeasure.MeasureUnitCode <- depthUnits
  testdata$ActivityTopDepthHeightMeasure.MeasureUnitCode <- depthUnits
  testdata$ActivityBottomDepthHeightMeasure.MeasureUnitCode <- depthUnits

  newUnits <- c("m", "m", "m")
  newDepths <- convertUnits(depths, from = depthUnits, to = "m")

  checked <- wqp_checkDepth(testdata, units = "m")

  expect_equal(checked$ResultDepthHeightMeasure.MeasureValue, newDepths$x)
  expect_equal(checked$ActivityDepthHeightMeasure.MeasureValue, newDepths$x)
  expect_equal(checked$ActivityTopDepthHeightMeasure.MeasureValue, newDepths$x)
  expect_equal(checked$ActivityBottomDepthHeightMeasure.MeasureValue, newDepths$x)

  expect_equal(checked$ResultDepthHeightMeasure.MeasureUnitCode, newUnits)
  expect_equal(checked$ActivityDepthHeightMeasure.MeasureUnitCode, newUnits)
  expect_equal(checked$ActivityTopDepthHeightMeasure.MeasureUnitCode, newUnits)
  expect_equal(checked$ActivityBottomDepthHeightMeasure.MeasureUnitCode, newUnits)

  # expect_warning(wqp_checkUnits(testdata)) # Why did I expect a warning?
})

test_that("wqp fraction check works", {
  data("aluminumData")
  testdata <- aluminumData

  expect_message(wqp_checkFraction(testdata))
  expect_silent(wqp_checkFraction(testdata, silent = TRUE))
  expect_lte(nrow(wqp_checkFraction(testdata)),
            nrow(testdata))
})

test_that("wqp detection limit check works", {
  data("aluminumData")
  testdata <- aluminumData
  checked <- wqp_checkBDL(testdata)

  expect_null(testdata$is.bdl)
  expect_is(checked$is.bdl, "logical")

  # test unit conversion
  narows <- is.na(checked$ResultMeasure.MeasureUnitCode) |
    is.na(checked$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  expect_true(all(checked$ResultMeasure.MeasureUnitCode[!narows] ==
               checked$DetectionQuantitationLimitMeasure.MeasureUnitCode[!narows]))
})

test_that("wqp_checkTZ converts all timezones", {
  data("aluminumData")
  testdata <- aluminumData
  checked <- wqp_checkTZ(testdata)

  expect_is(checked, "data.frame")

  expect_equal(sum(is.na(checked$ActivityStartDateTime)),
               0)
  expect_equal(sum(is.na(checked$ActivityEndDateTime)),
               0)
  expect_equal(sum(format(checked$ActivityStartDateTime, "%Z") == "UTC"),
               nrow(checked))
  expect_equal(sum(format(checked$ActivityEndDateTime, "%Z") == "UTC"),
               nrow(checked))
})

test_that("Simplification works as expected", {
  data("nitrateData")
  expect_error(wqp_simplifyConc(nitrateData))
  testdata <- nitrateData %>%
    wqp_checkClasses() %>%
    wqp_checkActivity() %>%
    wqp_checkFraction() %>%
    wqp_checkUnits(convertTo = "mg/L", inconvertibles = "omit") %>%
    wqp_checkBDL()

  simp <- wqp_simplifyConc(testdata)
  expect_is(simp, "data.frame")



  simpsum <- wqp_simplifyConc(testdata, average = "time")
  expect_lt(nrow(simpsum), nrow(simp))

  data("no3Flow")
  expect_error(wqp_simplifyFlow(no3Flow))
  testdata <- no3Flow %>%
    wqp_checkClasses() %>%
    wqp_checkActivity() %>%
    wqp_checkUnits()

  qsimp <- wqp_simplifyFlow(testdata)
  expect_is(qsimp, "data.frame")

  qsimpsum <- wqp_simplifyFlow(testdata, average = "time")
  expect_lt(nrow(qsimpsum), nrow(qsimp))

})
