context("timezone conversion")

test_that("timezone lookup works correctly", {

  expect_equal(statecodeLookup(25)[["Alpha.code"]], "MA")
  expect_equal(statecodeLookup(abb = "MA")[["Numeric.code"]], 25)

  expect_equal(timezoneLookup(lat = 42.3, lon = -72.5,
                        statecode = statecodeLookup(abb = "MA")$Numeric.code,
                        geonamesUser = "markwh"),
               "America/New_York")
  expect_equal(timezoneLookup(lat = 46.4, lon = -90.1,
                        statecode = statecodeLookup(abb = "MI")$Numeric.code,
                        geonamesUser = "markwh"),
               "America/Menominee")

  expect_equal(timezoneLookup(lat = Inf, lon = -Inf,
                        statecode = statecodeLookup(abb = "CO")$Numeric.code,
                        geonamesUser = "markwh"),
               "America/Denver")

  expect_equal(timezoneLookup(lat = c(42.3, 46.4, Inf), lon = c(-72.5, -90.1, -Inf),
                              statecode = statecodeLookup(abb = c("MA", "MI", "CO"))$Numeric.code,
                              geonamesUser = "markwh"),
               c("America/New_York", "America/Menominee", "America/Denver"))
})

test_that("time zones are converted to UTC correctly", {

  expect_equal(toUTC("2011-01-02 22:44:01", "America/New_York"),
               as.POSIXct("2011-01-03 03:44:01 UTC", tz = "UTC"))

  expect_equal(toUTC("2011-07-02 22:44:01", "America/New_York"),
               as.POSIXct("2011-07-03 02:44:01 UTC", tz = "UTC"))

  expect_error(toUTC(c("2011-07-02 22:44:01", "2011-01-02 22:44:01"),
                     c("America/New_York")))

  expect_error(toUTC_oneTZ(c("2011-07-02 22:44:01", "2011-01-02 22:44:01"),
                     c("America/New_York", "America/New_York")))
})
