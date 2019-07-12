context("aggregateSolute")
tryCatch({source("inst/tests/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("Unit aggregation and attach.units work", {
  ex <- data.frame(preds=1:15, dates=seq(as.Date("2000/1/1"), by = "week", length.out = 15))
  data(eg_metadata)

  # make some predictions with unit "aggregation"
  conc_preds_v <- loadflex:::aggregateSolute(preds=ex$preds, dates=ex$dates, format="conc", metadata=eg_metadata, agg.by="unit")
  flux_preds_v <- loadflex:::aggregateSolute(preds=ex$preds, dates=ex$dates, format="flux rate", metadata=eg_metadata, agg.by="unit")
  conc_preds_u <- loadflex:::aggregateSolute(preds=ex$preds, dates=ex$dates, format="conc", metadata=eg_metadata, agg.by="unit", attach.units=TRUE)
  flux_preds_u <- loadflex:::aggregateSolute(preds=ex$preds, dates=ex$dates, format="flux rate", metadata=eg_metadata, agg.by="unit", attach.units=TRUE)

  # check for expected column names
  expect_equal(names(conc_preds_v), c("unit", "conc", "count"))
  expect_equal(names(conc_preds_u), c("unit", "conc", "count"))
  expect_equal(names(flux_preds_v), c("unit", "flux.rate", "count"))
  expect_equal(names(flux_preds_u), c("unit", "flux.rate", "count"))

  # check that units can be attached or not
  expect_equal(unitted::get_units(conc_preds_v$conc), NA)
  expect_equal(unitted::get_units(flux_preds_v$flux.rate), NA)
  expect_equal(unitted::get_units(conc_preds_u$conc), eg_metadata@conc.units)
  expect_equal(unitted::get_units(flux_preds_u$flux.rate), eg_metadata@load.rate.units)
  expect_equal(unitted::get_units(conc_preds_u$unit), "")
  expect_equal(unitted::get_units(flux_preds_u$unit), "")
  expect_equal(unitted::get_units(conc_preds_u$count), "raw.preds")
  expect_equal(unitted::get_units(flux_preds_u$count), "raw.preds")

  # check that the values are equal to the predictions for these unit aggs
  expect_equal(conc_preds_v$conc, 1:15)
  expect_equal(flux_preds_v$flux.rate, 1:15)

})

test_that("Standard grouping works", {
  preds <- data.frame(
    date=as.Date("2019-01-01") + as.difftime(0:4, units='days'),
    fit=c(1, NA, 3, 4, 5))
  preds_u <- unitted::u(preds, c("", "kg d^-1"))

  # Daily grouping in this case should be like unit grouping
  daily <- loadflex:::aggregateSolute(preds, metadata=eg_metadata, format="conc", agg.by="day")
  expect_equal(daily$conc, preds$fit)

  # Filtering should work with grouping
  monthly <- loadflex:::aggregateSolute(preds, metadata=eg_metadata, format="conc", agg.by="month")
  expect_true(is.na(monthly$conc))
  monthly_filt <- loadflex:::aggregateSolute(preds, metadata=eg_metadata, format="conc", agg.by="month", na.rm=TRUE)
  expect_equal(monthly_filt$conc, mean(preds$fit, na.rm=TRUE))
})

test_that("Custom grouping is possible", {

  preds <- data.frame(
    date=as.Date("2019-01-01") + as.difftime(0:4, units='days'),
    fit=c(1, 2, 3, 4, 5),
    custom1=c('a','a','b','b','b'),
    "custom 2"=c('A','B','A','A','B'),
    check.names=FALSE)
  preds_u <- unitted::u(preds, c("", "kg d^-1", "", "q"))

  # Grouping should be possible with one or multiple custom columns
  custom1 <- loadflex:::aggregateSolute(preds, metadata=eg_metadata, format="conc", agg.by="custom1")
  expect_equal(nrow(custom1), 2)
  expect_equal(custom1$conc, c(1.5, 4))
  expect_equal(names(custom1), c('custom1', 'conc', 'count'))
  custom12 <- loadflex:::aggregateSolute(preds, metadata=eg_metadata, format="conc", agg.by=c("custom1","custom 2"))
  expect_equal(nrow(custom12), 4)
  expect_equal(names(custom12), c('custom1', 'custom 2', 'conc', 'count'))

  # Custom grouping should preserve units
  custom12u <- loadflex:::aggregateSolute(preds_u, metadata=eg_metadata, format="flux rate", agg.by=c("custom1","custom 2"), attach.units=TRUE)
  expect_equal(unname(unitted::get_units(custom12u)), c("", "q", "kg d^-1", "raw.preds"))
})
