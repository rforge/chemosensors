### About
# SensorDynamics: 
# - doesn't support arbitrary input
# - doesn't support pulses of arbitrary length
# - doesn't support negative sensor data
#
### Tests
# conc
# - pulse of air
# - 3 pulse of a gas
# errors
# - 'conc' length is not multiple by 'tunit'
# - 'sdata' minimum value is negative (< 0)

context("class SensorDynamics")

# conc
test_that("method 'predict' for a pulse of air.", {
  model <- SensorDynamics(tunit = 60)
  conc <- matrix(0, nrow = 120, ncol = ngases(model))
  sdata <- 10 * conc
  
  out <- predict(model, conc, sdata)
  
  expect_that(nrow(out) == nrow(conc), is_true())
})

test_that("method 'sdata2pulse' works with 3 pulses.", {
  model <- SensorDynamics(tunit = 60)
  conc <- matrix(0, nrow = 60 * 2 * 3, ncol = ngases(model))
  conc[c(1:60, 121:180, 241:300), 1] <- 1
  sdata <- 10 * conc
  
  out <- predict(model, conc, sdata)
  
  expect_that(nrow(out) == 60 * 2 * 3, is_true())
})

# errors
test_that("method 'predict' throws an error if length of pulses is not multiple if 'tunit'.", {
  model <- SensorDynamics(tunit = 60)
  conc <- matrix(0, nrow = 200, ncol = ngases(model))
  sdata <- 10 * conc
  
  expect_that(suppressWarnings(predict(model, conc, sdata)), throws_error("it must be composed of pulses"))
})

test_that("method 'predict' throws an error if min(sdata) < 0.", {
  model <- SensorDynamics(tunit = 60)
  conc <- matrix(0, nrow = 120, ncol = ngases(model))
  sdata <- -10 + 10 * conc

  expect_that(predict(model, conc, sdata), throws_error("sensor data must be non-negative"))
})

