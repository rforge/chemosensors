context("class SensorNoiseModel")

test_that("class initialization: default with no parameters.", {
  expect_that(SensorNoiseModel(), is_a("SensorNoiseModel"))
})

test_that("class initialization: 'gases' ( 2 ).", {
  expect_that(SensorNoiseModel(gases=2), is_a("SensorNoiseModel"))
})

test_that("class initialization: incorrect 'ssd' ( -1 ).", {
  expect_that(SensorNoiseModel(ssd=-1), throws_error("'ssd' is negative"))
})

#test_that("class initialization: 'ssd' 3-element vector.", {
#  expect_that(SensorNoiseModel(ssd=c(0.1, 0.2, 0.3)), is_a("SensorNoiseModel"))
#})

test_that("parameters: 'sndata' for 'SensorModel'.", {
  pck <- defaultDataPackage()
  data(UNIMANsnoise, package=pck) # -> 'Bsd'
  model <- "plsr"
  sa <- SensorArray(model=model, enableSorption=FALSE, num=1:17)
  
  expect_that(Bsd[["SensorModel"]][[model]], is_equivalent_to(sa@sndata))
})

test_that("parameters: 'sndata' for 'Sensor'.", {
  pck <- defaultDataPackage()
  data(UNIMANsnoise, package=pck) # -> 'Bsd'
  model <- "plsr"
  sa <- SensorArray(model=model, num=1:17)
  
  expect_that(Bsd[["Sensor"]][[model]], is_equivalent_to(sa@sndata))
})
