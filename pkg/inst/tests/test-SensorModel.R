context("class SensorModel")

test_that("class initialization: default with no parameters.", {
  expect_that(SensorModel(), is_a("SensorModel"))
})

test_that("class initialization: incorrect 'num' ( 0 ).", {
  expect_that(SensorModel(num=0), throws_error("'num' is incorrect"))
})

test_that("class initialization: incorrect 'num' ( 20 ).", {
  expect_that(SensorModel(num=20), throws_error("'num' is incorrect"))
})

test_that("class initialization: 'gases' ( 2 ).", {
  expect_that(SensorModel(gases=2), is_a("SensorModel"))
})

test_that("class initialization: 'gases' ( 1, 3 ).", {
  expect_that(SensorModel(gases=c(1, 3)), is_a("SensorModel"))
})

test_that("model: incorrect 'conc' ( 2 columns ), model ( 3 gases ).", {
  pck <- defaultDataPackage()
  data(UNIMANshort, package=pck)
  model <- SensorModel(gases=1:3)
  conc <- C[, 1:2]
  expect_that(predict(model, conc), throws_error("dimension of 'conc' is incorrect"))
})


test_that("consistency concModel/sdataModel: ( num 1 ).", {
  num <- 1
  pck <- defaultDataPackage()
  data(UNIMANshort, package=pck)
  model <- SensorModel(num)
  conc <- C 
  sdata <- predict(model, conc)
  sdata0 <- dat[, num]  
  sdata.rmse <- sqrt(mean((sdata - sdata0)^2))

  expect_that(sdata.rmse < sd(sdata0), is_true())
})


