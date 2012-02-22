context("class Sensor")

test_that("class initialization: default with no parameters.", {
  expect_that(Sensor(), is_a("SensorArray"))
})

test_that("class initialization: incorrect 'num' ( 0 ).", {
  expect_that(Sensor(num=0), throws_error())
})

test_that("class initialization: incorrect 'num' ( 20 ).", {
  expect_that(Sensor(num=20), throws_error())
})

test_that("class initialization: 'gases' ( 2 ).", {
  expect_that(Sensor(gases=2), is_a("SensorArray"))
})

test_that("class initialization: 'gases' ( 1, 3 ).", {
  expect_that(Sensor(gases=c(1, 3)), is_a("SensorArray"))
})

test_that("model: incorrect 'conc' ( 2 columns ), model ( 3 gases ).", {
  pck <- defaultDataPackage()
  data(UNIMANshort, package=pck)
  model <- Sensor(gases=1:3)
  conc <- C[, 1:2]
  expect_that(predict(model, conc), throws_error("dimension of 'conc' is incorrect"))
})

test_that("sub-class SensorModel: consistence of 'predict'.", {
  num <- 1
  pck <- defaultDataPackage()
  data(UNIMANshort, package=pck)
  s <- Sensor(num=num, csd=0, ssd=0, dsd=0, enableSorption=FALSE)
  sm <- SensorModel(num=num)
  conc <- C 
  sdata.s <- predict(s, conc)
  sdata.sm <- predict(sm, conc)
  sdata.rmse <- sqrt(mean((sdata.s - sdata.sm)^2))  
  expect_that(sdata.rmse < 1, is_true())  
})

test_that("sub-class SorptionModel: consistence of 'predict'.", {
  num <- 1
  s1 <- Sensor(num=num, csd=0, ssd=0, dsd=0, enableSorption=TRUE)
  s2 <- Sensor(num=num, csd=0, ssd=0, dsd=0, enableSorption=FALSE)  
  sm <- SensorModel(num=num)
  conc <- concSample(s1)
  sdata1 <- predict(s1, conc)
  sdata2 <- predict(s2, conc)
  sdata.rmse <- sqrt(mean((sdata1 - sdata2)^2))
  expect_that(sdata.rmse < 1, is_true())  
})

test_that("sub-class ConcNoiseModel: csd = 0 vs. csd = 1.", {
  ngases <- 1
  s1 <- Sensor(ngases=ngases, csd=0, ssd=0, dsd=0)
  s2 <- Sensor(ngases=ngases, csd=1, ssd=0, dsd=0)
  conc <- concSample(s1)
  sdata1 <- predict(s1, conc)
  sdata2 <- predict(s2, conc)
  expect_that(sd(as.numeric(sdata1)) < sd(as.numeric(sdata2)), is_true())
})
