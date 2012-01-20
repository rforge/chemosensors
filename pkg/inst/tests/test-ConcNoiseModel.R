context("class ConcNoiseModel")

test_that("class initialization: default with no parameters.", {
  expect_that(ConcNoiseModel(), is_a("ConcNoiseModel"))
})

test_that("class initialization: 'gases' ( 2 ).", {
  expect_that(ConcNoiseModel(gases=2), is_a("ConcNoiseModel"))
})

test_that("class initialization: incorrect 'csd' ( -1 ).", {
  expect_that(ConcNoiseModel(csd=-1), throws_error("'csd' is negative"))
})

test_that("class initialization: 'csd' 3-element vector.", {
  expect_that(ConcNoiseModel(csd=c(0.1, 0.2, 0.3)), is_a("ConcNoiseModel"))
})

test_that("model: 'concSample' ( 1 column )", {
  gases <- 2
  model <- ConcNoiseModel(gases=gases)
  conc <- concSample(model)
  expect_that(ncol(conc), is_equivalent_to(1))
})

test_that("model: incorrect 'conc' ( 2 columns ), model ( 3 gases ).", {
  ngases <- 3
  model <- ConcNoiseModel(ngases=ngases)
  conc <- concSample(model)
  conc <- conc[, 1:2]
  expect_that(predict(model, conc), throws_error())
})
