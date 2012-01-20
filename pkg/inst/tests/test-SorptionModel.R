context("class SorptionModel")

test_that("class initialization: default with no parameters.", {
  expect_that(SorptionModel(), is_a("SorptionModel"))
})

test_that("class initialization: 'knum' ( c(1, 5) ).", {
  expect_that(SorptionModel(knum=c(1, 5)), is_a("SorptionModel"))
})

test_that("class initialization: 'gases' ( c(1, 3) ).", {
  expect_that(SorptionModel(gases=c(1, 3)), is_a("SorptionModel"))
})

test_that("class initialization: 'gases' ( 2 ) via 'predict'.", {
  sm <- SorptionModel(gases=2)
  conc <- concSample(sm, type="inc")
  sconc <- predict(sm, conc)
  expect_that(length(conc), is_identical_to(length(sconc)))
})


