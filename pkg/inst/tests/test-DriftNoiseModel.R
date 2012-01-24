context("class DriftNoiseModel")

test_that("class initialization: default with no parameters.", {
  expect_that(DriftNoiseModel(), is_a("DriftNoiseModel"))
})

test_that("class initialization: error on 'nsensors > ndnum'.", {
  expect_that(DriftNoiseModel(num=1:2, ndcomp=3), throws_error("'ndcomp' is incosistent with 'num'"))
})

