context("class Scenario")

test_that("class initialization: different labels in T.", {
  T <- c("A 0.05", "B", "A 0.05, B 0.05", "A 0.05, B", "A, B")
  sc <- try(Scenario(concUnits = "perc", T = T))
  expect_that(class(sc) == "Scenario", is_true())
})

