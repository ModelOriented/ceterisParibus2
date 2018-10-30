context("Check ceteris_paribus() function")

test_that("Wrong input",{
  expect_error(ceteris_paribus(apartments_rf_model))
})

