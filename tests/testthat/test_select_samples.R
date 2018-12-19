context("Check select_samples() function")

apartmentsTest <- apartments_test

test_that("Output select samples",{
  expect_is(select_sample(apartmentsTest), "data.frame")
})
