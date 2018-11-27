context("Check select_neighbours() function")

new_apartment <- DALEX::apartments[1, 2:6]

test_that("Output format - select_neighbours",{
  expect_is(select_neighbours(DALEX::apartmentsTest, new_apartment, n = 10), "data.frame")
  expect_is(select_neighbours(DALEX::apartmentsTest, new_apartment, n = NULL, frac = 0.001), "data.frame")
})
