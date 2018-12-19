context("Check calculate profiles() functions")

library("DALEX2")
library("randomForest")
set.seed(59)
apartmentsTest <- apartments_test

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district, data = apartments)
vars <- c("construction.year", "surface", "floor", "no.rooms", "district")

test_that("Output format - calculate_variable_split",{
  expect_is(calculate_variable_split(apartments, vars), "list")
})

variable_splits <- calculate_variable_split(apartments, vars)
new_apartment <- apartmentsTest[1:10, ]

test_that("Output format - calculate_variable_profile",{
  expect_is(calculate_variable_profile(new_apartment, variable_splits,
                                       apartments_rf_model), "data.frame")
})
