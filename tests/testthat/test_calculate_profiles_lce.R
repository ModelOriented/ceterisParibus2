context("Check calculate profiles() functions")

library("DALEX2")
library("randomForest")
set.seed(59)
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district, data = apartments)
vars <- c("construction.year", "surface", "floor", "no.rooms", "district")

variable_splits <- calculate_variable_split(apartments, vars)
new_apartment <- apartments[1, ]

test_that("Output format - calculate_variable_profile",{
  expect_is(calculate_profiles_lce(new_apartment, 
                                   variable_splits,
                                   apartments_rf_model,
                                   dataset = apartments), "data.frame")
})