context("Check print_profile() function")

library("DALEX2")
library("randomForest")
set.seed(59)
apartmentsTest <- apartments_test

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
      no.rooms + district, data = apartments)
explainer_rf <- explain(apartments_rf_model,
      data = apartmentsTest, y = apartmentsTest$m2.price)

apartment <- apartmentsTest[1:2,]

cp_rf <- individual_variable_profile(explainer_rf, apartment)

vips <- individual_variable_oscillations(cp_rf)

test_that("Output produced - print_profile",{
  expect_failure(expect_output(print(vips), NA), "produced output")
  # expect_false(withVisible(print(vips))$visible)
})
