context("Check print_ceteris_paribus() function")

library("DALEX")
library("randomForest")
set.seed(59)

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district, data = apartments)
explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest, y = apartmentsTest$m2.price)

apartment <- apartmentsTest[1:2,]

cp_rf <- individual_variable_profile(explainer_rf, apartment)

test_that("Output produced - print_ceteris_paribus",{
  expect_failure(expect_output(print(cp_rf), NA), "produced output")
  # expect_false(withVisible(print(cp_rf))$visible)
})
