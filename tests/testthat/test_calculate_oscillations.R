context("Check calculate_oscilations() functions")

library("DALEX")
library("randomForest")
set.seed(59)

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
      no.rooms + district, data = apartments)

explainer_rf <- explain(apartments_rf_model,
      data = apartmentsTest, y = apartmentsTest$m2.price)

apartment <- apartmentsTest[1,]

ivp_rf <- individual_variable_profile(explainer_rf, apartment)


test_that("Output format - individual_variable_oscillations",{
  expect_is(individual_variable_oscillations(ivp_rf), "data.frame")
  expect_is(individual_variable_oscillations(ivp_rf), "individual_variable_profile_oscillations")
})

test_that("Wrong input - individual_variable_oscillations",{
  expect_error(individual_variable_oscillations(apartments_rf_model))
})
