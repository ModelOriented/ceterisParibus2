context("Check ceteris_paribus() function")

library("DALEX2")
library("randomForest")
set.seed(59)


HR_rf <- randomForest(status ~ . , data = HR)
explainer_rf <- explain(HR_rf, data = HRTest, y = HRTest)

my_HR <- HRTest[1, ]

lce_rf <- local_conditional_expectations(explainer_rf,  my_HR)
lce_rf2 <- local_conditional_expectations(HR_rf, data = HR, new_observation = my_HR, y = my_HR$evaluation, variable_splits = NULL)

test_that("Wrong input",{
  expect_error(local_conditional_expectations(explainer_rf,  NULL))
  expect_error(local_conditional_expectations(HR_rf, data = NULL, new_observation = my_HR, variable_splits = NULL))
  expect_error(local_conditional_expectations(apartments_rf_model))
})

test_that("Output format - local_conditional_expectations",{
  expect_is(lce_rf, "data.frame")
  expect_is(lce_rf, "individual_variable_profile_explainer")
  expect_is(lce_rf2, "data.frame")
  expect_is(lce_rf2, "individual_variable_profile_explainer")
})

