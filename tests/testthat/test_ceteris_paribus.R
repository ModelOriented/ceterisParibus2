context("Check ceteris_paribus() function")

library("DALEX2")
library("randomForest")
set.seed(59)
apartmentsTest <- apartments_test
HRTest <- HR_test

# apartments_rf <- randomForest(m2.price ~ construction.year + surface + floor +
#                                 no.rooms + district, data = apartments)
#
# explainer_rf <- explain(apartments_rf,
#                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#
# my_apartment <- apartmentsTest[1, ]
#
# lp_rf <- individual_variable_profile(explainer_rf, my_apartment)
# lp_rf
#
# plot(lp_rf)

# --------
# multiclass

HR_rf <- randomForest(status ~ . , data = HR)
explainer_rf <- explain(HR_rf, data = HRTest, y = HRTest)

my_HR <- HRTest[1, ]

lp_rf <- individual_variable_profile(explainer_rf,  my_HR)
lp_rf2 <- individual_variable_profile(HR_rf, data = HR, new_observation = my_HR, y = my_HR$evaluation, variable_splits = NULL)

test_that("Wrong input",{
  expect_error(individual_variable_profile(explainer_rf,  NULL))
  expect_error(individual_variable_profile(HR_rf, data = NULL, new_observation = my_HR, variable_splits = NULL))
  expect_error(ceteris_paribus(apartments_rf_model))
})

test_that("Output format - individual_variable_profile",{
  expect_is(lp_rf, "individual_variable_profile_explainer")
  expect_is(lp_rf, "data.frame")
  expect_is(lp_rf2, "individual_variable_profile_explainer")
  expect_is(lp_rf2, "data.frame")

})

