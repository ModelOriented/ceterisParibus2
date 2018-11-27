context("Check plot_ceteris_paribus_oscillations() function")

library("DALEX")
library("randomForest")
set.seed(59)

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
      no.rooms + district, data = apartments)
explainer_rf <- explain(apartments_rf_model,
      data = apartmentsTest, y = apartmentsTest$m2.price)

apartment <- apartmentsTest[1:2,]

cp_rf <- individual_variable_profile(explainer_rf, apartment)
vips <- individual_variable_oscillations(cp_rf)

test_that("Output format - plot_ceteris_paribus_oscillations",{
  expect_is(plot(vips), "ggplot")
})
