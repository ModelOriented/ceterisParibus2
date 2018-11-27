context("Check plot_ceteris_paribus() function")

library("DALEX")
library("randomForest")
library("e1071")
set.seed(59)

apartments_rf <- randomForest(m2.price ~ construction.year + surface + floor +
                                no.rooms + district, data = apartments)
explainer_rf <- explain(apartments_rf,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

apartments_lm <- lm(m2.price ~ construction.year + surface + floor +
                                no.rooms + district, data = apartments)
explainer_lm <- explain(apartments_lm,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

apartments_svm <- svm(m2.price ~ construction.year + surface + floor +
                                no.rooms + district, data = apartments)
explainer_svm <- explain(apartments_svm,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

# individual explanations
my_apartment <- apartmentsTest[1, ]

lp_rf <- individual_variable_profile(explainer_rf, my_apartment)
lp_lm <- individual_variable_profile(explainer_lm, my_apartment)
lp_svm <- individual_variable_profile(explainer_svm, my_apartment)

test_that("Output format - plot_ceteris_paribus",{
  expect_is(plot(lp_rf), "ggplot")
  expect_is(plot(lp_rf, lp_lm), "ggplot")
  expect_is(plot(lp_rf, lp_lm, lp_svm), "ggplot")
})

test_that("Output format - plot_ceteris_paribus - more parameters",{
  expect_is(plot(lp_rf, lp_lm, lp_svm, color = "_label_",
                 show_profiles = TRUE, show_observations = TRUE,
                 show_rugs = TRUE,
                 alpha = 0.3, alpha_points = 1,
                 size_points = 5, size_rugs = 0.5),
            "ggplot")
  expect_is(plot(lp_rf, show_profiles = TRUE, show_observations = TRUE,
                 color = "black",
                 selected_variables = c("construction.year", "surface"),
                 alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
                 size_points = 5, size_rugs = 0.5),
            "ggplot")
  expect_is(plot(lp_rf, show_profiles = TRUE, show_observations = TRUE,
                 color = "surface",
                 selected_variables = c("construction.year", "surface"),
                 alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
                 size_points = 5, size_rugs = 0.5),
            "ggplot")
  expect_is(plot(lp_rf, show_profiles = TRUE, show_observations = TRUE,
                 color = "darkblue", aggregate_profiles = mean,
                 selected_variables = c("construction.year", "surface"),
                 alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
                 size_points = 5, size_rugs = 0.5),
            "ggplot")
})

