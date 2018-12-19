#' Local Conditional Expectation Explainer
#'
#' This explainer works for individual observations.
#' For each observation it calculates Local Conditional Expectation (LCE) profiles for selected variables.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param y true labels for `new_observation`. If specified then will be added to ceteris paribus plots.
#' @param variables names of variables for which profiles shall be calculated. Will be passed to `calculate_variable_splits()`. If NULL then all variables from the validation data will be used.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with `calculate_variable_splits()`. If NULL then it will be calculated based on validation data avaliable in the `explainer`.
#' @param grid_points number of points for profile. Will be passed to `calculate_variable_splits()`.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#'
#' @return An object of the class 'individual_variable_profile_explainer'.
#' A data frame with calculated LCE profiles.
#' @export
#'
#' @examples
#' library("DALEX2")
#'  \dontrun{
#' library("randomForest")
#' set.seed(59)
#'
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'       no.rooms + district, data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'       data = apartments[,2:6], y = apartments$m2.price)
#'
#' new_apartment <- apartments[1, ]
#'
#' lce_rf <- individual_conditional_expectations(explainer_rf, new_apartment)
#' lce_rf
#'
#' lce_rf <- individual_conditional_expectations(explainer_rf, new_apartment,
#'           y = new_apartment$m2.price)
#' lce_rf
#'
#' plot(lce_rf)
#' }
#' @export
#' @rdname individual_conditional_expectations
individual_conditional_expectations <- function(x, ...)
  UseMethod("individual_conditional_expectations")

#' @export
#' @rdname individual_conditional_expectations
individual_conditional_expectations.explainer <- function(x, new_observation, y = NULL, variables = NULL,
                                                  variable_splits = NULL, grid_points = 101,
                                                  ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  individual_variable_profile.default(model, data, predict_function,
                                      new_observation = new_observation,
                                      label = label,
                                      variables = variables,
                                      grid_points = grid_points,
                                      y = y,
                                      ...)
}
#' @export
#' @rdname individual_conditional_expectations
individual_conditional_expectations.default <- function(x, data, predict_function = predict,
                                                   new_observation, y = NULL, variable_splits = NULL,
                                                   variables = NULL, grid_points = 101,
                                                   label = class(x)[1], ...) {

  model <- x

  # if splits are not provided, then will be calculated
  if (is.null(variable_splits)) {
    # need validation data from the explainer
    if (is.null(data))
      stop("The individual_conditional_expectations() function requires explainers created with specified 'data'.")
    # need variables, if not provided, will be extracted from data
    if (is.null(variables))
      variables <- intersect(colnames(data),
                             colnames(new_observation))

    variable_splits <- calculate_variable_split(data, variables = variables, grid_points = grid_points)
  }

  # calculate profiles
  profiles <- calculate_profiles_lce(new_observation, variable_splits, model, dataset = data, predict_function)
  profiles$`_label_` <- label

  # add points of interests
  new_observation$`_yhat_` <- predict_function(model, new_observation)
  if (!is.null(y)) new_observation$`_y_` <- y
  new_observation$`_label_` <- label

  # prepare final object
  attr(profiles, "observations") <- new_observation
  class(profiles) = c("individual_variable_profile_explainer", "data.frame")
  profiles
}
