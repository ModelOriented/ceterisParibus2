#' Local Profiles via Ceteris Paribus Explainer
#'
#' This function calculate Ceteris Paribus profiles for selected data points.
#'
#' @param explainer a model to be explained, preprocessed by function `DALEX::explain()`.
#' @param observations set of observarvation for which profiles are to be calculated
#' @param y true labels for `observations`. If specified then will be added to ceteris paribus plots.
#' @param variable_splits named list of splits for variables, in most cases created with `calculate_variable_splits()`. If NULL then it will be calculated based on validation data avaliable in the `explainer`.
#' @param grid_points number of points for profile. Will be passed to `calculate_variable_splits()`.
#' @param variables names of variables for which profiles shall be calculated. Will be passed to `calculate_variable_splits()`. If NULL then all variables from the validation data will be used.
#'
#' @return An object of the class 'ceteris_paribus_explainer'.
#' It's a data frame with calculated average responses.
#' @export
#'
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("randomForest")
#' set.seed(59)
#'
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'       no.rooms + district, data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'       data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#'
#' apartments_small <- select_sample(apartmentsTest, 10)
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartments_small)
#' cp_rf
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartments_small, y = apartments_small$m2.price)
#' cp_rf
#' }
#' @export
#' @rdname local_profile
local_profile <- function(x, ...)
  UseMethod("local_profile")


#' @export
#' @rdname local_profile
local_profile.explainer <- function(x, new_observation, variables = NULL,
                                    variable_splits = NULL, grid_points = 101,
                                    ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  local_profile.default(model, data, predict_function,
                             new_observation = new_observation,
                             label = label,
                             ...)
}


#' @export
#' @rdname local_profile
local_profile.default <- function(x, data, y = NULL, predict_function = predict,
                                      new_observation, variables = NULL,
                                      variable_splits = NULL,
                                      grid_points = 101,
                                      label = class(x)[1], ...) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }
  p <- ncol(data)

  # calculate splits
  # if splits are not provided, then will be calculated
  if (is.null(variable_splits)) {
    # need validation data from the explainer
    if (is.null(data))
      stop("The ceteris_paribus() function requires explainers created with specified 'data'.")
    # need variables, if not provided, will be extracted from data
    if (is.null(variables))
      variables <- colnames(data)

    variable_splits <- calculate_variable_splits(data, variables = variables, grid_points = grid_points)
  }

  # calculate profiles
  profiles <- calculate_profiles(new_observation, variable_splits, x, predict_function)

  # if there is more then one collumn with `_yhat_`
  # then we need to convert it to a single collumn
  col_yhat <- grep(colnames(profiles), pattern = "^_yhat_")
  if (length(col_yhat) == 1) {
    profiles$`_label_` <- label
  } else {
    # we need to recreate _yhat_ and create proper labels
    new_profiles <- profiles[rep(1:nrow(profiles), times = length(col_yhat)), -col_yhat]
    new_profiles$`_yhat_` <- unlist(c(profiles[,col_yhat]))
    stripped_names <- gsub(colnames(profiles)[col_yhat], pattern = "_yhat_", replacement = "")
    new_profiles$`_label_` <- paste0(label, rep(stripped_names, each = nrow(profiles)))
    profiles <- new_profiles
  }

  # prepare final object
  attr(profiles, "observations") <- new_observation
  class(profiles) = c("ceteris_paribus_explainer", "data.frame")
  profiles
}


#' @export
#' @rdname local_profile
ceteris_paribus <- function(explainer, observations, y = NULL,
                            variable_splits = NULL, variables = NULL,
                            grid_points = 101) {
  if (!("explainer" %in% class(explainer)))
      stop("The ceteris_paribus() function requires an object created with explain() function.")

  predict_function <- explainer$predict_function
  model <- explainer$model

  # if splits are not provided, then will be calculated
  if (is.null(variable_splits)) {
    # need validation data from the explainer
    if (is.null(explainer$data))
      stop("The ceteris_paribus() function requires explainers created with specified 'data'.")
    # need variables, if not provided, will be extracted from data
    if (is.null(variables))
      variables <- intersect(colnames(explainer$data),
                             colnames(observations))

    variable_splits <- calculate_variable_splits(explainer$data, variables = variables, grid_points = grid_points)
  }

  # calculate profiles
  profiles <- calculate_profiles(observations, variable_splits, model, predict_function)
  profiles$`_label_` <- explainer$label

  # add points of interests
  observations$`_yhat_` <- predict_function(model, observations)
  if (!is.null(y)) observations$`_y_` <- y
  observations$`_label_` <- explainer$label

  # prepare final object
  attr(profiles, "observations") <- observations
  class(profiles) = c("ceteris_paribus_explainer", "data.frame")
  profiles
}
