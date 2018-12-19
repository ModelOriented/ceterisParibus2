#' Calculates Oscillations for Individual Variable Profiles
#'
#' @param x an individual variable profile explainer produced with the `individual_variable_profile()` function
#' @param sort a logical value. If `TRUE` then rows are sorted along the oscillations
#' @param ... other arguments
#'
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
#'       data = apartments_test, y = apartments_test$m2.price)
#'
#' apartment <- apartments_test[1,]
#'
#' ivp_rf <- individual_variable_profile(explainer_rf, apartment)
#' individual_variable_oscillations(ivp_rf)
#' }
individual_variable_oscillations <- function(x, sort = TRUE, ...) {
  stopifnot("individual_variable_profile_explainer" %in% class(x))

  observations <- attr(x, "observations")
  variables <- unique(as.character(x$`_vname_`))
  ids <- unique(as.character(x$`_ids_`))
  lapply(variables, function(variable) {
    lapply(ids, function(id) {
      diffs <- x[x$`_vname_` == variable & x$`_ids_` == id,"_yhat_"] - observations[id,"_yhat_"]
      data.frame(`_vname_` = variable, `_ids_` = id, oscillations = mean(abs(diffs)))
    }) -> tmp
    do.call(rbind, tmp)
  }) -> tmp
  res <- do.call(rbind, tmp)
  colnames(res) <- c("_vname_", "_ids_", "oscillations")
  if (sort) {
    res <- res[order(res$oscillations, decreasing = TRUE),]
  }
  class(res) = c("individual_variable_profile_oscillations", "data.frame")
  res
}

