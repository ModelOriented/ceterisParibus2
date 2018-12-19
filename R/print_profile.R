#' Prints Individual Variable Profiles
#'
#' @param x an individual variable profile explainer produced with the `individual_variable_profile` function
#' @param ... other arguments that will be passed to `head()`
#'
#' @export
#' @importFrom utils head
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
#' apartment <- apartments_test[1:2,]
#'
#' cp_rf <- individual_variable_profile(explainer_rf, apartment)
#' plot(cp_rf, color = "_ids_")
#'
#' vips <- individual_variable_oscillations(cp_rf)
#' vips
#' }
print.individual_variable_profile_oscillations <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x, ...))
}
