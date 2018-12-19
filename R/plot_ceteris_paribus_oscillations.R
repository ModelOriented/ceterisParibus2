#' Plots Oscillations for Individual Variable Profiles
#'
#' Function 'plot.individual_variable_profile_oscillations' plots variable importance plots.
#'
#' @param x a ceteris paribus oscillation explainer produced with function `calculate_variable_oscillations()`
#' @param ... other explainers that shall be plotted together
#'
#' @return a ggplot2 object
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
#' apartment <- apartments_test[1:2,]
#'
#' cp_rf <- individual_variable_profile(explainer_rf, apartment)
#' plot(cp_rf, color = "_ids_")
#'
#' vips <- individual_variable_oscillations(cp_rf)
#' vips
#' plot(vips)
#' }
plot.individual_variable_profile_oscillations <- function(x, ...) {

  x <- as.data.frame(x)
  x$`_vname_` <- reorder(x$`_vname_`, x$oscillations, mean, na.rm = TRUE)
  x$`_ids_` <- paste0("ID: ",x$`_ids_`)

  # plot it
  `_vname_` <- oscillations <- NULL
  ggplot(x, aes(`_vname_`, ymin = 0, ymax = oscillations)) +
    geom_errorbar() + coord_flip() +
    facet_wrap(~`_ids_`, scales = "free_y") +
    ylab("Individual Variable Oscillations") + xlab("") +
    theme_mi2()
}
