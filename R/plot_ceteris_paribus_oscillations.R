#' Plot Individual Variable Oscillations
#'
#' Function 'plot.individual_variable_oscillations' plots variable importance plots.
#'
#' @param x a ceteris paribus oscillation explainer produced with function `calculate_oscillations()`
#' @param ... other explainers that shall be plotted together
#'
#' @return a ggplot2 object
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
#'       data = apartmentsTest, y = apartmentsTest$m2.price)
#'
#' apartment <- apartmentsTest[1:2,]
#'
#' cp_rf <- local_profile(explainer_rf, apartment)
#' plot(cp_rf, color = "_ids_")
#'
#' vips <- calculate_oscillations(cp_rf)
#' vips
#' plot(vips)
#' }
plot.individual_variable_oscillations <- function(x, ...) {

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
