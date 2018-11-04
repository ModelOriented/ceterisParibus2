#' Plots Individual Variable Profile Explanations
#'
#' Function 'plot.individual_variable_profile_explainer' plots Individual Variable Profiles for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' @param x a ceteris paribus explainer produced with function `individual_variable_profile()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param color_points a character. Either name of a color or name of a variable that should be used for coloring
#' @param size_points a numeric. Size of points to be plotted
#' @param alpha_points a numeric between 0 and 1. Opacity of points
#' @param color_rugs a character. Either name of a color or name of a variable that should be used for coloring
#' @param size_rugs a numeric. Size of rugs to be plotted
#' @param alpha_rugs a numeric between 0 and 1. Opacity of rugs
#' @param color_residuals a character. Either name of a color or name of a variable that should be used for coloring for residuals
#' @param size_residuals a numeric. Size of line and points to be plotted for residuals
#' @param alpha_residuals a numeric between 0 and 1. Opacity of points and lines for residuals
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#' @param show_profiles a logical. If TRUE then profiles will be plotted. Either individual or aggregate (see `aggregate_profiles`)
#' @param aggregate_profiles function. If NULL (default) then individual profiles will be plotted. If a function (e.g. mean or median) then profiles will be aggregated and only the aggregate profile will be plotted
#' @param show_observations a logical. If TRUE then individual observations will be marked as points
#' @param show_rugs a logical. If TRUE then individual observations will be marked as rugs
#' @param show_residuals a logical. If TRUE then residuals will be plotted as a line ended with a point
#' @param facet_ncol number of columns for the `facet_wrap()`
#' @param selected_variables if not NULL then only `selected_variables` will be presented
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @importFrom DALEX theme_mi2
#' @importFrom stats aggregate
#'
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("randomForest")
#' set.seed(59)
#'
#' apartments_rf <- randomForest(m2.price ~ construction.year + surface + floor +
#'                                 no.rooms + district, data = apartments)
#' explainer_rf <- explain(apartments_rf,
#'                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#'
#' apartments_lm <- lm(m2.price ~ construction.year + surface + floor +
#'                                 no.rooms + district, data = apartments)
#' explainer_lm <- explain(apartments_lm,
#'                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#'
#' library("e1071")
#' apartments_svm <- svm(m2.price ~ construction.year + surface + floor +
#'                                 no.rooms + district, data = apartments)
#' explainer_svm <- explain(apartments_svm,
#'                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#'
#' # individual explanations
#' my_apartment <- apartmentsTest[1, ]
#'
#' # for random forest
#' lp_rf <- individual_variable_profile(explainer_rf, my_apartment)
#' lp_rf
#'
#' plot(lp_rf)
#'
#' # for others
#' lp_lm <- individual_variable_profile(explainer_lm, my_apartment)
#' plot(lp_rf, lp_lm, color = "_label_")
#'
#' # for others
#' lp_svm <- individual_variable_profile(explainer_svm, my_apartment)
#' plot(lp_rf, lp_lm, lp_svm, color = "_label_")
#'
#' # more parameters
#' plot(lp_rf, lp_lm, lp_svm, color = "_label_",
#'    show_profiles = TRUE, show_observations = TRUE,
#'    show_rugs = TRUE,
#'    alpha = 0.3, alpha_points = 1,
#'    size_points = 5, size_rugs = 0.5)
#'
#' plot(lp_rf, show_profiles = TRUE, show_observations = TRUE,
#'    color = "black",
#'    selected_variables = c("construction.year", "surface"),
#'    alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
#'    size_points = 5, size_rugs = 0.5)
#'
#' plot(lp_rf, show_profiles = TRUE, show_observations = TRUE,
#'    color = "surface",
#'    selected_variables = c("construction.year", "surface"),
#'    alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
#'    size_points = 5, size_rugs = 0.5)
#'
#' plot(lp_rf, show_profiles = TRUE, show_observations = TRUE,
#'    color = "darkblue", aggregate_profiles = mean,
#'    selected_variables = c("construction.year", "surface"),
#'    alpha = 0.3, alpha_points = 1, alpha_residuals = 0.5,
#'    size_points = 5, size_rugs = 0.5)
#'
#' # --------
#' # multiclass
#'
#' HR_rf <- randomForest(status ~ . , data = HR)
#' explainer_rf <- explain(HR_rf, data = HRTest, y = HRTest)
#'
#' my_HR <- HRTest[1, ]
#'
#' lp_rf <- individual_variable_profile(explainer_rf, my_HR)
#' lp_rf
#'
#' plot(lp_rf, color = "_label_")
#' }
plot.individual_variable_profile_explainer <- function(x, ...,
   size = 1,
   alpha = 0.3,
   color = "black",
   size_points = 2,
   alpha_points = 1,
   color_points = color,
   size_rugs = 0.5,
   alpha_rugs = 1,
   color_rugs = color,
   size_residuals = 1,
   alpha_residuals = 1,
   color_residuals = color,

   only_numerical = TRUE,

   show_profiles = TRUE,
   show_observations = TRUE,
   show_rugs = FALSE,
   show_residuals = FALSE,
   aggregate_profiles = NULL,
   facet_ncol = NULL, selected_variables = NULL) {

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_observations <- lapply(dfl, function(tmp) {
    attr(tmp, "observations")
  })
  all_observations <- do.call(rbind, all_observations)
  all_observations$`_ids_` <- factor(rownames(all_observations))
  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(selected_variables)) {
    all_variables <- intersect(all_variables, selected_variables)
    if (length(all_variables) == 0) stop(paste0("selected_variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  # is color a variable or literal?
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")
  # only numerical or only factors?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
  if (only_numerical) {
    vnames <- names(which(is_numeric))
    if (length(vnames) == 0) stop("There are no numerical variables")
    all_profiles$`_x_` <- 0
  } else {
    vnames <- names(which(!is_numeric))
    if (length(vnames) == 0) stop("There are no non-numerical variables")
    all_profiles$`_x_` <- ""
  }
  # select only suitable variables
  all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
  # create _x_
  tmp <- as.character(all_profiles$`_vname_`)
  for (i in seq_along(tmp)) {
    all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
  }

  # prepare plot
  `_x_` <- `_y_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
  pl <- ggplot(all_profiles, aes(`_x_`, `_yhat_`, group = paste(`_ids_`, `_label_`))) +
      facet_wrap(~ `_vname_`, scales = "free_x", ncol = facet_ncol)

  # show profiles without aggregation
  if (show_profiles & is.null(aggregate_profiles)) {
    if (is_color_a_variable) {
      pl <- pl + geom_line(data = all_profiles, aes_string(color = paste0("`",color,"`")), size = size, alpha = alpha)
    } else {
      pl <- pl + geom_line(data = all_profiles, size = size, alpha = alpha, color = color)
    }
  }

  # prepare data for plotting points
  if (show_observations | show_rugs | show_residuals) {
    is_color_points_a_variable    <- color_points %in% c(all_variables, "_label_", "_vname_", "_ids_")
    is_color_rugs_a_variable      <- color_rugs %in% c(all_variables, "_label_", "_vname_", "_ids_")
    is_color_residuals_a_variable <- color_residuals %in% c(all_variables, "_label_", "_vname_", "_ids_")

    tmp <- lapply(vnames, function(var) {
      data.frame(`_x_` = all_observations[,var],
                 `_vname_` = var,
                 `_yhat_`  = all_observations$`_yhat_`,
                 `_y_`     = if (is.null(all_observations$`_y_`)) NA else all_observations$`_y_`,
                 `_color_` = if (!is_color_points_a_variable) NA else {
                   if (color_points == "_vname_") var else all_observations[,color_points]
                 },
                 `_ids_`   = all_observations$`_ids_`,
                 `_label_`  = all_observations$`_label_`)
    })
    all_observations_long <- do.call(rbind, tmp)
    colnames(all_observations_long) <- c("_x_", "_vname_", "_yhat_", "_y_", "_color_", "_ids_", "_label_")
    if ((is_color_points_a_variable | is_color_rugs_a_variable) & !(color_points %in% colnames(all_observations_long))) colnames(all_observations_long)[5] = color_points

    # show observations
    if (show_observations) {
      if (is_color_points_a_variable) {
        pl <- pl + geom_point(data = all_observations_long, aes_string(color = paste0("`",color_points,"`")), size = size_points, alpha = alpha_points)
      } else {
        pl <- pl + geom_point(data = all_observations_long, size = size_points, alpha = alpha_points, color = color_points)
      }
    }

    # show rugs
    if (show_rugs) {
      if (is_color_rugs_a_variable) {
        pl <- pl + geom_rug(data = all_observations_long, aes_string(color = paste0("`",color_rugs,"`")), size = size_rugs, alpha = alpha_rugs)
      } else {
        pl <- pl + geom_rug(data = all_observations_long, size = size_rugs, alpha = alpha_rugs, color = color_rugs)
      }
    }

    if (show_residuals) {
      if (is_color_residuals_a_variable) {
        pl <- pl + geom_linerange(data = all_observations_long, aes_string(ymin = "`_y_`", ymax = "`_yhat_`", color = paste0("`",color_residuals,"`")), size = size_residuals, alpha = alpha_residuals) +
          geom_point(data = all_observations_long, aes_string(y = "`_y_`", color = paste0("`",color_residuals,"`")), size = size_residuals, alpha = alpha_residuals)
      } else {
        pl <- pl + geom_linerange(data = all_observations_long, aes_string(ymin = "`_y_`", ymax = "`_yhat_`"), size = size, alpha = alpha_residuals, color = color_residuals) +
          geom_point(data = all_observations_long, aes(y = `_y_`), size = size_residuals, alpha = alpha_residuals, color = color_residuals)
      }
    }
  }

  # show profiles with aggregation
  if (show_profiles & !is.null(aggregate_profiles)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_")]
    aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`), FUN = aggregate_profiles)
    colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_yhat_")
    aggregated_profiles$`_ids_` <- 0

    if (is_color_a_variable) {
      pl <- pl + geom_line(data = aggregated_profiles, aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha)
    } else {
      pl <- pl + geom_line(data = aggregated_profiles, aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
    }
  }

  pl <- pl + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.y = element_line(color = "white"),
          axis.ticks.y = element_line(color = "white"),
          axis.text = element_text(size = 10)) + xlab("") + ylab("")

  pl
}

