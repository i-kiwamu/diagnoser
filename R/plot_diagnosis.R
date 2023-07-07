#' Conversion from P-value to expression
#' @param p P-value
#' @return a vector of character of expression.
#' @examples
#' p <- c(1e-05, 0.002, 0.03, 0.4)
#' p2lt(p)
#' @export
p2lt <- function(p)
  ifelse(!is.finite(p), NA,
         ifelse(p < 0.001, as.character(expression(italic(P) < 0.001)),
                ifelse(p < 0.01, as.character(expression(italic(P) < 0.01)),
                       ifelse(p < 0.05, as.character(expression(italic(P) < 0.05)),
                              as.character(expression(italic(P) == round(p, 2)))))))


#' Conversion from P-value to stars
#' @param p P-value
#' @return a vector of character of asterisks or ns
#' @examples
#' p <- c(1e-05, 0.002, 0.03, 0.4)
#' p2star(p)
#' @export
p2star <- function(p)
  ifelse(!is.finite(p), NA,
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", "ns"))))


#' update_aes
update_aes <- function(mapping_orig, mapping_new) {
  result <- mapping_orig
  names_new <- names(mapping_new)
  for (i in seq_along(mapping_new)) {
    nn <- names_new[i]
    result[[nn]] <- mapping_new[[i]]
  }
  return(result)
}


#' autoplot.tbl_df_diag
#' @importFrom ggplot2 autoplot
#' @importFrom cli cli_alert_danger
#' @importFrom glue glue
#' @export
autoplot.tbl_df_diag <-
  function(object, type = "mf", formula = NULL, mapping = NULL, ...) {
  if(is.element(type, c("mf", "measured-fitted"))) {
    plot_measured_fitted(object, mapping = mapping, ...)
  } else if(is.element(type, c("rf", "resid-fitted"))) {
    plot_resid_fitted(object, formula = formula, mapping = mapping, ...)
  } else if(is.element(type, c("qq"))) {
    plot_qq(object, mapping = mapping, ...)
  } else if(is.element(type, c("sl", "scale-location"))) {
    plot_scale_location(object, mapping = mapping, ...)
  } else if(is.element(type, c("cook", "cooks-distance"))) {
    plot_cooksd(object, mapping = mapping, ...)
  } else if(is.element(type, c("mm", "marginal-model"))) {
    plot_marginal_model(object, formula = formula, mapping = mapping, ...)
  } else if(is.element(type, c("av", "added-variable"))) {
    plot_added_variable(object, formula = formula, mapping = mapping, ...)
  } else if(is.element(type, c("cr", "component-plus-residual"))) {
    plot_component_residual(object, formula = formula, mapping = mapping, ...)
  } else {
    cli_alert_danger(glue("The type of {type} is not available!"))
  }
}


#' plot.tbl_df_diag
#' @importFrom graphics plot
#' @export
plot.tbl_df_diag <-
  function(x, type = "mf", formula = NULL, mapping = NULL, ...) {
  print(autoplot.tbl_df_diag(
    x, type = type, formula = formula, mapping = mapping, ...
  ))
}


#' plot_measured_fitted
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom ggrepel geom_text_repel
plot_measured_fitted <- function(object, mapping, ...) {
  model <- attr(object, "model")
  model_class <- class(model)
  if (is.element(model_class, c("aov", "lm", "lme"))) {
    resp <- attr(object, "model")$terms[[2]]
  } else if (model_class == "lmerMod") {
    resp <- attr(slot(model, "frame"), "terms")[[2]]
  }
  mapping_new <- update_aes(aes(.fitted, !! resp, label = label), mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_text_repel()
}


#' plot_resid_fitted
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth 
#' @importFrom ggrepel geom_text_repel
plot_resid_fitted <- function(object, formula, mapping, ...) {
  if (is.null(formula)) {
    formula <- ~ .fitted
  }
  mapping_new <- update_aes(aes(!! formula[[2]], .std.resid, label = label),
                            mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                colour = "red", se = FALSE,
                linewidth = 0.3) +
    geom_text_repel()
}


#' plot_qq
#' @importFrom ggplot2 ggplot aes geom_qq geom_qq_line
#' @importFrom cli cli_abort
plot_qq <- function(object, mapping, level = 1, ...) {
  if (level == 1) {
    mapping_new <- update_aes(aes(sample = .resid), mapping)
    ggplot(object, mapping_new) +
      geom_qq(shape = 1) +
      geom_qq_line(linewidth = 0.3)
  } else {
    xg <- attr(object, "levels")[[level]]
    if (!is.element("sample", names(mapping))) {
      cli_abort("sample is required in aes()!")
    }
    ggplot(xg, mapping) +
      geom_qq(shape = 1) +
      geom_qq_line(linewidth = 0.3)
  }
}


#' plot_scale_location
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom ggrepel geom_text_repel
plot_scale_location <- function(object, mapping, ...) {
  mapping_new <- update_aes(aes(.fitted, .std.resid_abs_sqrt, label = label),
                            mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                colour = "red", se = FALSE,
                linewidth = 0.3) +
    geom_text_repel()
}


#' plot_cooksd
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_segment
#' @importFrom tibble tibble rowid_to_column
#' @importFrom ggrepel geom_text_repel
plot_cooksd <- function(object, mapping, ...) {
  mapping_new <- update_aes(aes(id, cooksd, label = label), mapping)
  object |>
    rowid_to_column(var = "id") |>
    ggplot(mapping_new) +
    geom_point(shape = 1) +
    geom_segment(aes(xend = id, yend = 0), linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_text_repel()
}


#' plot_marginal_model
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_colour_manual scale_linetype_manual
#' @importFrom cli cli_alert_danger
plot_marginal_model <- function(object, formula, mapping, ...) {
  if (is.null(formula)) {
    cli_alert_danger("Argument formula is required!")
  }
  if (length(formula[[2]]) != 1) {
    cli_alert_danger("Argument formula requires one variable!")
  }
  model <- attr(object, "model")
  model_class <- class(model)
  if (is.element(model_class, c("aov", "lm", "lme"))) {
    resp <- attr(object, "model")$terms[[2]]
  } else if (model_class == "lmerMod") {
    resp <- attr(slot(model, "frame"), "terms")[[2]]
  }
  explanatory <- formula[[2]]
  
  mapping_new <- update_aes(aes(!! explanatory, !! resp), mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                aes(colour = "Data", linetype = "Data"), 
                linewidth = 0.3, se = FALSE) +
    geom_smooth(method = "loess", formula = y ~ x,
                aes(y = predict(model, newdata = object),
                    colour = "Model", linetype = "Model"),
                linewidth = 0.3, se = FALSE) +
    scale_colour_manual(name = "", breaks = c("Data", "Model"),
                        values = c("Data" = "blue", "Model" = "red")) +
    scale_linetype_manual(name = "", breaks = c("Data", "Model"),
                          values = c("Data" = "solid", "Model" = "dashed"))
}


#' plot_added_variable
#' @importFrom ggplot2 ggplot aes geom_point geom_abline
#' @importFrom tibble tibble
#' @importFrom stats update
#' @importFrom cli cli_alert_danger
plot_added_variable <- function(object, formula, mapping, ...) {
  if (is.null(formula)) {
    cli_alert_danger("Argument formula is required!")
  }
  if (length(formula[[2]]) != 1) {
    cli_alert_danger("Argument formula requires one variable!")
  }
  
  model <- attr(object, "model")
  model_class <- class(model)
  if (!is.element(model_class, c("aov", "lm"))) {
    cli_alert_danger("Added-variable plots are available only for aov or lm!")
  }
  explanatory <- formula[[2]]
  which_coef <- c("(Intercept)", as.character(explanatory))
  
  resp <- model$terms[[2]]
  coef_sub <- coef(model)[which_coef]

  model1 <- update(model, substitute(~ . - a, list(a = explanatory)))
  model2 <- update(model1, substitute(a ~ ., list(a = explanatory)))
  
  mapping_new <- update_aes(aes(x = x, y = y), mapping)
  df_plot <- tibble(x = residuals(model2), y = residuals(model1))
  ggplot(df_plot, mapping_new) +
    geom_point(shape = 1) +
    geom_abline(intercept = coef_sub[1L], slope = coef_sub[2L],
                linewidth = 0.3) +
    labs(x = paste(explanatory, "| others"),
         y = paste(resp, "| others"))
}


#' plot_component_residual
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_smooth scale_colour_manual scale_linetype_manual
#' @importFrom cli cli_alert_danger
plot_component_residual <- function(object, formula, mapping, ...) {
  if (is.null(formula)) {
    cli_alert_danger("Argument formula is required!")
  }
  if (length(formula[[2]]) != 1) {
    cli_alert_danger("Argument formula requires one variable!")
  }
  model <- attr(object, "model")
  model_class <- class(model)
  if (!is.element(model_class, c("aov", "lm"))) {
    cli_alert_danger("Added-variable plots are available only for aov or lm!")
  }
  explanatory <- formula[[2]]
  coef_sub <- coef(model)[as.character(explanatory)]
  
  df_plot <- tibble()
  mapping_new <- 
    update_aes(aes(!! explanatory, .resid + coef_sub * !! explanatory),
               mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                aes(colour = "Data", linetype = "Data"),
                se = FALSE, linewidth = 0.3) +
    geom_abline(intercept = 0, slope = coef_sub,
                # aes(colour = "Model", linetype = "Model"),
                linewidth = 0.3,) +
    scale_colour_manual(name = "", breaks = c("Data", "Model"),
                        values = c("Data" = "blue", "Model" = "red")) +
    scale_linetype_manual(name = "", breaks = c("Data", "Model"),
                          values = c("Data" = "solid", "Model" = "dashed"))
}