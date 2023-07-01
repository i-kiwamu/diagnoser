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


#' ignore_x_aes
#' @importFrom cli cli_alert_info
ignore_x_aes <- function(mapping) {
  cli_alert_info("`x` is specified in aes(), but ignored...")
  mapping[names(mapping) != "x"]
}


#' ignore_y_aes
#' @importFrom cli cli_alert_info
ignore_y_aes <- function(mapping) {
  cli_alert_info("`y` is specified in aes(), but ignored...")
  mapping[names(mapping) != "y"]
}


#' autoplot.tbl_df_diag
#' @importFrom ggplot2 autoplot
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @export
autoplot.tbl_df_diag <-
  function(object, type = "mf", mapping = NULL, ...) {
  if(is.element(type, c("mf", "measured-fitted"))) {
    plot_measured_fitted(object, mapping = mapping, ...)
  } else if(is.element(type, c("rf", "resid-fitted"))) {
    plot_resid_fitted(object, mapping = mapping, ...)
  } else if(is.element(type, c("qq"))) {
    plot_qq(object, mapping = mapping, ...)
  } else if(is.element(type, c("sl", "scale-location"))) {
    plot_scale_location(object, mapping = mapping, ...)
  } else if(is.element(type, c("cook", "cooks-distance"))) {
    plot_cooksd(object, mapping = mapping, ...)
  } else if(is.element(type, c("mm", "marginal-model"))) {
    plot_marginal_model(object, mapping = mapping, ...)
  } else if(is.element(type, c("av", "added-variable"))) {
    plot_added_variable(object, mapping = mapping, ...)
  } else if(is.element(type, c("cr", "component-plus-residual"))) {
    plot_component_residual(object, mapping = mapping, ...)
  } else {
    cli_abort(glue("The type of {type} is not available!"))
  }
}


#' plot.tbl_df_diag
#' @importFrom graphics plot
#' @export
plot.tbl_df_diag <-
  function(x, type = "mf", mapping = NULL, ...) {
  print(autoplot.tbl_df_diag(x, type = type, mapping = mapping, ...))
}


#' plot_measured_fitted
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom ggrepel geom_text_repel
plot_measured_fitted <- function(object, mapping, ...) {
  mapping <- ignore_x_aes(mapping)
  mapping <- ignore_y_aes(mapping)

  model <- attr(object, "model")
  model_class <- class(model)
  if (is.element(model_class, c("aov", "lm", "lme"))) {
    resp <- attr(object, "model")$terms[[2]]
  } else if (model_class == "lmerMod") {
    resp <- attr(slot(model, "frame"), "terms")[[2]]
  }
  mapping_new <- update_aes(aes(.fitted, !! resp), mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_text_repel(aes(label = label))
}


#' plot_resid_fitted
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_boxplot
#' @importFrom ggrepel geom_text_repel
plot_resid_fitted <- function(object, mapping, ...) {
  if (!is.element("x", names(mapping))) {
    mapping <- update_aes(mapping, aes(x = .fitted))
  }
  mapping <- ignore_y_aes(mapping)
  x_label <- as_label(mapping$x)
  data_classes <- attr(attr(object, "terms"), "dataClasses")
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  
  mapping_new <- update_aes(mapping, aes(y = .std.resid))
  
  if (x_type == "numeric") {
    ggplot(object, mapping_new) +
      geom_point(shape = 1) +
      geom_smooth(method = "loess", formula = y ~ x,
                  colour = "red", se = FALSE,
                  linewidth = 0.3) +
      geom_text_repel(aes(label = label))
  } else {
    ggplot(object, mapping_new) +
      geom_boxplot(linewidth = 0.3)
  }
}


#' plot_qq
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line
plot_qq <- function(object, mapping, ...) {
  mapping <- ignore_x_aes(mapping)
  mapping <- ignore_y_aes(mapping)
  mapping_new <- update_aes(aes(sample = .resid), mapping)
  ggplot(object, mapping_new) +
    geom_qq(shape = 1) +
    geom_qq_line(linewidth = 0.3)
}


#' plot_scale_location
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom ggrepel geom_text_repel
plot_scale_location <- function(object, mapping, ...) {
  mapping <- ignore_x_aes(mapping)
  mapping <- ignore_y_aes(mapping)
  mapping_new <- update_aes(aes(.fitted, .std.resid_abs_sqrt),
                            mapping)
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                colour = "red", se = FALSE,
                linewidth = 0.3) +
    geom_text_repel(aes(label = label))
}


#' plot_cooksd
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_segment
#' @importFrom tibble tibble rowid_to_column
#' @importFrom ggrepel geom_text_repel
plot_cooksd <- function(object, mapping, ...) {
  mapping <- ignore_x_aes(mapping)
  mapping <- ignore_y_aes(mapping)
  mapping_new <- update_aes(mapping, aes(id, cooksd))
  object |>
    rowid_to_column(var = "id") |>
    ggplot(mapping_new) +
    geom_point(shape = 1) +
    geom_segment(aes(xend = id, yend = 0), linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_text_repel(aes(label = label))
}


#' plot_marginal_model
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_colour_manual scale_linetype_manual
#' @importFrom glue glue
#' @importFrom rlang as_label
#' @importFrom cli cli_abort
plot_marginal_model <- function(object, mapping, ...) {
  mapping <- ignore_y_aes(mapping)
  if (!is.element("x", names(mapping))) {
    cli_abort("Specify `x` argument of `aes` for `mapping` argument!")
  }
  x_label <- as_label(mapping$x)
  data_classes <- attr(attr(object, "terms"), "dataClasses")
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  if (x_type != "numeric") {
    cli_abort(glue("Type of {x_label} needs to be numeric!"))
  }
  
  model <- attr(object, "model")
  model_class <- class(model)
  if (is.element(model_class, c("aov", "lm", "lme"))) {
    resp <- attr(object, "model")$terms[[2]]
  } else if (model_class == "lmerMod") {
    resp <- attr(slot(model, "frame"), "terms")[[2]]
  }
  
  mapping_new <- update_aes(mapping, aes(y = !! resp))
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
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs
#' @importFrom tibble tibble
#' @importFrom stats update
#' @importFrom rlang as_label sym
#' @importFrom cli cli_abort
plot_added_variable <- function(object, mapping, ...) {
  mapping <- ignore_y_aes(mapping)
  if (!is.element("x", names(mapping))) {
    cli_abort("Specify `x` argument of `aes` for `mapping` argument!")
  }
  x_label <- as_label(mapping$x)
  data_classes <- attr(attr(object, "terms"), "dataClasses")
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  if (x_type != "numeric") {
    cli_abort(glue("Type of {x_label} needs to be numeric!"))
  }
  
  model <- attr(object, "model")
  model_class <- class(model)
  if (!is.element(model_class, c("aov", "lm"))) {
    cli_abort("Added-variable plots are available only for aov or lm!")
  }
  which_coef <- c("(Intercept)", x_label)
  
  resp <- model$terms[[2]]
  coef_sub <- coef(model)[which_coef]

  x_sym <- sym(x_label)
  model1 <- update(model, substitute(~ . - a, list(a = x_sym)))
  model2 <- update(model1, substitute(a ~ ., list(a = x_sym)))
  
  mapping_new <- update_aes(mapping, aes(x = x, y = y))
  df_plot <- tibble(x = residuals(model2), y = residuals(model1))
  ggplot(df_plot, mapping_new) +
    geom_point(shape = 1) +
    geom_abline(intercept = coef_sub[1L], slope = coef_sub[2L],
                linewidth = 0.3) +
    labs(x = paste(x_label, "| others"),
         y = paste(resp, "| others"))
}


#' plot_component_residual
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_smooth scale_colour_manual scale_linetype_manual
#' @importFrom rlang as_label sym
#' @importFrom cli cli_abort
plot_component_residual <- function(object, mapping, ...) {
  mapping <- ignore_y_aes(mapping)
  if (!is.element("x", names(mapping))) {
    cli_abort("Specify `x` argument of `aes` for `mapping` argument!")
  }
  x_label <- as_label(mapping$x)
  data_classes <- attr(attr(object, "terms"), "dataClasses")
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  if (x_type != "numeric") {
    cli_abort(glue("Type of {x_label} needs to be numeric!"))
  }
  
  model <- attr(object, "model")
  model_class <- class(model)
  if (!is.element(model_class, c("aov", "lm"))) {
    cli_abort("Added-variable plots are available only for aov or lm!")
  }
  x_label <- as_label(mapping$x)
  coef_sub <- coef(model)[x_label]
  x_sym <- sym(x_label)
  
  mapping_new <- 
    update_aes(mapping,
               aes(x = !! x_sym,
                   y = .resid + coef_sub * !! x_sym))
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