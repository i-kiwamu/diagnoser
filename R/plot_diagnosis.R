update_aes <- function(mapping_orig, mapping_new) {
  result <- mapping_orig
  names_new <- names(mapping_new)
  for (i in seq_along(mapping_new)) {
    nn <- names_new[i]
    result[[nn]] <- mapping_new[[i]]
  }
  return(result)
}


ignore_x_aes <- function(mapping) {
  cli::cli_alert_info("`x` is specified in aes(), but ignored...")
  mapping[names(mapping) != "x"]
}


ignore_y_aes <- function(mapping) {
  cli::cli_alert_info("`y` is specified in aes(), but ignored...")
  mapping[names(mapping) != "y"]
}


#' autoplot.tbl_df_diag
#' @importFrom ggplot2 autoplot
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @param object An object of \code{tbl_df_diag}.
#' @param type A character either \code{mf} (default), \code{rf}, \code{qq}, \code{sl}, \code{cook}, \code{mm}, \code{av}, or \code{cr}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param ... currently not to be used.
#' @export
#' @rdname plot.tbl_df_diag
autoplot.tbl_df_diag <-
  function(object, type = "mf", mapping = aes(), ...) {
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
#' @description A generic function to plot an object of \code{tbl_df_diag} in various types.
#' @details The argument \code{type} can be specified as below:
#' * \code{mf} (\code{measured-fitted}): Measured vs fitted values (default)
#' * \code{rf} (\code{resid-fitted}): Studentized residual vs fitted values
#' * \code{qq}: QQ norm plot
#' * \code{sl} (\code{scale-location}): Scale-Location plot
#' * \code{cook} (\code{cooks-distance}): Cook's distance
#' * \code{mm} (\code{marginal-model}): Marginal-model plot
#' * \code{av} (\code{added-variable}): Added-variable plot (only for \code{lm} and \code{aov} object)
#' * \code{cr} (\code{component-plus-residual}): Component-plus residual plot (only for \code{lm} and \code{aov} object)
#' @importFrom graphics plot
#' @param x An object of \code{tbl_df_diag}.
#' @param type A string. See details.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param ... currently not to be used.
#' @export
#' @examples
#' library(ggplot2)
#' library(nlme)
#' lm_od <- lm(distance ~ age + Sex, Orthodont)
#' lme_od <- lme(distance ~ age + Sex, Orthodont)
#' diag_lm_od <- diagnose(lm_od)
#' diag_lme_od <- diagnose(lme_od)
#'
#' # measured-fitted plot
#' plot(diag_lme_od)
#' 
#' # residual-fitted plot
#' plot(diag_lme_od, type = "rf")
#' 
#' # QQ plot
#' plot(diag_lme_od, type = "qq")
#' 
#' # scale-location plot
#' plot(diag_lme_od, type = "sl")
#' 
#' # Cook's distance
#' plot(diag_lme_od, type = "cook")
#' 
#' # marginal-model plot
#' plot(diag_lme_od, type = "mm", mapping = aes(x = age))
#' 
#' # added-variable plot (only for lm and aov)
#' plot(diag_lm_od, type = "av", mapping = aes(x = age))
#' 
#' # component-plus-residual plot (only for lm and aov)
#' plot(diag_lm_od, type = "cr", mapping = aes(x = age))
plot.tbl_df_diag <-
  function(x, type = "mf", mapping = aes(), ...) {
  print(autoplot.tbl_df_diag(x, type = type, mapping = mapping, ...))
}


#' plot_measured_fitted
#' @description Plot of measured vs fitted values
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom ggrepel geom_text_repel
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param ... currently not to be used.
#' @note To generate a scatter plot of measured vs fitted values.
plot_measured_fitted <- function(object, mapping, ...) {
  if (is.element("x", names(mapping)))
    mapping <- ignore_x_aes(mapping)
  
  if (is.element("y", names(mapping)))
    mapping <- ignore_y_aes(mapping)

  model <- attr(object, "model")
  model_class <- class(model)
  if (is.element(model_class, c("aov", "lm", "lme"))) {
    resp <- attr(object, "model")$terms[[2]]
  } else if (model_class == "lmerMod") {
    resp <- attr(slot(model, "frame"), "terms")[[2]]
  }
  mapping_new <- update_aes(aes(.data$.fitted, {{resp}}),
                            mapping)
  object_tbl <- as_tibble(object)
  ggplot(object_tbl, mapping_new) +
    geom_point(shape = 1) +
    geom_text_repel(data = object_tbl %>% filter(.data$label != ""),
                    aes(label = .data$label))
}


#' plot_resid_fitted
#' @description Plot of studentized residuals vs fitted values.
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_boxplot
#' @importFrom ggrepel geom_text_repel
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param ... currently not to be used.
#' @note To generate a scatter plot of fitted values vs studentized residuals.
plot_resid_fitted <- function(object, mapping, ...) {
  if (!is.element("x", names(mapping))) {
    mapping <- update_aes(mapping, aes(x = .data$.fitted))
    x_type <- "numeric"
  } else {
    x_label <- as_label(mapping$x)
    data_classes <- attr(attr(object, "terms"), "dataClasses")
    if (!is.element(x_label, names(data_classes))) {
      cli_abort(glue("{x_label} is not found in the model!"))
    }
    x_type <- data_classes[x_label]
  }
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }

  mapping_new <-
    update_aes(mapping, aes(y = .data$.std.resid))
  object_tbl <- as_tibble(object)
  if (x_type == "numeric") {
    ggplot(object_tbl, mapping_new) +
      geom_point(shape = 1) +
      geom_smooth(method = "loess", formula = y ~ x,
                  colour = "red", se = FALSE,
                  linewidth = 0.3) +
      geom_text_repel(data = object_tbl %>% filter(.data$label != ""),
                      aes(label = .data$label))
  } else {
    ggplot(object_tbl, mapping_new) +
      geom_boxplot(linewidth = 0.3)
  }
}


#' plot_qq
#' @description QQ plot
#' @importFrom ggplot2 ggplot aes geom_qq geom_qq_line
#' @importFrom cli cli_abort
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param level A character of level for mixed-effects models (default = 1). See \code{\link[HLMdiag]{hlm_influence}}.
#' @param ... currently not to be used.
#' @note To generate a QQ plot.
plot_qq <- function(object, mapping, level = 1, ...) {
  if (is.element("x", names(mapping))) {
    mapping <- ignore_x_aes(mapping)
  }
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }

  if (level == 1) {
    mapping_new <- update_aes(aes(sample = .data$.resid), mapping)
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
#' @description Scale-location plot
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom ggrepel geom_text_repel
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param ... currently not to be used.
#' @note To generate a Scale-Location plot.
plot_scale_location <- function(object, mapping, ...) {
  if (is.element("x", names(mapping))) {
    mapping <- ignore_x_aes(mapping)
  }
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }

  mapping_new <-
    update_aes(aes(.data$.fitted, .data$.std.resid_abs_sqrt),
               mapping)
  object_tbl <- as_tibble(object)
  ggplot(object_tbl, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                colour = "red", se = FALSE,
                linewidth = 0.3) +
    geom_text_repel(data = object_tbl %>% filter(.data$label != ""),
                    aes(label = .data$label))
}


#' plot_cooksd
#' @description Plot of Cook's distance
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_segment
#' @importFrom ggrepel geom_text_repel
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2}.
#' @param ... currently not to be used.
#' @note To visualize the Cook's distance.
plot_cooksd <- function(object, mapping, ...) {
  if (is.element("x", names(mapping))) {
    mapping <- ignore_x_aes(mapping)
  }
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }
  mapping_new <-
    update_aes(mapping,
               aes(.data$.rowid, .data$cooksd))
  object_tbl <- as_tibble(object)
  ggplot(object_tbl, mapping_new) +
    geom_point(shape = 1) +
    geom_segment(aes(xend = .data$.rowid, yend = 0), linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_text_repel(data = object_tbl %>% filter(.data$label != ""),
                    aes(label = .data$label))
}


#' plot_marginal_model
#' @description Marginal-model plot
#' @importFrom stats predict
#' @importFrom rlang as_label
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_colour_manual scale_linetype_manual
#' @importFrom glue glue
#' @importFrom cli cli_abort
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2} with a \code{x} argument.
#' @param ... currently not to be used.
#' @note To generate a marginal-model plot.
plot_marginal_model <- function(object, mapping, ...) {
  if (!is.element("x", names(mapping))) {
    cli_abort("Specify `x` argument of `aes` for `mapping` argument!")
  }
  x_label <- as_label(mapping$x)
  model <- attr(object, "model")
  data_classes <- get_data_classes(model)
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  if (x_type != "numeric") {
    cli_abort(glue("Type of {x_label} needs to be numeric!"))
  }
  
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }
  model_class <- class(model)
  if (is.element(model_class, c("aov", "lm", "lme"))) {
    resp <- model$terms[[2]]
  } else if (model_class == "lmerMod") {
    resp <- attr(slot(model, "frame"), "terms")[[2]]
  }
  
  mapping_new <- update_aes(mapping, aes(y = {{resp}}))
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
#' @description Added-variable plot
#' @importFrom stats residuals coef
#' @importFrom rlang as_label sym
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs
#' @importFrom tibble tibble
#' @importFrom stats update
#' @importFrom cli cli_abort
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2} with a \code{x} argument.
#' @param ... currently not to be used.
#' @note To generate an added-variable plot.
plot_added_variable <- function(object, mapping, ...) {
  if (!is.element("x", names(mapping))) {
    cli_abort("Specify `x` argument of `aes` for `mapping` argument!")
  }
  x_label <- as_label(mapping$x)
  model <- attr(object, "model")
  data_classes <- get_data_classes(model)
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  if (x_type != "numeric") {
    cli_abort(glue("Type of {x_label} needs to be numeric!"))
  }
  
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }
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
  
  mapping_new <- update_aes(mapping, aes(x = .data$x, y = .data$y))
  df_plot <- tibble(x = residuals(model2), y = residuals(model1))
  ggplot(df_plot, mapping_new) +
    geom_point(shape = 1) +
    geom_abline(intercept = coef_sub[1L], slope = coef_sub[2L],
                linewidth = 0.3) +
    labs(x = paste(x_label, "| others"),
         y = paste(resp, "| others"))
}


#' plot_component_residual
#' @description Component+residual plot
#' @importFrom stats coef
#' @importFrom rlang as_label sym
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_smooth scale_colour_manual scale_linetype_manual
#' @importFrom cli cli_abort
#' @param object An object of \code{tbl_df_diag}.
#' @param mapping An aesthetic mapping object \code{aes()} of \code{ggplot2} with a \code{x} argument.
#' @param ... currently not to be used.
#' @note To generate a component-plus-residual plot.
plot_component_residual <- function(object, mapping, ...) {
  if (!is.element("x", names(mapping))) {
    cli_abort("Specify `x` argument of `aes` for `mapping` argument!")
  }
  x_label <- as_label(mapping$x)
  model <- attr(object, "model")
  data_classes <- get_data_classes(model)
  if (!is.element(x_label, names(data_classes))) {
    cli_abort(glue("{x_label} is not found in the model!"))
  }
  x_type <- data_classes[x_label]
  if (x_type != "numeric") {
    cli_abort(glue("Type of {x_label} needs to be numeric!"))
  }
  
  if (is.element("y", names(mapping))) {
    mapping <- ignore_y_aes(mapping)
  }
  model_class <- class(model)
  if (!is.element(model_class, c("aov", "lm"))) {
    cli_abort("Added-variable plots are available only for aov or lm!")
  }
  x_label <- as_label(mapping$x)
  coef_sub <- coef(model)[x_label]
  x_sym <- sym(x_label)
  
  mapping_new <- 
    update_aes(mapping,
               aes(x = {{x_sym}},
                   y = .data$.resid + coef_sub * {{x_sym}}))
  ggplot(object, mapping_new) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", formula = y ~ x,
                aes(colour = "Data", linetype = "Data"),
                se = FALSE, linewidth = 0.3) +
    geom_abline(intercept = 0, slope = coef_sub,
                # aes(colour = "Model", linetype = "Model"),
                linewidth = 0.3) +
    scale_colour_manual(name = "", breaks = c("Data", "Model"),
                        values = c("Data" = "blue", "Model" = "red")) +
    scale_linetype_manual(name = "", breaks = c("Data", "Model"),
                          values = c("Data" = "solid", "Model" = "dashed")) +
    labs(y = "Component + Residual")
}