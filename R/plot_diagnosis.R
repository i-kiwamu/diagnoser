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


#' autoplot.tbl_df_diag
#' @importFrom ggplot2 autoplot
#' @importFrom cli cli_alert_danger
#' @importFrom glue glue
#' @export
autoplot.tbl_df_diag <- function(object, type = "rf", formula = NULL, ...) {
  if(is.element(type, c("mf", "measured-fitted"))) {
    plot_measured_fitted(object, ...)
  } else if(is.element(type, c("rf", "resid-fitted"))) {
    plot_resid_fitted(object, ...)
  } else if(is.element(type, c("qq"))) {
    plot_qq(object, ...)
  } else if(is.element(type, c("sl", "scale-location"))) {
    plot_scale_location(object, ...)
  } else if(is.element(type, c("cook", "cooks-distance"))) {
    plot_cooksd(object, ...)
  } else if(is.element(type, c("mm", "marginal-model"))) {
    plot_marginal_model(object, formula = formula, ...)
  } else if(is.element(type, c("av", "added-variable"))) {
    plot_added_variable(object, formula = formula, ...)
  } else {
    cli_alert_danger(glue("The type of {type} is not available!"))
  }
}


#' plot.tbl_df_diag
#' @importFrom graphics plot
#' @export
plot.tbl_df_diag <- function(x, type = "rf", ...) {
  print(autoplot.tbl_df_diag(x, type = type, ...))
}


#' plot_measured_fitted
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom ggrepel geom_text_repel
plot_measured_fitted <- function(object, ...) {
  df_repel <- object |> filter(cooksd > 0.05)
  p <- object |>
    ggplot(aes(.fitted, !! attr(object, "model")$term[[2]])) +
    geom_point(shape = 1)
  if (nrow(df_repel) > 1) {
    p <- p +
      geom_text_repel(aes(label = .rownames))
  }
  return(p)
}


#' plot_resid_fitted
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth 
#' @importFrom ggrepel geom_text_repel
plot_resid_fitted <- function(object, ...) {
  df_repel <- object |> filter(cooksd > 0.05)
  p <- object |>
    ggplot(aes(.fitted, .std.resid)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", colour = "red", se = FALSE,
                linewidth = 0.3)
  if (nrow(df_repel) > 1) {
    p <- p +
      geom_text_repel(aes(label = .rownames))
  }
  return(p)
}


#' plot_qq
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line
#' @importFrom ggrepel geom_text_repel
plot_qq <- function(object, ...) {
  df_repel <- object |> filter(cooksd > 0.05)
  p <- object |>
    ggplot(aes(sample = .resid)) +
    geom_qq(shape = 1) +
    geom_qq_line(linewidth = 0.3)
  if (nrow(df_repel) > 1) {
    p <- p +
      geom_text_repel(aes(label = .rownames))
  }
  return(p)
}


#' plot_scale_location
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom ggrepel geom_text_repel
plot_scale_location <- function(object, ...) {
  df_repel <- object |> filter(cooksd > 0.05)
  p <- object |>
    ggplot(aes(.fitted, .std.resid_abs_sqrt)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", colour = "red", se = FALSE,
                linewidth = 0.3)
  if (nrow(df_repel) > 1) {
    p <- p +
      geom_text_repel(aes(label = .rownames))
  }
  return(p)
}


#' plot_cooksd
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_segment
#' @importFrom tibble tibble
#' @importFrom ggrepel geom_text_repel
plot_cooksd <- function(object, ...) {
  df_repel <- object |> filter(cooksd > 0.05)
  p <- object |> 
    ggplot(aes(seq(1, nrow(object)), cooksd)) +
    geom_point(shape = 1) +
    geom_segment(aes(xend = id, yend = 0), linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3)
  if (nrow(df_repel) > 1) {
    p <- p +
      geom_text_repel(aes(label = .rownames))
  }
  return(p)
}


#' plot_marginal_model
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom cli cli_alert_danger
plot_marginal_model <- function(object, formula, ...) {
  if (is.null(formula)) {
    cli_alert_danger("Argument formula is required!")
  }
  if (length(formula[[2]]) != 1) {
    cli_alert_danger("Argument formula requires one variable!")
  }
  model <- attr(object, "model")
  resp <- model$terms[[2]]
  explanatory <- formula[[2]]
  ggplot(object, aes(!! explanatory, !! resp)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess",
                aes(colour = "Data", linetype = "Data"), 
                linewidth = 0.3, se = FALSE) +
    geom_smooth(method = "loess",
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
#' @importFrom cli cli_alert_danger
plot_added_variable <- function(object, formula, ...) {
  if (is.null(formula)) {
    cli_alert_danger("Argument formula is required!")
  }
  if (length(formula[[2]]) != 1) {
    cli_alert_danger("Argument formula requires one variable!")
  }
  model <- attr(object, "model")
  resp <- model$terms[[2]]
  explanatory <- formula[[2]]
  coef_sub <- coef(model)[c("(Intercept)", as.character(explanatory))]
  model1 <- update(model, substitute(~ . - a, list(a = explanatory)))
  model2 <- update(model1, substitute(a ~ ., list(a = explanatory)))
  df_plot <- tibble(x = residuals(model2), y = residuals(model1))
  ggplot(df_plot, aes(x = x, y = y)) +
    geom_point(shape = 1) +
    geom_abline(intercept = coef_sub[1L], slope = coef_sub[2L],
                linewidth = 0.3) +
    labs(x = paste(explanatory, "| others"),
         y = paste(resp, "| others"))
}



#' get_rank
get_rank <- function(model) UseMethod("get_rank", model)


#' get_rank.default
#' @importFrom glue glue
get_rank.default <- function(model) {
  stop(glue("There is no method for the class of {class(model)}!"))
}


#' get_rank.lm
get_rank.lm <- function(model) {
  return(model$rank)
}


#' get_rank.lme
#' @importFrom HLMdiag extract_design
get_rank.lme <- function(model) {
  return(ncol(extract_design(model)$X))
}


#' get_rank.lmerMod
get_rank.lmerMod <- function(model) {
  return(ncol(slot(model, "pp")$X))
}
