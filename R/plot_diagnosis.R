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
autoplot.tbl_df_diag <- function(object, type = "rf", ...) {
  if(is.element(type, c("rf", "resid-fitted"))) {
    plot_resid_fitted(object, ...)
  } else if(is.element(type, c("qq"))) {
    plot_qq(object, ...)
  } else if(is.element(type, c("sl", "scale-location"))) {
    plot_scale_location(object, ...)
  } else if(is.element(type, c("infl", "influential"))) {
    plot_influential(object, ...)
  } else {
    cli_alert_danger(glue("The type of {type} is not available!"))
  }
}


#' plot.tbl_df_diag
#' @importFrom graphics plot
#' @export
plot.tbl_df_diag <- function(x, type = "rf", ...) {
  print(autoplot(x, type = type, ...))
}


#' plot_resid_fitted
#' @importFrom ggplot2 ggplot geom_point geom_smooth 
plot_resid_fitted <- function(object, ...) {
  ggplot(object, aes(.fitted, .resid)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", colour = "red", se = FALSE,
                linewidth = 0.3)
}


#' plot_qq
#' @importFrom ggplot2 ggplot stat_qq stat_qq_line
plot_qq <- function(object, ...) {
  ggplot(object, aes(sample = .resid)) +
    geom_qq(shape = 1) +
    geom_qq_line(linewidth = 0.3)
}


#' plot_scale_location
#' @importFrom ggplot2 ggplot geom_point geom_smooth
plot_scale_location <- function(object, ...) {
  ggplot(object, aes(.fitted, .std.resid_abs_sqrt)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess", colour = "red", se = FALSE,
                linewidth = 0.3)
}


#' plot_influential
#' @importFrom ggplot2 ggplot geom_point geom_line
#' @importFrom tibble tibble
plot_influential <- function(object, ...) {
  model <- attr(object, "model")
  p <- get_rank(model)
  df_cook <-
    tibble(x = seq(0, max(object$leverage.overall), length.out = 100)) |>
    mutate(cook05 = 0.5 * p * (1 - x) / x,
           cook10 = 1.0 * p * (1 - x) / x,
           cook05_upr = sqrt(cook05),
           cook05_lwr = -cook05_upr,
           cook10_upr = sqrt(cook10),
           cook10_lwr = -cook10_upr)
  ggplot(object, aes(leverage.overall, .std.resid)) +
    geom_point(aes(size = cooksd), shape = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3,
               colour = "gray") +
    geom_line(data = df_cook, aes(x = x, y = cook05_upr, group = 1),
              linetype = "dashed", linewidth = 0.3, colour = "black") +
    geom_line(data = df_cook, aes(x = x, y = cook05_lwr, group = 1),
              linetype = "dashed", linewidth = 0.3, colour = "black") +
    geom_line(data = df_cook, aes(x = x, y = cook10_upr, group = 1),
              linetype = "solid", linewidth = 0.3, colour = "black") +
    geom_line(data = df_cook, aes(x = x, y = cook10_lwr, group = 1),
              linetype = "solid", linewidth = 0.3, colour = "black") +
    coord_cartesian(xlim = range(object$leverage.overall),
                    ylim = range(object$.std.resid))
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
