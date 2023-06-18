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


#' GeomPointLoess
#' @importFrom ggplot2 ggproto Geom GeomPoint GeomSmooth
#' @importFrom grid gList
#' @export
GeomPointLoess <- 
  ggproto("GeomPointLoess", Geom,
          required_aes = c("x", "y"),
          default_aes = aes(colour = "black", size = 2, shape = 19,
                            linetype = 1, linewidth = 0.5,
                            fill = NA, alpha = NA, stroke = 1),
          draw_panel = function(data, panel_params, coord, ...) {
            gList(GeomPoint$draw_panel(data, panel_params_point, coord, ...),
                  GeomSmooth$draw_panel(data, panel_params_smooth, coord, ...))
          })

#' geom_diag_resid_fitted
#' @importFrom ggplot2 layer aes GeomPoint
#' @importFrom rlang list2
#' @importFrom cli cli_alert_warning
#' @inheritParams ggplot2::geom_point
#' @export
geom_diag_resid_fitted <-
  function(mapping = NULL, data = NULL,
           stat = "identity", position = "identity",
           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    if (!is.null(mapping)) {
      cli_alert_warning("aes() is specified, but ignored...")
    }
    layer(data = data, mapping = aes(x = .fitted, y = .resid),
          geom = GeomPointLoess, stat = stat, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list2(na.rm = na.rm, ...))
}




# plot_diagnosis ==============================================================

#' Generic function of plot_diagnosis
#' @param model model object
#' @param ... additional arguments
#' @note This is a generic function to make the diagnosis plot of the model.
#' @export
#' @seealso \code{\link{plot}}
plot_diagnosis <- function(model, ...) UseMethod("plot_diagnosis")


#' Diagnosis plot fo lm object
#' @importFrom stats cooks.distance fitted resid
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point labs stat_qq_line stat_qq geom_hline
#' @importFrom cowplot plot_grid
#' @param model lm object
#' @param ... Not to be used.
#' @note It produces four plots: Residual vs Fitted, Normal QQ, Scale-Location, and Cook's distance.
#' @examples
#' fit <- lm(weight ~ height, women)
#' plot_diagnosis(fit)
#' @export
plot_diagnosis.lm <- function(model, ...) {
  df <- tibble(
    fitted = fitted(model),
    resid = resid(model, type = "pearson"),
    resid_std = sqrt(abs(resid(model, type = "pearson"))),
    cooks = cooks.distance(model))
  p1 <- ggplot(df, aes(fitted, resid)) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_point() +
    labs(title = "Residual vs Fitted",
         x = "Fitted values", y = "Residuals")
  p2 <- ggplot(df, aes(sample = resid)) +
    stat_qq_line(colour = "gray", linetype = "dashed") +
    stat_qq() +
    labs(title = "Normal QQ",
         x = "Theoretical quantities", y = "Standardized residuals")
  p3 <- ggplot(df, aes(fitted, resid_std)) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_point() +
    labs(title = "Scale-Location",
         x = "Fitted values", y = expression(sqrt("|Standardized residuals|")))
  p4 <- ggplot(df, aes(1:nrow(df), cooks)) +
    geom_hline(yintercept = 0.5, colour = "blue", linetype = "dashed") +
    geom_point() +
    labs(title = "Cook's distance",
         x = "Index", y = "Cook's distance")
  plot_grid(
    p1, p2, p3, p4,
    align = "hv", nrow = 2, ncol = 2)
}


#' Diagnosis plot fo lme object
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point labs stat_qq_line stat_qq geom_hline
#' @importFrom cowplot plot_grid
#' @param model lme object
#' @param ... Not to be used.
#' @note four plots of Residual vs Fitted, Normal QQ, Scale-Location, and Cook's distance
#' @keywords function
#' @author ISHIKURA Kiwamu
#' @examples
#' library(nlme)
#' fit <- lme(circumference ~ age, random = ~ 1|Tree, Orange)
#' plot_diagnosis(fit)
#' @export
plot_diagnosis.lme <- function(model, ...) {
  df <- hlm_augment(model) |>
    mutate(.std.resid = hlm_resid(model, standardize = TRUE)$.std.resid)
  p1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_point() +
    labs(title = "Residual vs Fitted",
         x = "Fitted values", y = "Residuals")
  p2 <- ggplot(df, aes(sample = .std.resid)) +
    stat_qq_line(colour = "gray", linetype = "dashed") +
    stat_qq() +
    labs(title = "Normal QQ",
         x = "Theoretical quantities", y = "Standardized residuals")
  p3 <- ggplot(df, aes(.fitted, .std.resid)) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_point() +
    labs(title = "Scale-Location",
         x = "Fitted values", y = expression(sqrt("|Standardized residuals|")))
  p4 <- ggplot(df, aes(1:nrow(df), cooksd)) +
    geom_hline(yintercept = 0.5, colour = "blue", linetype = "dashed") +
    geom_point() +
    labs(title = "Cook's distance",
         x = "Index", y = "Cook's distance")
  plot_grid(
    p1, p2, p3, p4,
    align = "hv", nrow = 2, ncol = 2)
}



#' Diagnosis plot fo lmerMod object
#' @importFrom HLMdiag hlm_augment
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point labs stat_qq_line stat_qq geom_hline
#' @importFrom cowplot plot_grid
#' @param model lmerMod object
#' @param ... Not to be used.
#' @note It produces four plots: Residual vs Fitted, Normal QQ, Scale-Location, and Cook's distance.
#' @examples
#' library(lme4)
#' fit <- lmer(circumference ~ age + (1|Tree), Orange)
#' plot_diagnosis(fit)
#' @export
plot_diagnosis.lmerMod <- function(model, ...) {
  df <- hlm_augment(model) |>
    mutate(.std.resid = hlm_resid(model, standardize = TRUE)$.std.resid)
  p1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_point() +
    labs(title = "Residual vs Fitted",
         x = "Fitted values", y = "Residuals")
  p2 <- ggplot(df, aes(sample = .std.resid)) +
    stat_qq_line(colour = "gray", linetype = "dashed") +
    stat_qq() +
    labs(title = "Normal QQ",
         x = "Theoretical quantities", y = "Standardized residuals")
  p3 <- ggplot(df, aes(.fitted, .std.resid)) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_point() +
    labs(title = "Scale-Location",
         x = "Fitted values", y = expression(sqrt("|Standardized residuals|")))
  p4 <- ggplot(df, aes(1:nrow(df), cooksd)) +
    geom_hline(yintercept = 0.5, colour = "blue", linetype = "dashed") +
    geom_point() +
    labs(title = "Cook's distance",
         x = "Index", y = "Cook's distance")
  plot_grid(
    p1, p2, p3, p4,
    align = "hv", nrow = 2, ncol = 2)
}
