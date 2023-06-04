#' Conversion from P-value to expression
#' @param p P-value
#' @return a vector of expression.
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
#' @return a vector of character
#' @examples
#' p <- c(1e-05, 0.002, 0.03, 0.4)
#' p2star(p)
#' @export
p2star <- function(p)
  ifelse(!is.finite(p), NA,
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", "ns"))))




# plot_diagnosis ==============================================================

#' Generic function of plot_diagnosis
#' @param model model object
#' @param ... additional arguments
#' @note This is a generic function to make the diagnosis plot of the model.
#' @keywords function
#' @author ISHIKURA Kiwamu
#' @export
#' @seealso \code{\link{plot}}
plot_diagnosis <- function(model, ...) UseMethod("plot_diagnosis")


#' Diagnosis plot fo lmerMod object
#' @importFrom stats cooks.distance fitted resid
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point labs stat_qq_line stat_qq geom_hline
#' @importFrom cowplot plot_grid
#' @param model lmerMod object
#' @inheritDotParams lme4::lmer
#' @note It produces four plots: Residual vs Fitted, Normal QQ, Scale-Location, and Cook's distance.
#' @examples
#' library(lme4)
#' fit <- lmer(circumference ~ age + (1|Tree), Orange)
#' plot_diagnosis(fit)
#' @export
plot_diagnosis.lmerMod <- function(model, ...) {
  df <- tibble(
    fitted = fitted(model),
    resid = resid(model, type = "pearson"),
    resid_std = sqrt(abs(resid(model, type = "pearson", scaled = TRUE))),
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
