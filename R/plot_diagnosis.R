#' Conversion from P-value to expression
#' @param p P-value
#' @examples
#' p <- c(1e-05, 0.002, 0.03, 0.4)
#' p2lt(p)
p2lt <- function(p)
  ifelse(p < 0.001, as.character(expression(italic(P) < 0.001)),
         ifelse(p < 0.01, as.character(expression(italic(P) < 0.01)),
                ifelse(p < 0.05, as.character(expression(italic(P) < 0.05)),
                       as.character(expression(italic(P) == round(p, 2))))))


#' Conversion from P-value to stars
#' @param p P-value
#' @examples
#' p <- c(1e-05, 0.002, 0.03, 0.4)
#' p2star(p)
p2star <- function(p)
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "ns")))
