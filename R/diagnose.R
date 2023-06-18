# Diagnose the model

#' @export
diagnose <- function(model, ...) UseMethod("diagnose", model)


#' @export
diagnose.default <- function(model, ...) 
  stop(paste("There is no diagnose() method for this object of the class",
             paste(class(model), collapse = ", ")))


#' Diagnose a aov model
#' @importFrom broom augment
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr mutate rename
#' @param model aov object
#' @note To diagnose the aov model
#' @export
diagnose.aov <- function(model) {
  model_lm <- lm(model)
  influence <- augment(model_lm) |>
    rowid_to_column("id") |>
    rename(cooksd = .cooksd,
           leverage.overall = .hat) |>
    mutate(.std.resid_abs_sqrt = sqrt(abs(.std.resid)))
  
  return(new_tibble_diag(influence, model))
}


#' Diagnose a lm model
#' @importFrom broom augment
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr mutate rename
#' @param model lm object
#' @note To diagnose the lm model
#' @export
diagnose.lm <- function(model) {
  influence <- augment(model) |>
    rowid_to_column("id") |>
    rename(cooksd = .cooksd,
           leverage.overall = .hat) |>
    mutate(.std.resid_abs_sqrt = sqrt(abs(.std.resid)))

  return(new_tibble_diag(influence, model))
}


#' Diagnose a lme model
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom dplyr mutate
#' @param model lme object
#' @note To diagnose the lme model
#' @export
diagnose.lme <- function(model) {
  influence <- hlm_augment(model, include.ls = FALSE) |>
    mutate(.std.resid = resid(model, type = "pearson"),
           .std.resid_abs_sqrt = sqrt(abs(.std.resid)))

  return(new_tibble_diag(influence, model))
}