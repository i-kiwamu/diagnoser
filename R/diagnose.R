# Diagnose the model

#' @export
diagnose <- function(model, ...) UseMethod("diagnose", model)


#' @export
diagnose.default <- function(model, ...) 
  stop(paste("There is no diagnose() method for this object of the class",
             paste(class(model), collapse = ", ")))


#' Diagnose a lm model
#' @importFrom broom augment
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr mutate rename
#' @importFrom stats ks.test
#' @importFrom car leveneTest
#' @param model lm object
#' @note To diagnose the lm model
#' @export
diagnose.lm <- function(model) {
  influence <- augment(model) |>
    rowid_to_column("id") |>
    rename(cooksd = .cooksd,
           leverage.overall = .hat) |>
    mutate(.std.resid_abs_sqrt = sqrt(abs(.std.resid)))
  
  # 1. Normality of errors
  ks_test <- 
    with(influence,
         ks.test(.std.resid, "pnorm", mean(.std.resid), sd(.std.resid)))
  
  # 2. Homogeneity of variance
  levene_test <- leveneTest(model)
  
  # 3. Possible influential data
  id_influential <- which(influence$cooksd > 1)
  
  # Return
  result <- new_tibble_diagnoser(
    influence,
    ks_test = ks_test,
    levene_test = levene_test,
    id_influential = id_influential
  )
  return(result)
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
}