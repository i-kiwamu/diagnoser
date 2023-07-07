# Diagnose the model

#' @export
diagnose <- function(model, ...) UseMethod("diagnose", model)


#' @export
diagnose.default <- function(model, ...) 
  stop(paste("There is no diagnose() method for this object of the class",
             paste(class(model), collapse = ", ")))


#' Diagnose a aov model
#' @importFrom broom augment
#' @importFrom dplyr mutate rename
#' @param model aov object
#' @note To diagnose the aov model
#' @export
diagnose.aov <- function(model) {
  model_lm <- lm(model)
  influence <- augment(model_lm) |>
    rename(cooksd = .cooksd,
           leverage.overall = .hat) |>
    mutate(.std.resid_abs_sqrt = sqrt(abs(.std.resid)))
  if (!is.element(".rownames", colnames(influence))) {
    influence <- influence |>
      mutate(.rownames = rownames(influence))
  }
  influence <- influence |>
    mutate(label = ifelse(cooksd > 0.05, .rownames, ""))
  
  return(new_tibble_diag(influence, NULL, model))
}


#' Diagnose a lm model
#' @importFrom broom augment
#' @importFrom dplyr mutate rename
#' @param model lm object
#' @note To diagnose the lm model
#' @export
diagnose.lm <- function(model) {
  influence <- augment(model) |>
    rename(cooksd = .cooksd,
           leverage.overall = .hat) |>
    mutate(.std.resid_abs_sqrt = sqrt(abs(.std.resid)))
  if (!is.element(".rownames", colnames(influence))) {
    influence <- influence |>
      mutate(.rownames = rownames(influence))
  }
  influence <- influence |>
    mutate(label = ifelse(cooksd > 0.05, .rownames, ""))

  return(new_tibble_diag(influence, NULL, model))
}


#' Diagnose a lme model
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom dplyr mutate rename
#' @param model lme object
#' @note To diagnose the lme model
#' @export
diagnose.lme <- function(model) {
  influence <- hlm_augment(model, include.ls = FALSE) |>
    rename(.rownames = id) |>
    mutate(.std.resid = resid(model, type = "pearson"),
           .std.resid_abs_sqrt = sqrt(abs(.std.resid)))
  if (any(is.na(influence$.rownames))) {
    influence$.rownames <- rownames(model$data)
  }
  influence <- influence |>
    mutate(label = ifelse(cooksd > 0.05, .rownames, ""))
  
  groups <- colnames(model$groups)
  influence_groups <- list()
  for(g in groups) {
    influence_groups[[g]] <- hlm_augment(model, level = g, include.ls = FALSE)
  }

  return(new_tibble_diag(influence, infuence_groups, model))
}


#' Diagnose a lmerMod model
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate rename left_join
#' @importFrom lme4 ranef
#' @param model lmerMod object
#' @note To diagnose the lmerMod model
#' @export
diagnose.lmerMod <- function(model) {
  influence <- hlm_augment(model, include.ls = FALSE) |>
    rename(.rownames = id) |>
    mutate(.std.resid = resid(model, type = "pearson", scaled = TRUE),
           .std.resid_abs_sqrt = sqrt(abs(.std.resid)))
  if (any(is.na(influence$.rownames))) {
    influence$.rownames <- rownames(slot(model, "frame"))
  }
  influence <- influence |>
    mutate(label = ifelse(cooksd > 0.05, .rownames, ""))
  
  bs <- ranef(model)
  groups <- names(slot(model, "flist"))
  influence_groups <- list()
  for(g in groups) {
    b <- rownames_to_column(bs[[g]]) |>
      rename(group_name = rowname)
    xg <- hlm_augment(model, level = g, include.ls = FALSE) |>
      rename(group_name = 1) |>
      left_join(b, by = "group_name")
    influence_groups[[g]] <- xg
  }
  
  return(new_tibble_diag(influence, influence_groups, model))
}