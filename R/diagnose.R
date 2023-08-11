# Diagnose the model

#' @export
diagnose <- function(model, ...) UseMethod("diagnose", model)


#' @export
diagnose.default <- function(model, ...) 
  stop(paste("There is no diagnose() method for this object of the class",
             paste(class(model), collapse = ", ")))


#' Diagnose a aov model
#' @description To generate a diagnosis of an \code{aov} model.
#' @importFrom broom augment
#' @importFrom dplyr mutate rename
#' @param model \code{\link[stats]{aov}} object.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{\link[stats]{aov}} model.
#' @export
#' @examples
#' op <- options(contrasts = c("contr.sum", "contr.poly"))
#' aov_npk <- aov(yield ~ block + N*P*K, npk)
#' (diag_npk <- diagnose(aov_npk))
#' options(op)
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
#' @description To generate a diagnosis of a \code{lm} model.
#' @importFrom broom augment
#' @importFrom dplyr mutate rename
#' @param model \code{\link[stats]{lm}} object.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{\link[stats]{lm}} model.
#' @export
#' @examples
#' lm_rock <- lm(perm ~ area + peri + shape, rock)
#' (diag_rock <- diagnose(aov_rock))
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
#' @description To generate a diagnosis of a \code{lme} model.
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom dplyr mutate rename
#' @param model \code{\link[nlme]{lme}} object.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{\link[nlme]{lme}} model.
#' @export
#' @examples
#' library(nlme)
#' lme_orthodont <- lme(distance ~ age + Sex, Orthodont)
#' (diag_orthodont <- diagnose(lme_orthodont))
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

  return(new_tibble_diag(influence, influence_groups, model))
}


#' Diagnose a lmerMod model
#' @description To generate a diagnosis of a \code{lmerMod} model.
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate rename left_join
#' @importFrom lme4 ranef
#' @param model \code{lmerMod} object.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{lmerMod} model.
#' @export
#' @examples
#' library(lme4)
#' lmer_orthodont <- lmer(distance ~ age + Sex + (age + Sex | Subject),
#'                        data = nlme::Orthodont)
#' (diag_orthodont <- diagnose(lmer_orthodont))
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



get_data_classes <- function(model, ...) UseMethod("get_data_classes", model)

#' @export
get_data_classes.default <- function(model, ...) 
  stop(paste("There is no get_data_classes() method for this object of the class",
             paste(class(model), collapse = ", ")))

get_data_classes.lm <- function(model) {
  attr(model$terms, "dataClasses")
}

get_data_classes.aov <- function(model) {
  attr(model$terms, "dataClasses")
}

get_data_classes.lme <- function(model) {
  attr(model$terms, "dataClasses")
}

get_data_classes.lmerMod <- function(model) {
  attr(attr(slot(model, "frame"), "terms"), "dataClasses")
}