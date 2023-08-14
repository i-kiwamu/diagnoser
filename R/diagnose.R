# Diagnose the model

#' diagnose
#' @description A generic function of a diagnosis of a model object
#' @details Available models are \code{aov}, \code{lm}, \code{lme}, and \code{lmerMod}.
#' @param model \code{\link[stats]{aov}} object.
#' @param ... Currently not to be used.
#' @return An object of \code{tbl_df_diag}.
#' @export
diagnose <- function(model, ...) UseMethod("diagnose", model)


#' @export
diagnose.default <- function(model, ...) 
  stop(paste("There is no diagnose() method for this object of the class",
             paste(class(model), collapse = ", ")))


#' diagnose.aov
#' @description To generate a diagnosis of an \code{aov} model.
#' @importFrom stats lm
#' @importFrom broom augment
#' @importFrom rlang .data
#' @importFrom dplyr mutate rename
#' @importFrom tibble tibble rowid_to_column
#' @param model \code{\link[stats]{aov}} object.
#' @param ... Currently not to be used.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{\link[stats]{aov}} model.
#' @export
#' @examples
#' op <- options(contrasts = c("contr.sum", "contr.poly"))
#' aov_npk <- aov(yield ~ block + N*P*K, npk)
#' (diag_npk <- diagnose(aov_npk))
#' options(op)
diagnose.aov <- function(model, ...) {
  model_lm <- lm(model)
  influence <- augment(model_lm) %>%
    rowid_to_column(var = ".rowid") %>%
    rename(cooksd = ".cooksd",
           leverage.overall = ".hat") %>%
    mutate(.std.resid_abs_sqrt = sqrt(abs(.data$.std.resid)))
  if (!is.element(".rownames", colnames(influence))) {
    influence <- influence %>%
      mutate(.rownames = rownames(influence))
  }
  influence <- influence %>%
    mutate(label = ifelse(.data$cooksd > 1, .data$.rownames, ""))
  
  return(new_tibble_diag(influence, NULL, model))
}


#' diagnose.lm
#' @description To generate a diagnosis of a \code{lm} model.
#' @importFrom broom augment
#' @importFrom rlang .data
#' @importFrom dplyr mutate rename
#' @importFrom tibble tibble rowid_to_column
#' @param model \code{\link[stats]{lm}} object.
#' @param ... Currently not to be used.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{\link[stats]{lm}} model.
#' @export
#' @examples
#' lm_rock <- lm(perm ~ area + peri + shape, rock)
#' (diag_rock <- diagnose(lm_rock))
diagnose.lm <- function(model, ...) {
  influence <- augment(model) %>%
    rowid_to_column(var = ".rowid") %>%
    rename(cooksd = ".cooksd",
           leverage.overall = ".hat") %>%
    mutate(.std.resid_abs_sqrt = sqrt(abs(.data$.std.resid)))
  if (!is.element(".rownames", colnames(influence))) {
    influence <- influence %>%
      mutate(.rownames = rownames(influence))
  }
  influence <- influence %>%
    mutate(label = ifelse(.data$cooksd > 1, .data$.rownames, ""))

  return(new_tibble_diag(influence, NULL, model))
}


#' diagnose.lme
#' @description To generate a diagnosis of a \code{lme} model.
#' @importFrom stats resid
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom rlang .data
#' @importFrom dplyr mutate rename
#' @importFrom tibble tibble rowid_to_column
#' @param model \code{\link[nlme]{lme}} object.
#' @param ... Currently not to be used.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{\link[nlme]{lme}} model.
#' @export
#' @examples
#' library(nlme)
#' lme_orthodont <- lme(distance ~ age + Sex, Orthodont)
#' (diag_orthodont <- diagnose(lme_orthodont))
diagnose.lme <- function(model, ...) {
  influence <- hlm_augment(model, include.ls = FALSE) %>%
    rowid_to_column(var = ".rowid") %>%
    rename(.rownames = "id") %>%
    mutate(.std.resid = resid(model, type = "pearson"),
           .std.resid_abs_sqrt = sqrt(abs(.data$.std.resid)))
  if (any(is.na(influence$.rownames))) {
    influence$.rownames <- rownames(model$data)
  }
  influence <- influence %>%
    mutate(label = ifelse(.data$cooksd > 1, .data$.rownames, ""))
  
  groups <- colnames(model$groups)
  influence_groups <- list()
  for(g in groups) {
    influence_groups[[g]] <-
      hlm_augment(model, level = g, include.ls = FALSE) %>%
      rowid_to_column(var = ".rowid")
  }

  return(new_tibble_diag(influence, influence_groups, model))
}


#' diagnose.lmerMod
#' @description To generate a diagnosis of a \code{lmerMod} model.
#' @importFrom methods slot
#' @importFrom stats resid
#' @importFrom HLMdiag hlm_augment hlm_resid
#' @importFrom tibble rownames_to_column rowid_to_column
#' @importFrom rlang .data
#' @importFrom dplyr mutate rename left_join
#' @importFrom lme4 ranef
#' @param model \code{lmerMod} object.
#' @param ... Currently not to be used.
#' @return An object of \code{tbl_df_diag}.
#' @note To diagnose the \code{lmerMod} model.
#' @export
#' @examples
#' library(nlme)
#' library(lme4)
#' lmer_orthodont <- lmer(distance ~ age + Sex + (age + Sex | Subject),
#'                        data = Orthodont)
#' (diag_orthodont <- diagnose(lmer_orthodont))
diagnose.lmerMod <- function(model, ...) {
  influence <- hlm_augment(model, include.ls = FALSE) %>%
    rowid_to_column(var = ".rowid") %>%
    rename(.rownames = "id") %>%
    mutate(.std.resid = resid(model, type = "pearson", scaled = TRUE),
           .std.resid_abs_sqrt = sqrt(abs(.data$.std.resid)))
  if (any(is.na(influence$.rownames))) {
    influence$.rownames <- rownames(slot(model, "frame"))
  }
  influence <- influence %>%
    mutate(label = ifelse(.data$cooksd > 1, .data$.rownames, ""))
  
  bs <- ranef(model)
  groups <- names(slot(model, "flist"))
  influence_groups <- list()
  for(g in groups) {
    b <- rownames_to_column(bs[[g]], var = "group_name")
    xg <- hlm_augment(model, level = g, include.ls = FALSE) %>%
      rowid_to_column(var = ".rowid") %>%
      rename(group_name = !! g) %>%
      left_join(b, by = "group_name")
    influence_groups[[g]] <- xg
  }
  
  return(new_tibble_diag(influence, influence_groups, model))
}



get_data_classes <- function(model, ...) UseMethod("get_data_classes", model)

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