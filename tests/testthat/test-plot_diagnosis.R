### Load libraries
library(ggplot2)
library(nlme)


### Load data & diagnosis
data(Orthodont, package = "nlme")
lm_od <- lm(distance ~ age + Sex, Orthodont)
lme_od <- lme(distance ~ age + Sex, Orthodont)
diag_lm_od <- diagnose(lm_od)
diag_lme_od <- diagnose(lme_od)


### Test for plots
expect_ggplot_class <- function(x) {
  expect_type(x, "list")
  expect_s3_class(x, c("gg", "ggplot"))
}
test_that("Can you generate plots without errors?", {
  # measured vs fitted
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "mf")))
  expect_success(expect_ggplot_class(plot(diag_lme_od, type = "mf")))
 
  # residuals vs fitted
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "rf")))
  expect_success(expect_ggplot_class(plot(diag_lme_od, type = "rf")))

  # QQ norm
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "qq")))
  expect_success(expect_ggplot_class(plot(diag_lme_od, type = "qq")))

  # Scale-Location
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "sl")))
  expect_success(expect_ggplot_class(plot(diag_lme_od, type = "sl")))

  # Cook's distance
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "cook")))
  expect_success(expect_ggplot_class(plot(diag_lme_od, type = "cook")))

  # Marginal-model
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "mm", aes(x = age))))
  expect_error(plot(diag_lm_od, type = "mm", aes(x = Sex)))
  expect_success(expect_ggplot_class(plot(diag_lme_od, type = "mm", aes(x = age))))
  expect_error(plot(diag_lme_od, type = "mm", aes(x = Sex)))

  # Added-variable
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "av", aes(x = age))))
  expect_error(plot(diag_lm_od, type = "av", aes(x = Sex)))
  expect_error(plot(diag_lme_od, type = "av", aes(x = age)))

  # Component+residuals
  expect_success(expect_ggplot_class(plot(diag_lm_od, type = "cr", aes(x = age))))
  expect_error(plot(diag_lm_od, type = "cr", aes(x = Sex)))
  expect_error(plot(diag_lme_od, type = "cr", aes(x = age)))
})