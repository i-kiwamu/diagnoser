### Load libraries
library(lme4, quietly = TRUE)
library(nlme, quietly = TRUE)


### Load data
data(sleepstudy, package = "lme4")
lme_sleep <- lme(Reaction ~ Days, random =  ~ Days|Subject, data = sleepstudy)
lmer_sleep <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

data(Chem97, package = "mlmRev")
Chem97 <- Chem97[1:257,]
lme_chem <- lme(score ~ gcsecnt, random = ~1|lea/school, data = Chem97)
lmer_chem <- lmer(score ~ gcsecnt + (1|lea/school), data = Chem97)


### Test for lme
test_that("Are diagnosis outputs equal to lme model outputs?", {
  # sleep study
  diag_lme_sleep <- diagnose(lme_sleep)
  expect_setequal(diag_lme_sleep$.fitted, fitted(lme_sleep))
  expect_setequal(diag_lme_sleep$.resid, resid(lme_sleep))
  expect_equal(diag_lme_sleep$.std.resid, resid(lme_sleep, type = "pearson"))
  expect_setequal(diag_lme_sleep$cooksd, cooks.distance(lme_sleep))
  
  # chemistry
  diag_lme_chem <- diagnose(lme_chem)
  expect_setequal(diag_lme_chem$.fitted, fitted(lme_chem))
  expect_setequal(diag_lme_chem$.resid, resid(lme_chem))
  expect_equal(diag_lme_chem$.std.resid, resid(lme_chem, type = "pearson"))
  expect_setequal(diag_lme_chem$cooksd, cooks.distance(lme_chem))
})


### Test for lmerMod
test_that("Are diagnosis outputs equal to lmerMod model outputs?", {
  # sleep study
  diag_lmer_sleep <- diagnose(lmer_sleep)
  expect_setequal(diag_lmer_sleep$.fitted, fitted(lmer_sleep))
  expect_setequal(diag_lmer_sleep$.resid, resid(lmer_sleep))
  expect_equal(diag_lmer_sleep$.std.resid, resid(lmer_sleep, type = "pearson", scaled = TRUE))
  expect_setequal(diag_lmer_sleep$cooksd, cooks.distance(lmer_sleep))

  # chemistry
  diag_lme_chem <- diagnose(lme_chem)
  expect_setequal(diag_lme_chem$.fitted, fitted(lme_chem))
  expect_setequal(diag_lme_chem$.resid, resid(lme_chem))
  expect_equal(diag_lme_chem$.std.resid, resid(lme_chem, type = "pearson", scaled = TRUE))
  expect_setequal(diag_lme_chem$cooksd, cooks.distance(lme_chem))
})