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


### Test for nlme
test_that("", {
  # sleep study
  diag_lme_sleep <- diagnose(lme_sleep)
  expect_equal(diag_lme_sleep$cooksd, cooks.distance(lme_sleep))
})