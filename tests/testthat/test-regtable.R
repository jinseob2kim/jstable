context("Show regression table")


test_that("Run glmshow.display", {
  expect_is(glmshow.display(glm(mpg ~ cyl, data = mtcars)), "display")
  expect_is(glmshow.display(glm(mpg ~ cyl + disp, data = mtcars)), "display")
  expect_is(glmshow.display(glm(am ~ cyl, data = mtcars, family = "binomial")), "display")
  expect_is(glmshow.display(glm(am ~ cyl + disp, data = mtcars, family = "binomial")), "display")
  expect_is(glmshow.display(glm(am ~ cyl + disp, data = mtcars, family = "binomial"),pcut.univariate = 0.0055), "display")
  expect_is(glmshow.display(glm(mpg ~ factor(am) +wt+qsec, data = mtcars, family = "gaussian"), pcut.univariate = 0.01), "display")
})

test_that("Run cox2.display", {
  library(survival)
  # data(lung)
  fit00 <- coxph(Surv(time, status) ~ ph.ecog, data = lung, model = TRUE)
  fit0 <- coxph(Surv(time, status) ~ ph.ecog + age, data = lung, model = TRUE)
  fit1 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), data = lung, model = TRUE)
  fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), data = lung, model = TRUE)

  # all status 0 case
  lung.all0 <- lung
  lung.all0$status <- 0
  fit00.all0 <- coxph(Surv(time, status) ~ ph.ecog, data = lung.all0, model = TRUE)
  fit0.all0 <- coxph(Surv(time, status) ~ ph.ecog + age, data = lung.all0, model = TRUE)
  fit1.all0 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), data = lung.all0, model = TRUE)
  fit2.all0 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), data = lung.all0, model = TRUE)

  expect_is(cox2.display(fit0), "list")
  expect_is(cox2.display(fit1), "list")
  expect_is(cox2.display(fit2), "list")
  expect_is(cox2.display(fit0.all0), "list")
  expect_is(cox2.display(fit1.all0), "list")
  expect_is(cox2.display(fit2.all0), "list")

  # interaction tests (with data_for_univariate)
  lung$sex_factor <- factor(lung$sex, labels = c("Male", "Female"))
  lung$ecog_factor <- factor(lung$ph.ecog)
  fit_int_cat_cont <- coxph(Surv(time, status) ~ sex_factor * age, data = lung, model = TRUE)
  fit_int_cat_cat <- coxph(Surv(time, status) ~ sex_factor * ecog_factor, data = lung, model = TRUE)
  expect_is(cox2.display(fit_int_cat_cont, data_for_univariate = lung), "list")
  expect_is(cox2.display(fit_int_cat_cont, data_for_univariate = lung, pcut.univariate = 0.2), "list")
  expect_is(cox2.display(fit_int_cat_cat, data_for_univariate = lung), "list")

  # table structure test
  res00 <- cox2.display(fit00)
  res00.all0 <- cox2.display(fit00.all0)
  expect_equal(dim(res00$table), dim(res00.all0$table))
  expect_equal(rownames(res00$table), rownames(res00.all0$table))
  expect_equal(colnames(res00$table), colnames(res00.all0$table))

  res0 <- cox2.display(fit0)
  res0.all0 <- cox2.display(fit0.all0)
  expect_equal(dim(res0$table), dim(res0.all0$table))
  expect_equal(rownames(res0$table), rownames(res0.all0$table))
  expect_equal(colnames(res0$table), colnames(res0.all0$table))

  res1 <- cox2.display(fit1)
  res1.all0 <- cox2.display(fit1.all0)
  expect_equal(dim(res1$table), dim(res1.all0$table))
  expect_equal(rownames(res1$table), rownames(res1.all0$table))
  expect_equal(colnames(res1$table), colnames(res1.all0$table))

  res2 <- cox2.display(fit2)
  res2.all0 <- cox2.display(fit2.all0)
  expect_equal(dim(res2$table), dim(res2.all0$table))
  expect_equal(rownames(res2$table), rownames(res2.all0$table))
  expect_equal(colnames(res2$table), colnames(res2.all0$table))
})


test_that("Run svyglm.display", {
  library(survey)
  data(api)
  apistrat$tt <- c(rep(1, 20), rep(0, nrow(apistrat) - 20))
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  ds <- svyglm(api00 ~ ell + meals, design = dstrat)
  expect_is(svyregress.display(ds, decimal = 3), "display")
  expect_is(svyregress.display(svyglm(api00 ~ ell, design = dstrat), decimal = 3), "display")
  expect_is(svyregress.display(ds, decimal = 3, pcut.univariate = 0.05), "display")
  ds2 <- svyglm(tt ~ ell + meals + cname + mobility, design = dstrat, family = quasibinomial())
  expect_is(svyregress.display(ds2, decimal = 3), "display")
  expect_is(svyregress.display(ds2, decimal = 3, pcut.univariate = 0.05), "display")
})

test_that("Run svycox.display", {
  library(survival)
  data(pbc)
  pbc$sex <- factor(pbc$sex)
  pbc$stage <- factor(pbc$stage)
  pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
  biasmodel <- glm(randomized ~ age * edema, data = pbc, family = binomial)
  pbc$randprob <- fitted(biasmodel)

  if (is.null(pbc$albumin)) pbc$albumin <- pbc$alb ## pre2.9.0

  dpbc <- survey::svydesign(id = ~1, prob = ~randprob, strata = ~edema, data = subset(pbc, randomized))
  model <- survey::svycoxph(Surv(time, status > 0) ~ sex + protime + albumin + stage, design = dpbc)
  expect_is(svycox.display(model), "list")
  expect_is(svycox.display(model, pcut.univariate = 0.02), "list")
  expect_is(svycox.display(model, pcut.univariate = 0.001), "list")
})


library(geepack)
data(dietox)

test_that("Run geeglm.display", {
  expect_is(geeglm.display(geeglm(Weight ~ Time, id = Pig, data = dietox, family = gaussian, corstr = "ex")), "list")
  expect_is(geeglm.display(geeglm(Weight ~ Time + Cu, id = Pig, data = dietox, family = gaussian, corstr = "ex")), "list")
  dietox$Weight_cat <- as.integer(dietox$Weight > 50)
  expect_is(geeglm.display(geeglm(Weight_cat ~ Time, id = Pig, data = dietox, family = binomial, corstr = "ex")), "list")
  expect_is(geeglm.display(geeglm(Weight_cat ~ Time + Cu, id = Pig, data = dietox, family = binomial, corstr = "ex")), "list")
  expect_is(geeglm.display(geeglm(Weight ~ Time + Cu, id = Pig, data = dietox, family = gaussian, corstr = "ex"),pcut.univariate =0.05), "list")
  expect_is(geeglm.display(geeglm(Weight_cat ~ Time + Cu, id = Pig, data = dietox, family = binomial, corstr = "ex"),pcut.univariate = 0.05), "list")
  
  # Interaction test
  expect_is(geeglm.display(geeglm(Weight ~ Time * Cu, id = Pig, data = dietox, family = gaussian, corstr = "ex")), "list")
})


library(lme4)

test_that("Run lmer.display", {
  expect_is(lmer.display(lmer(Reaction ~ Days + (1 | Subject), sleepstudy)), "list")
  expect_is(lmer.display(lmer(Weight ~ Time + Feed + (1 | Pig) + (1 | Evit), data = dietox)), "list")
  expect_is(lmer.display(lmer(Weight ~ Time + Feed + (1 | Pig) + (1 | Evit), data = dietox),pcut.univariate = 0.05), "list")
  dietox$Weight_cat <- as.integer(dietox$Weight > 50)
  expect_is(lmer.display(glmer(Weight_cat ~ Time + Cu + (1 | Pig) + (1 | Evit), data = dietox, family = binomial)), "list")
  expect_is(lmer.display(glmer(Weight_cat ~ Time + (1 | Pig) + (1 | Evit), data = dietox, family = binomial)), "list")
  expect_is(lmer.display(glmer(Weight_cat ~ Time + Cu + (1 | Pig) + (1 | Evit), data = dietox, family = binomial),pcut.univariate = 0.05), "list")
  expect_is(lmer.display(glmer(Weight_cat ~ Time + Cu + (1 | Pig) + (1 | Evit), data = dietox, family = binomial),pcut.univariate = 0.6), "list")
})
