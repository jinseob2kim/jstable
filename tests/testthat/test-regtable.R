context("Show regression table")


test_that("Run glmshow.display", {
  expect_is(glmshow.display(glm(mpg ~ cyl, data = mtcars)), "display")
  expect_is(glmshow.display(glm(mpg ~ cyl + disp, data = mtcars)), "display")
  expect_is(glmshow.display(glm(am ~ cyl, data = mtcars, family = "binomial")), "display")
  expect_is(glmshow.display(glm(am ~ cyl + disp, data = mtcars, family = "binomial")), "display")
})

test_that("Run cox2.display", {
  library(survival)
  data(lung)
  fit0 <- coxph(Surv(time, status) ~ ph.ecog + age, data = lung, model = TRUE)
  fit1 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), data = lung, model = TRUE)
  fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), data = lung, model = TRUE)

  expect_is(cox2.display(fit0), "list")
  expect_is(cox2.display(fit1), "list")
  expect_is(cox2.display(fit2), "list")
})


test_that("Run svyglm.display", {
  library(survey)
  data(api)
  apistrat$tt <- c(rep(1, 20), rep(0, nrow(apistrat) - 20))
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  ds <- svyglm(api00 ~ ell + meals, design = dstrat)
  expect_is(svyregress.display(ds, decimal = 3), "display")
  expect_is(svyregress.display(svyglm(api00 ~ ell, design = dstrat), decimal = 3), "display")
  ds2 <- svyglm(tt ~ ell + meals + cname + mobility, design = dstrat, family = quasibinomial())
  expect_is(svyregress.display(ds2, decimal = 3), "display")
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
})


library(geepack)
data(dietox)

test_that("Run geeglm.display", {
  expect_is(geeglm.display(geeglm(Weight ~ Time, id = Pig, data = dietox, family = gaussian, corstr = "ex")), "list")
  expect_is(geeglm.display(geeglm(Weight ~ Time + Cu, id = Pig, data = dietox, family = gaussian, corstr = "ex")), "list")
  dietox$Weight_cat <- as.integer(dietox$Weight > 50)
  expect_is(geeglm.display(geeglm(Weight_cat ~ Time, id = Pig, data = dietox, family = binomial, corstr = "ex")), "list")
  expect_is(geeglm.display(geeglm(Weight_cat ~ Time + Cu, id = Pig, data = dietox, family = binomial, corstr = "ex")), "list")
})


library(lme4)

test_that("Run lmer.display", {
  expect_is(lmer.display(lmer(Reaction ~ Days + (1 | Subject), sleepstudy)), "list")
  expect_is(lmer.display(lmer(Weight ~ Time + Feed + (1 | Pig) + (1 | Evit), data = dietox)), "list")
  dietox$Weight_cat <- as.integer(dietox$Weight > 50)
  expect_is(lmer.display(glmer(Weight_cat ~ Time + Cu + (1 | Pig) + (1 | Evit), data = dietox, family = binomial)), "list")
  expect_is(lmer.display(glmer(Weight_cat ~ Time + (1 | Pig) + (1 | Evit), data = dietox, family = binomial)), "list")
})
