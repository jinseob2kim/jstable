context("Show table with label")

test_that("Make label data", {
  expect_is(mk.lev(iris), "data.table")
})

test_that("LabelepiDisplay", {
  fit <- glm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  fit.table <- epiDisplay::regress.display(fit, crude = TRUE, crude.p.value = TRUE)
  iris.label <- mk.lev(iris)
  expect_is(LabelepiDisplay(fit.table, label = TRUE, ref = iris.label), "matrix")
})


test_that("LabeljsTable", {
  library(coxme)
  fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1|inst) +(1|sex), lung)
  fit.table <- coxme.display(fit)
  lung.label <- mk.lev(lung)
  expect_is(LabeljsMixed(fit.table, ref = lung.label), "list")
})


test_that("LabeljsCox", {
  library(survival)
  fit <- coxph(Surv(time, status) ~ sex + ph.ecog + ph.karno + cluster(inst), data = lung, model = TRUE)
  fit.table <- cox2.display(fit)
  lung.label <- mk.lev(lung)
  LabeljsCox(fit.table, ref = lung.label)
  expect_is(LabeljsCox(fit.table, ref = lung.label), "list")
})


test_that("LabeljsGeeglm", {
  library(geepack);data(dietox)
  dietox$Cu <- as.factor(dietox$Cu)
  gee01 <- geeglm (Weight ~ Time + Cu , id =Pig, data = dietox, family=gaussian,corstr="ex")
  g1 <- geeglm.display(gee01)
  expect_is(LabeljsGeeglm(g1, ref = mk.lev(dietox)), "list")
})

