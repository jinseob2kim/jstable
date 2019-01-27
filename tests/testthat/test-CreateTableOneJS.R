context("Show Table 1")

test_that("Run CreateOneTableJS", {
  library(survival)
  lung$sex <- as.factor(lung$sex)
  lung$status <- as.factor(lung$status)
  lung.label <- mk.lev(lung)
  expect_is(CreateTableOneJS(vars = names(lung), data = lung), "list")
  expect_is(CreateTableOneJS(vars = names(lung), data = lung, labeldata = lung.label, Labels = T), "list")
  
  expect_is(CreateTableOneJS(vars = names(lung), strata = "sex", data = lung), "list")
  expect_is(CreateTableOneJS(vars = names(lung), strata = "sex", data = lung, labeldata = lung.label, Labels = T), "list")
  
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, labeldata = lung.label, Labels = T))
})