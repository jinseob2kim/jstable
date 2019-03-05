context("Show Table 1")

test_that("Run CreateOneTableJS", {
  library(survival)
  lung$sex <- as.factor(lung$sex)
  lung$status <- as.factor(lung$status)
  lung$ph.ecog <- as.factor(lung$ph.ecog)
  lung.label <- mk.lev(lung)
  expect_is(CreateTableOneJS(vars = names(lung), data = lung), "list")
  expect_is(CreateTableOneJS(vars = names(lung), data = lung, showAllLevels = F), "list")
  expect_is(CreateTableOneJS(vars = names(lung), data = lung, labeldata = lung.label, Labels = T), "list")
  expect_is(CreateTableOneJS(vars = names(lung), data = lung, labeldata = lung.label, Labels = T, showAllLevels = F), "list")
  
  expect_is(CreateTableOneJS(vars = names(lung), strata = "sex", data = lung), "list")
  expect_is(CreateTableOneJS(vars = names(lung), strata = "sex", data = lung, labeldata = lung.label, Labels = T), "list")
  expect_is(CreateTableOneJS(vars = names(lung), strata = "sex", data = lung, showAllLevels = F), "list")
  expect_is(CreateTableOneJS(vars = names(lung), strata = "sex", data = lung, showAllLevels = F, labeldata = lung.label, Labels = T), "list")
  
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, showAllLevels = F))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, psub = F))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, psub = F, showAllLevels = F))
  
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, labeldata = lung.label, Labels = T))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, showAllLevels = F, labeldata = lung.label, Labels = T))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, labeldata = lung.label, Labels = T, psub = F))
  expect_warning(CreateTableOneJS(vars = names(lung), strata = "sex", strata2 = "ph.ecog", data = lung, showAllLevels = F, labeldata = lung.label, Labels = T, psub = F))
})