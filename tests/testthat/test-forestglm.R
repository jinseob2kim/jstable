context("Show sub-group table")


test_that("Run TableSubgroupMultiGLM", {
  library(survival)
  library(dplyr)
  library(magrittr)
  lung %>%
    dplyr::mutate(
      status = as.integer(status == 1),
      sex = factor(sex),
      kk = factor(as.integer(pat.karno >= 70)),
      kk1 = factor(as.integer(pat.karno >= 60))
    ) -> lung

  expect_is(TableSubgroupMultiGLM(status ~ sex, data = lung, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), data = lung, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(pat.karno ~ sex, var_subgroups = c("kk", "kk1"), data = lung, family = "gaussian", line = TRUE), "data.frame")
  expect_is(TableSubgroupMultiGLM(status ~ sex + (1 | inst), var_subgroups = c("kk", "kk1"), data = lung, family = "gaussian", line = TRUE), "data.frame")

  ## Survey data
  library(survey)
  expect_warning(data.design <- svydesign(id = ~1, data = lung))
  expect_is(TableSubgroupMultiGLM(status ~ sex, data = data.design, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(pat.karno ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "gaussian", line = TRUE), "data.frame")
})
