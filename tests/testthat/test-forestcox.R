context("Show sub-group table")


test_that("Run TableSubgroupMultiCox", {
  library(survival); library(dplyr)
  lung %>% 
    mutate(status = as.integer(status == 1),
           sex = factor(sex),
           kk = factor(as.integer(pat.karno >= 70)),
           kk1 = factor(as.integer(pat.karno >= 60))) -> lung
  
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, time_eventrate = 100, data=lung), "data.frame")
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data=lung, time_eventrate = 100))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data=lung, time_eventrate = 100, line = TRUE))
  
  ## Survey data
  library(survey)
  expect_warning(data.design <- svydesign(id = ~1, data = lung))
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, data=data.design, time_eventrate = 100), "data.frame")
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE))
  
  })