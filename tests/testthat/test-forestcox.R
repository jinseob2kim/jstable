context("Show sub-group table")


test_that("Run TableSubgroupMultiCox", {
  library(survival)
  library(dplyr)
  lung %>%
    mutate(
      status = as.integer(status == 1),
      sex = factor(sex),
      kk = factor(as.integer(pat.karno >= 70)),
      kk1 = factor(as.integer(pat.karno >= 60))
    ) -> lung
  lung.label <- mk.lev(lung)
  lung.label <- lung.label %>%
    mutate(
      val_label = case_when(
        variable == "sex" & level == "1" ~ "Male",
        variable == "sex" & level == "2" ~ "Female",
        variable == "kk" & level == "0" ~ "No",
        variable == "kk" & level == "1" ~ "Yes",
        variable == "kk1" & level == "0" ~ "No",
        variable == "kk1" & level == "1" ~ "Yes",
        TRUE ~ val_label
      )
    )
  lung.label
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, time_eventrate = 100, data = lung), "data.frame")
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, strata = "inst"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, weights = "age"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = T))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = T, count_by = "sex"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = F, count_by = "sex"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", labeldata = lung.label))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = TRUE, labeldata = lung.label))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = TRUE, count_by = "sex", labeldata = lung.label))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = FALSE, count_by = "sex", labeldata = lung.label))

  ## Survey data
  library(survey)
  expect_warning(data.design <- svydesign(id = ~1, data = lung))
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, data = data.design, time_eventrate = 100), "data.frame")
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = T))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = F, count_by = "sex"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = T, count_by = "sex"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = T, count_by = "sex", labeldata = lung.label))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, strata = "inst"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, weights = "age"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst"))
  expect_warning(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age"))
})
