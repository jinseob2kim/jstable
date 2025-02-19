context("Show sub-group table")


test_that("Run TableSubgroupMultiCox", {
  library(survival)
  library(dplyr)
  lung$inst <- factor(sample(1:3, nrow(lung), replace = TRUE))
  lung$inst2 <- factor(sample(1:3, nrow(lung), replace = TRUE))
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
        variable == "inst" & level == "1" ~ "a",
        variable == "inst" & level == "2" ~ "b",
        variable == "inst" & level == "3" ~ "c",
        variable == "inst2" & level == "1" ~ "A",
        variable == "inst2" & level == "2" ~ "B",
        variable == "inst2" & level == "3" ~ "C",
        TRUE ~ val_label
      )
    )
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, time_eventrate = 100, data = lung), "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, strata = "inst"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, weights = "age"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = T),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = T, count_by = "sex"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = F, count_by = "sex"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", labeldata = lung.label),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = TRUE, labeldata = lung.label),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age", event = TRUE, count_by = "sex", labeldata = lung.label),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ inst2 + (1|inst), data = lung, time_eventrate = 365, labeldata = lung.label),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex+(1|inst) , var_subgroup = "inst2",  data = lung, time_eventrate = 365, labeldata =  lung.label),  "data.frame")
  
  ## Survey data
  library(survey)
  expect_warning(data.design <- svydesign(id = ~1, data = lung))
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, data = data.design, time_eventrate = 100), "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = T),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = F, count_by = "sex"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = T, count_by = "sex"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, time_eventrate = 100, line = TRUE, event = T, count_by = "sex", labeldata = lung.label),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, strata = "inst"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, weights = "age"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst"),  "data.frame")
  expect_is(TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = lung, time_eventrate = 100, line = TRUE, cluster = "inst", strata = "inst", weights = "age"),  "data.frame")
})
