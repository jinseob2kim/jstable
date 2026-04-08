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
  expect_is(suppressWarnings(TableSubgroupMultiGLM(status ~ sex + (1 | inst), var_subgroups = c("kk", "kk1"), data = lung, family = "gaussian", line = TRUE)), "data.frame")

  ## Survey data
  library(survey)
  expect_warning(data.design <- svydesign(id = ~1, data = lung))
  expect_is(TableSubgroupMultiGLM(status ~ sex, data = data.design, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(pat.karno ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "gaussian", line = TRUE), "data.frame")

  
  lung %>%
    dplyr::mutate(
      status = as.integer(status == 1),
      sex = factor(sex),
      ph.ecog = factor(ph.ecog),
      kk = factor(ifelse(pat.karno < 70,1,
                         ifelse(pat.karno >= 70 & pat.karno <= 90,2,3)))
    ) -> lung
  expect_warning(data.design <- svydesign(id = ~1, data = lung))
  
  
  expect_is(TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "gaussian"), "data.frame")
  expect_is(TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "binomial"), "data.frame")
  expect_is(TableSubgroupMultiGLM(pat.karno ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, family = "gaussian"), "data.frame")
  

  

  
  
  
  
})

test_that("TableSubgroupMultiGLM preserves offset in subgroup poisson models", {
  set.seed(94)
  n <- 1200
  dd <- data.frame(
    x = factor(sample(c("A", "B"), n, TRUE)),
    g = factor(sample(c("H", "L"), n, TRUE)),
    expo = sample(1:5, n, TRUE)
  )
  rr_H <- 0.2
  rr_L <- 1.8
  dd$y <- rpois(
    n,
    lambda = exp(-1 + log(ifelse(dd$x == "B" & dd$g == "H", rr_H,
                                 ifelse(dd$x == "B" & dd$g == "L", rr_L, 1)))) * dd$expo
  )

  fit_int <- glm(y ~ x + offset(log(expo)) + g + x:g, data = dd, family = poisson())
  expected_p <- summary(fit_int)$coefficients[nrow(summary(fit_int)$coefficients), 4]
  expected_p <- if (expected_p < 0.001) "<0.001" else as.character(round(expected_p, 3))

  res <- TableSubgroupMultiGLM(
    y ~ x + offset(log(expo)),
    var_subgroups = "g",
    data = dd,
    family = "poisson"
  )

  subgroup_rows <- trimws(res$Variable) %in% levels(dd$g)
  expect_true(all(!is.na(suppressWarnings(as.numeric(res$RR[subgroup_rows])))))
  expect_true(all(!is.na(suppressWarnings(as.numeric(res$Lower[subgroup_rows])))))
  expect_true(all(!is.na(suppressWarnings(as.numeric(res$Upper[subgroup_rows])))))
  expect_identical(as.character(res$`P for interaction`[trimws(res$Variable) == "g"]), expected_p)
})
