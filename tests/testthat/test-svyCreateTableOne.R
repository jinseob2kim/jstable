context("Show Table 1 for survey data")

library(survey)
test_that("Run SvyCreateOneTableJS", {
  data(nhanes)
  nhanes$SDMVPSU <- as.factor(nhanes$SDMVPSU)
  nhanes$race <- as.factor(nhanes$race)
  nhanes$RIAGENDR <- as.factor(nhanes$RIAGENDR)
  a.label <- mk.lev(nhanes)
  a.label <- a.label %>%
    dplyr::mutate(val_label = dplyr::case_when(
      variable == "race" & level == "1" ~ "White",
      variable == "race" & level == "2" ~ "Black",
      variable == "race" & level == "3" ~ "Hispanic",
      variable == "race" & level == "4" ~ "Asian",
      TRUE ~ val_label
    ))
  nhanesSvy <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, nest = TRUE, data = nhanes)
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, n_original = F
  ), "list")
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, showAllLevels = F
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, showAllLevels = F, n_original = F
  ), "list")
  
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    strata = "RIAGENDR", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    strata = "RIAGENDR", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, n_original = F
  ), "list")
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    strata = "race", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, pairwise = T
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    strata = "race", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, pairwise = T, n_original = F
  ), "list")
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    strata = "RIAGENDR", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, showAllLevels = F
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
    strata = "RIAGENDR", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, showAllLevels = F, n_original = F
  ), "list")
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"),
    strata = "SDMVPSU", strata2 = "agecat", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"),
    strata = "SDMVPSU", strata2 = "agecat", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, n_original = F
  ), "list")
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"),
    strata = "SDMVPSU", strata2 = "agecat", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, showAllLevels = F
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"),
    strata = "SDMVPSU", strata2 = "agecat", data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T, showAllLevels = F, n_original = F
  ), "list")
  
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"), data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T,
    includeNA = F, showAllLevels = T, printToggle = F, quote = F, nonnormal = NULL, catDigits = 2, contDigits = 2
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"), data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T,
    includeNA = F, showAllLevels = T, printToggle = F, quote = F, nonnormal = NULL, catDigits = 2, contDigits = 2, n_original = F
  ), "list")
  
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"), data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T,
    includeNA = F, showAllLevels = F, printToggle = F, quote = F, nonnormal = NULL, catDigits = 2, contDigits = 2
  ), "list")
  expect_is(svyCreateTableOneJS(
    vars = c("HI_CHOL", "race", "RIAGENDR"), data = nhanesSvy, factorVars = c("HI_CHOL", "race", "RIAGENDR"), labeldata = a.label, Labels = T,
    includeNA = F, showAllLevels = F, printToggle = F, quote = F, nonnormal = NULL, catDigits = 2, contDigits = 2, n_original = F
  ), "list")
})
