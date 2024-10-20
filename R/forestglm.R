#' @title TableSubgroupGLM: Sub-group analysis table for GLM.
#' @description Sub-group analysis table for GLM.
#' @param formula formula with survival analysis.
#' @param var_subgroup 1 sub-group variable for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param family family, "gaussian" or "binomial" or 'poisson' or 'quasipoisson'
#' @param decimal.estimate Decimal for estimate, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @return Sub-group analysis table.
#' @details This result is used to make forestplot.
#' @examples
#' library(survival)
#' library(dplyr)
#' lung %>%
#'   mutate(
#'     status = as.integer(status == 1),
#'     sex = factor(sex),
#'     kk = factor(as.integer(pat.karno >= 70))
#'   ) -> lung
#' TableSubgroupGLM(status ~ sex, data = lung, family = "binomial")
#' TableSubgroupGLM(status ~ sex, var_subgroup = "kk", data = lung, family = "binomial")
#'
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupGLM(status ~ sex, data = data.design, family = "binomial")
#' TableSubgroupGLM(status ~ sex, var_subgroup = "kk", data = data.design, family = "binomial")
#' @seealso
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[stats]{glm}}
#'  \code{\link[survey]{svyglm}}
#' @rdname TableSubgroupGLM
#' @export
#' @importFrom purrr possibly map_dbl map map2
#' @importFrom dplyr group_split select filter mutate bind_cols
#' @importFrom magrittr %>%
#' @importFrom survey svyglm
#' @importFrom stats glm coefficients anova gaussian quasibinomial poisson quasipoisson qnorm
#' @importFrom utils tail

TableSubgroupGLM <- function(formula, var_subgroup = NULL, var_cov = NULL, data, family = "binomial", decimal.estimate = 2, decimal.percent = 1, decimal.pvalue = 3) {
  . <- NULL

  ### 경고문 ###
  if (length(formula[[3]]) > 1) stop("Formula must contains only 1 independent variable")
  if (any(class(data) == "survey.design" & !is.null(var_subgroup))) {
    if (is.numeric(data$variables[[var_subgroup]])) stop("var_subgroup must categorical.")
  } else if (any(class(data) == "data.frame" & !is.null(var_subgroup))) {
    if (is.numeric(data[[var_subgroup]])) stop("var_subgroup must categorical.")
  }

  ## functions with error
  possible_table <- purrr::possibly(table, NA)
  possible_prop.table <- purrr::possibly(function(x) {
    prop.table(x, 1)[2, ] * 100
  }, NA)
  possible_pv <- purrr::possibly(function(x) {
    summary(x)[["coefficients"]][2, ] %>% tail(1)
  }, NA)
  possible_glm <- purrr::possibly(stats::glm, NA)
  possible_svyglm <- purrr::possibly(survey::svyglm, NA)
  possible_modely <- purrr::possibly(function(x) {
    purrr::map_dbl(x, .[["y"]], 1)
  }, NA)

  xlabel <- setdiff(as.character(formula)[[3]], "+")[1]
  ncoef <- ifelse(any(class(data) == "survey.design"), ifelse(length(levels(data$variables[[xlabel]])) <= 2, 1, length(levels(data$variables[[xlabel]])) - 1),
    ifelse(length(levels(data[[xlabel]])) <= 2, 1, length(levels(data[[xlabel]])) - 1)
  )
  var_cov <- setdiff(var_cov, c(as.character(formula[[3]]), var_subgroup))

  family.svyglm <- gaussian()
  if (family == "binomial") family.svyglm <- quasibinomial()
  if (family == "poisson") family.svyglm <- poisson()
  if (family == "quasipoisson") family.svyglm <- quasipoisson()

  if (is.null(var_subgroup)) {
    ### subgroup 지정 안 한 경우 ###

    # 공변량 있는 경우 formula 변경
    if (!is.null(var_cov)) {
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }

    if (any(class(data) == "survey.design")) {
      model <- survey::svyglm(formula, design = data, x = T, family = family.svyglm)
      # if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
    } else {
      model <- stats::glm(formula, data = data, x = T, family = family)
      # if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
    }

    xlev <- NA
    if (length(model$xlevels[[xlabel]]) > 0) {
      xlev <- model$xlevels[[xlabel]]
    }

    # cc, PE, CI, PV 구하기
    cc <- summary(model)$coefficients
    Point.Estimate <- round(stats::coef(model), decimal.estimate)[2:(1 + ncoef)]
    CI <- round(matrix(c(cc[2:(1 + ncoef), 1] - qnorm(0.975) * cc[2:(1 + ncoef), 2], cc[2:(1 + ncoef), 1] + qnorm(0.975) * cc[2:(1 + ncoef), 2]),
      ncol = 2,
      dimnames = list(paste0(xlabel, xlev[-1]), c("2.5 %", "97.5 %"))
    ), decimal.estimate)

    if (family %in% c("binomial", "poisson", "quasipoisson")) {
      Point.Estimate <- round(exp(stats::coef(model)), decimal.estimate)[2:(1 + ncoef)]
      CI <- round(exp(matrix(c(cc[2:(1 + ncoef), 1] - qnorm(0.975) * cc[2:(1 + ncoef), 2], cc[2:(1 + ncoef), 1] + qnorm(0.975) * cc[2:(1 + ncoef), 2]),
        ncol = 2,
        dimnames = list(paste0(xlabel, xlev[-1]), c("2.5 %", "97.5 %"))
      )), decimal.estimate)
    }

    # if (length(Point.Estimate) > 1){
    #  stop("Formula must contain 1 independent variable only.")
    # }

    # event <- model$y
    # prop <- round(prop.table(table(event, model$x[, 1]), 2)[2, ] * 100, decimal.percent)
    pv <- round(summary(model)$coefficients[2:(1 + ncoef), 4], decimal.pvalue)

    # output 만들기
    if (ncoef < 2) {
      data.frame(Variable = "Overall", Count = length(model$y), Percent = 100, `Point Estimate` = Point.Estimate, Lower = CI[1], Upper = CI[2]) %>%
        dplyr::mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA) -> out

      if (family == "binomial") {
        names(out)[4] <- "OR"
      }
      if (family %in% c("poisson", "quasipoisson")) {
        names(out)[4] <- "RR"
      }
    } else {
      data.frame(
        Variable = c("Overall", rep("", length(Point.Estimate))), Count = c(length(model$y), rep("", length(Point.Estimate))), Percent = c(100, rep("", length(Point.Estimate))),
        Levels = paste0(xlabel, "=", xlev), `Point Estimate` = c("Reference", Point.Estimate), Lower = c("", CI[, 1]), Upper = c("", CI[, 2])
      ) %>%
        dplyr::mutate(`P value` = c("", ifelse(pv >= 0.001, pv, "<0.001")), `P for interaction` = NA) -> out

      if (family == "binomial") {
        names(out)[5] <- "OR"
      }
      if (family %in% c("poisson", "quasipoisson")) {
        names(out)[5] <- "RR"
      }

      rownames(out) <- NULL
    }

    return(out)
  } else if (length(var_subgroup) >= 2 | any(grepl(var_subgroup, formula))) {
    stop("Please input correct subgroup variable.")
  } else {
    ### subgroup 지정 한 경우 ###

    # 공변량 있는 경우 formula 변경
    if (!is.null(var_cov)) {
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }

    if (any(class(data) == "survey.design")) {
      ### survey data인 경우 ###

      data$variables[[var_subgroup]] %>%
        table() %>%
        names() -> label_val
      label_val %>% purrr::map(~ possible_svyglm(formula, design = subset(data, get(var_subgroup) == .), x = TRUE, family = family.svyglm)) -> model
      xlev <- NA
      if (length(survey::svyglm(formula, design = data)$xlevels[[xlabel]]) > 0) {
        xlev <- survey::svyglm(formula, design = data)$xlevels[[xlabel]]
      }

      # pv_int 구하기
      pv_int <- tryCatch(
        {
          pvs_int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), design = data, family = family.svyglm) %>%
            summary() %>%
            coefficients()
          pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
          pv_int
        },
        error = function(e) {
          return(NA)
        }
      )
      # if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")

      data.design <- data
      if (family == "binomial") {
        model.int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), design = data.design, family = quasibinomial())
      } else if (family == "gaussian") {
        model.int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), design = data.design, family = gaussian())
      } else if (family == "poisson") {
        model.int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), design = data.design, family = poisson())
      } else {
        model.int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), design = data.design, family = quasipoisson())
      }

      if (any(is.na(model.int))) {
      } else if (sum(grepl(":", names(coef(model.int)))) > 1) {
        pv_anova <- anova(model.int, method = "Wald")
        pv_int <- round(pv_anova[[length(pv_anova)]][[7]], decimal.pvalue)
      }

      Count <- as.vector(table(data$variables[[var_subgroup]]))
    } else {
      ### survey data가 아닌 경우 ###

      data %>%
        subset(!is.na(get(var_subgroup))) %>%
        group_split(get(var_subgroup)) %>%
        purrr::map(~ possible_glm(formula, data = ., x = T, family = family)) -> model
      data %>%
        subset(!is.na(get(var_subgroup))) %>%
        select(dplyr::all_of(var_subgroup)) %>%
        table() %>%
        names() -> label_val

      xlev <- NA
      if (length(stats::glm(formula, data = data, family = family)$xlevels[[xlabel]]) > 0) {
        xlev <- stats::glm(formula, data = data, family = family)$xlevels[[xlabel]]
      }
      model.int <- possible_glm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), data = data, family = family)

      # pv_int 구하기
      if (any(is.na(model.int))) {
        pv_int <- NA
      } else if (sum(grepl(":", names(coef(model.int)))) > 1) {
        pv_anova <- anova(model.int, test = "Chisq")
        pv_int <- round(pv_anova[nrow(pv_anova), 5], decimal.pvalue)
      } else {
        pvs_int <- model.int %>%
          summary() %>%
          coefficients()
        pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
        # if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      }

      Count <- as.vector(table(data[[var_subgroup]]))
    }

    # PE, CI, PV 구하기
    if (family %in% c("binomial", "poisson", "quasipoisson")) {
      Point.Estimate <- model %>%
        purrr::map("coefficients", default = NA) %>%
        lapply(function(x) {
          est <- rep(NA, max(length(xlev) - 1, 1))
          names(est) <- paste0(xlabel, xlev[-1])

          for (i in names(est)) {
            tryCatch(est[i] <- x[i],
              error = function(e) est[i] <- NA
            )
          }

          round(exp(est), decimal.estimate)
        })

      CI <- model %>%
        purrr::map(function(model) {
          cc0 <- tryCatch(summary(model)$coefficients, error = function(e) {
            return(NA)
          })

          ci0 <- matrix(NA, ncol = 2, nrow = max(length(xlev) - 1, 1), dimnames = list(paste0(xlabel, xlev[-1]), c("2.5 %", "97.5 %")))
          for (i in rownames(ci0)) {
            ci0[i, 1] <- tryCatch(cc0[i, 1] - stats::qnorm(0.975) * cc0[i, 2], error = function(e) {
              return(NA)
            })
            ci0[i, 2] <- tryCatch(cc0[i, 1] + stats::qnorm(0.975) * cc0[i, 2], error = function(e) {
              return(NA)
            })
          }

          round(exp(ci0), decimal.estimate)
        })
    } else {
      Point.Estimate <- model %>%
        purrr::map("coefficients", default = NA) %>%
        lapply(function(x) {
          est <- rep(NA, max(length(xlev) - 1, 1))
          names(est) <- paste0(xlabel, xlev[-1])

          for (i in names(est)) {
            tryCatch(est[i] <- x[i],
              error = function(e) est[i] <- NA
            )
          }

          round(est, decimal.estimate)
        })

      CI <- model %>%
        purrr::map(function(model) {
          cc0 <- tryCatch(summary(model)$coefficients, error = function(e) {
            return(NA)
          })

          ci0 <- matrix(NA, ncol = 2, nrow = max(length(xlev) - 1, 1), dimnames = list(paste0(xlabel, xlev[-1]), c("2.5 %", "97.5 %")))
          for (i in rownames(ci0)) {
            ci0[i, 1] <- tryCatch(cc0[i, 1] - stats::qnorm(0.975) * cc0[i, 2], error = function(e) {
              return(NA)
            })
            ci0[i, 2] <- tryCatch(cc0[i, 1] + stats::qnorm(0.975) * cc0[i, 2], error = function(e) {
              return(NA)
            })
          }

          round(ci0, decimal.estimate)
        })
    }

    pv <- model %>%
      purrr::map(function(model) {
        cc0 <- tryCatch(summary(model)$coefficients, error = function(e) {
          return(NA)
        })

        pvl <- rep(NA, max(length(xlev) - 1, 1))
        names(pvl) <- paste0(xlabel, xlev[-1])
        for (i in names(pvl)) {
          pvl[i] <- tryCatch(cc0[i, 4], error = function(e) {
            return(NA)
          })
        }

        round(pvl, decimal.pvalue)
      })

    # output 만들기
    if (ncoef < 2) {
      data.frame(Variable = paste("  ", label_val), Count = Count, Percent = round(Count / sum(Count) * 100, decimal.percent), `Point Estimate` = unlist(Point.Estimate), Lower = unlist(purrr::map(CI, 1)), Upper = unlist(purrr::map(CI, 2))) %>%
        dplyr::mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA) -> out

      if (family == "binomial") {
        names(out)[4] <- "OR"
      }
      if (family %in% c("poisson", "quasipoisson")) {
        names(out)[4] <- "RR"
      }
    } else {
      data.frame(
        Variable = unlist(lapply(label_val, function(x) c(x, rep("", length(xlev) - 1)))), Count = unlist(lapply(Count, function(x) c(x, rep("", length(xlev) - 1)))), Percent = unlist(lapply(round(Count / sum(Count) * 100, decimal.percent), function(x) c(x, rep("", length(xlev) - 1)))),
        Levels = rep(paste0(xlabel, "=", xlev), length(label_val)), `Point Estimate` = unlist(lapply(Point.Estimate, function(x) c("Reference", x))), Lower = unlist(lapply(CI, function(x) c("", x[, 1]))), Upper = unlist(lapply(CI, function(x) c("", x[, 2])))
      ) %>%
        dplyr::mutate(`P value` = unlist(lapply(pv, function(x) c("", ifelse(x >= 0.001, x, "<0.001")))), `P for interaction` = NA) -> out

      if (family == "binomial") {
        names(out)[5] <- "OR"
      }
      if (family %in% c("poisson", "quasipoisson")) {
        names(out)[5] <- "RR"
      }
    }

    return(rbind(c(var_subgroup, rep(NA, ncol(out) - 2), ifelse(pv_int >= 0.001, pv_int, "<0.001")), out))
  }
}



#' @title TableSubgroupMultiGLM: Multiple sub-group analysis table for GLM.
#' @description Multiple sub-group analysis table for GLM.
#' @param formula formula with survival analysis.
#' @param var_subgroups Multiple sub-group variables for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param family family, "gaussian" or "binomial" or 'poisson' or 'quasipoisson'
#' @param decimal.estimate Decimal for estimate, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @param line Include new-line between sub-group variables, Default: F
#' @return Multiple sub-group analysis table.
#' @details This result is used to make forestplot.
#' @examples
#' library(survival)
#' library(dplyr)
#' lung %>%
#'   mutate(
#'     status = as.integer(status == 1),
#'     sex = factor(sex),
#'     kk = factor(as.integer(pat.karno >= 70)),
#'     kk1 = factor(as.integer(pat.karno >= 60))
#'   ) -> lung
#' TableSubgroupMultiGLM(status ~ sex,
#'   var_subgroups = c("kk", "kk1"),
#'   data = lung, line = TRUE, family = "binomial"
#' )
#'
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupMultiGLM(status ~ sex,
#'   var_subgroups = c("kk", "kk1"),
#'   data = data.design, family = "binomial"
#' )
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}}
#' @rdname TableSubgroupMultiGLM
#' @export
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows


TableSubgroupMultiGLM <- function(formula, var_subgroups = NULL, var_cov = NULL, data, family = "binomial", decimal.estimate = 2, decimal.percent = 1, decimal.pvalue = 3, line = F) {
  . <- NULL
  out.all <- TableSubgroupGLM(formula, var_subgroup = NULL, var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue)

  if (is.null(var_subgroups)) {
    return(out.all)
  } else {
    out.list <- purrr::map(var_subgroups, ~ TableSubgroupGLM(formula, var_subgroup = ., var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue))
    out.list <- purrr::map(out.list, ~ .x %>%
      dplyr::mutate(`P value` = purrr::map_chr(`P value`, ~ if (is.list(.)) as.character(unlist(.)) else as.character(.))))
    if (line) {
      out.newline <- out.list %>% purrr::map(~ rbind(NA, .))
      return(rbind(out.all, out.newline %>% dplyr::bind_rows()))
    } else {
      return(rbind(out.all, out.list %>% dplyr::bind_rows()))
    }
  }
}
