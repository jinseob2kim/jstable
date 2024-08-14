#' @title TableSubgroupCox: Sub-group analysis table for Cox/svycox model.
#' @description Sub-group analysis table for Cox/svycox model.
#' @param formula formula with survival analysis.
#' @param var_subgroup 1 sub-group variable for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param time_eventrate Time for kaplan-meier based event rate calculation, Default = 365 * 3
#' @param decimal.hr Decimal for hazard ratio, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @param cluster Cluster variable for coxph, Default: NULL
#' @param strata Strata variable for coxph, Default: NULL
#' @param weights Weights variable for coxph, Default: NULL
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
#' TableSubgroupCox(Surv(time, status) ~ sex, data = lung, time_eventrate = 100)
#' TableSubgroupCox(Surv(time, status) ~ sex,
#'   var_subgroup = "kk", data = lung,
#'   time_eventrate = 100
#' )
#'
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupCox(Surv(time, status) ~ sex, data = data.design, time_eventrate = 100)
#' TableSubgroupCox(Surv(time, status) ~ sex,
#'   var_subgroup = "kk", data = data.design,
#'   time_eventrate = 100
#' )
#' @seealso
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[survival]{coxph}}
#'  \code{\link[survey]{svycoxph}}
#'  \code{\link[stats]{confint}}
#' @rdname TableSubgroupCox
#' @export
#' @importFrom purrr possibly map_dbl map map2
#' @importFrom dplyr select filter mutate bind_cols
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom survival coxph
#' @importFrom survey svycoxph regTermTest
#' @importFrom stats confint coefficients
#' @importFrom utils tail


TableSubgroupCox <- function(formula, var_subgroup = NULL, var_cov = NULL, data, time_eventrate = 3 * 365, decimal.hr = 2, decimal.percent = 1, decimal.pvalue = 3, cluster = NULL, strata = NULL, weights = NULL) {
  . <- NULL

  ### var_subgroup이 categorical variable이 아닌 경우 중단 ###
  if (any(class(data) == "survey.design" & !is.null(var_subgroup))) {
    if (is.numeric(data$variables[[var_subgroup]])) stop("var_subgroup must categorical.")
    # if (length(levels(data$variables[[as.character(formula[[3]])]])) != 2) stop("Independent variable must have 2 levels.")
  } else if (any(class(data) == "data.frame" & !is.null(var_subgroup))) {
    if (is.numeric(data[[var_subgroup]])) stop("var_subgroup must categorical.")
    # if (length(levels(data[[as.character(formula[[3]])]])) != 2) stop("Independent variable must have 2 levels.")
  }

  ## functions with error
  possible_table <- purrr::possibly(table, NA)
  possible_prop.table <- purrr::possibly(function(x) {
    prop.table(x, 1)[2, ] * 100
  }, NA)
  possible_pv <- purrr::possibly(function(x) {
    summary(x)[["coefficients"]][1, ] %>% tail(1)
  }, NA)
  possible_coxph <- purrr::possibly(survival::coxph, NA)
  possible_svycoxph <- purrr::possibly(survey::svycoxph, NA)
  possible_confint <- purrr::possibly(stats::confint, NA)
  possible_modely <- purrr::possibly(function(x) {
    purrr::map_dbl(x, .[["y"]], 1)
  }, NA)
  possible_rowone <- purrr::possibly(function(x) {
    x[1, ]
  }, NA)

  formula.km <- formula
  var_cov <- setdiff(var_cov, c(as.character(formula[[3]]), var_subgroup))
  xlabel <- setdiff(as.character(formula)[[3]], "+")[1]

  ncoef <- ifelse(any(class(data) == "survey.design"), ifelse(length(levels(data$variables[[xlabel]])) <= 2, 1, length(levels(data$variables[[xlabel]])) - 1),
    ifelse(length(levels(data[[xlabel]])) <= 2 || is.numeric(data[[xlabel]]), 1, length(levels(data[[xlabel]])) - 1)
  )

  if (is.null(var_subgroup)) {
    ### subgroup 지정 안 한 경우 ###
    # 공변량 있는 경우 formula 변경
    if (!is.null(var_cov)) {
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }

    # Strata !is.null인 경우 formula 변경
    if (!is.null(strata)) {
      formula <- as.formula(paste0(deparse(formula), " + ", paste0("strata(", strata, ")")))
    }

    if (any(class(data) == "survey.design")) {
      ### survey data인 경우 ###
      model <- survey::svycoxph(formula, design = data, x = T)
      # if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")

      # KM 구하기(categorical인 경우)
      if (is.numeric(data$variables[[xlabel]])) {
        prop <- NULL
      } else {
        res.kap <- survey::svykm(formula.km, design = data)
        prop <- round(100 * sapply(res.kap, function(x) {
          1 - x[["surv"]][which.min(abs(x[["time"]] - time_eventrate))]
        }), decimal.percent)
        names(prop) <- paste0(xlabel, "=", model$xlevels[[1]])
      }
    } else {
      ### survey data가 아닌 경우 ###
      weights <- if (!is.null(weights)) {
        data[[weights]]
      } else {
        NULL
      }
      if (!is.null(cluster)) {
        formula.1 <- as.formula(
          paste0(deparse(formula), " + ", "cluster(", cluster, ")")
        )
        cc <- substitute(
          survival::coxph(formula.1, data = data, x = T, weights = .weights),
          list(.weights = weights)
        )
        model <- eval(cc)
      } else {
        cc <- substitute(
          survival::coxph(formula, data = data, x = T, weights = .weights),
          list(.weights = weights)
        )
        model <- eval(cc)
      }
      # if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")

      # KM 구하기(categorical인 경우)
      if (is.numeric(data[[xlabel]])) {
        prop <- NULL
      } else {
        res.kap <- survival::survfit(formula.km, data = data)
        res.kap.times <- summary(res.kap, times = time_eventrate, extend = T)
        prop <- round(100 * (1 - res.kap.times[["surv"]]), decimal.percent)
        names(prop) <- paste0(xlabel, "=", model$xlevels[[1]])
      }
      # out.kap <- paste(res.kap.times[["n.event"]], " (", round(100 * (1 - res.kap.times[["surv"]]), decimal.percent), ")", sep = "")
    }

    # PE, CI, PV 구하기
    Point.Estimate <- round(exp(coef(model)), decimal.hr)[1:ncoef]

    # if (length(Point.Estimate) > 1){
    #  stop("Formula must contain 1 independent variable only.")
    # }

    CI <- round(exp(confint(model)[1:ncoef, ]), decimal.hr)
    event <- purrr::map_dbl(model$y, 1) %>% tail(model$n)
    # prop <- round(prop.table(table(event, model$x[, 1]), 2)[2, ] * 100, decimal.percent)
    pv <- round(summary(model)$coefficients[1:ncoef, "Pr(>|z|)"], decimal.pvalue)

    # output 만들기
    if (ncoef < 2) {
      out <- data.frame(Variable = "Overall", Count = model$n, Percent = 100, `Point Estimate` = Point.Estimate, Lower = CI[1], Upper = CI[2], check.names = F) %>%
        mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA)

      if (!is.null(names(prop))) {
        out <- data.frame(Variable = "Overall", Count = model$n, Percent = 100, `Point Estimate` = Point.Estimate, Lower = CI[1], Upper = CI[2], check.names = F) %>%
          cbind(t(prop)) %>%
          mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA)
      }
    } else {
      out <- data.frame(
        Variable = c("Overall", rep("", length(Point.Estimate))), Count = c(model$n, rep("", length(Point.Estimate))), Percent = c(100, rep("", length(Point.Estimate))),
        Levels = paste0(xlabel, "=", model$xlevels[[1]]), `Point Estimate` = c("Reference", Point.Estimate), Lower = c("", CI[, 1]), Upper = c("", CI[, 2]), check.names = F
      ) %>%
        mutate(`P value` = c("", ifelse(pv >= 0.001, pv, "<0.001")), `P for interaction` = NA)

      if (!is.null(names(prop))) {
        out <- data.frame(
          Variable = c("Overall", rep("", length(Point.Estimate))), Count = c(model$n, rep("", length(Point.Estimate))), Percent = c(100, rep("", length(Point.Estimate))),
          Levels = paste0(xlabel, "=", model$xlevels[[1]]), `Point Estimate` = c("Reference", Point.Estimate), Lower = c("", CI[, 1]), Upper = c("", CI[, 2]), KM = prop, check.names = F
        ) %>%
          mutate(`P value` = c("", ifelse(pv >= 0.001, pv, "<0.001")), `P for interaction` = NA)
      }

      rownames(out) <- NULL
    }

    return(out)
  } else if (length(var_subgroup) > 1 | any(grepl(var_subgroup, formula))) {
    stop("Please input correct subgroup variable.")
  } else {
    ### subgroup 지정 한 경우 ###
    # 공변량 있는 경우 formula 변경
    if (!is.null(var_cov)) {
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }

    # Strata !is.null인 경우 formula 변경
    if (!is.null(strata)) {
      formula <- as.formula(paste0(deparse(formula), " + ", paste0("strata(", strata, ")")))
    }

    if (any(class(data) == "survey.design")) {
      ### survey data인 경우 ###
      data$variables[[var_subgroup]] <- factor(data$variables[[var_subgroup]])
      data$variables[[var_subgroup]] %>%
        table() %>%
        names() -> label_val
      label_val %>% purrr::map(~ possible_svycoxph(formula, design = subset(data, get(var_subgroup) == .), x = TRUE)) -> model
      xlev <- survey::svycoxph(formula, design = data)$xlevels

      # pv_int 구하기
      pv_int <- tryCatch(
        {
          pvs_int <- possible_svycoxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), design = data) %>%
            summary() %>%
            coefficients()
          pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
          pv_int
        },
        error = function(e) {
          return(NA)
        }
      )

      ## interaction 여러개인 경우 pv_int 구하기
      model.int <- possible_svycoxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), design = data)

      if (any(is.na(model.int))) {
      } else if (sum(grepl(":", names(coef(model.int)))) > 1) {
        model.int$call$formula <- as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula)))
        pv_anova <- survey::regTermTest(model.int, as.formula(paste0("~", xlabel, ":", var_subgroup)))
        pv_int <- round(pv_anova$p[1], decimal.pvalue)
      }

      # KM 구하기(categorical인 경우만)
      if (!is.numeric(data$variables[[xlabel]])) {
        prop <- NULL
        try(
          {
            res.kap <- purrr::map(label_val, ~ survey::svykm(formula.km, design = subset(data, get(var_subgroup) == .)))
            mkz <- function(reskap) {
              round(100 * sapply(reskap, function(x) {
                1 - x[["surv"]][which.min(abs(x[["time"]] - time_eventrate))]
              }), decimal.percent)
            }
            prop <- purrr::map(res.kap, mkz) %>%
              dplyr::bind_cols() %>%
              t()
            # prop <- purrr::map(res.kap, ~round(100 * sapply(., function(x){1 - x[["surv"]][which.min(abs(x[["time"]] - time_eventrate))]}), decimal.percent))
            colnames(prop) <- paste0(xlabel, "=", xlev[[1]])
          },
          silent = TRUE
        )
      } else {
        prop <- NULL
      }
    } else {
      ### survey data가 아닌 경우 ###
      weights_option <- if (!is.null(weights)) TRUE else FALSE
      data[[var_subgroup]] <- factor(data[[var_subgroup]])
      # Coxph 함수를 각 subgroup에 대해 적용시키기 위한 함수
      run_coxph <- function(subgroup_var, subgroup_value, data, formula, weights_option) {
        subset_data <- data[data[[subgroup_var]] == subgroup_value, ]

        if (nrow(subset_data) == 0) {
          return(NULL)
        }

        subset_weights <- if (weights_option) {
          as.numeric(as.character(subset_data[[weights]]))
        } else {
          NULL
        }
        cc <- substitute(
          survival::coxph(formula, data = subset_data, x = T, weights = .weights),
          list(.weights = subset_weights)
        )
        eval(cc)
      }

      if (is.null(cluster)) {
        model <- sapply(var_subgroup, function(var) {
          if (is.factor(data[[var]])) {
            unique_vals <- levels(data[[var]])
          } else {
            unique_vals <- sort(setdiff(unique(data[[var]]), NA))
          }
          lapply(unique_vals, function(value) {
            result <- run_coxph(var, value, data, formula, weights_option)
          })
        })
      } else {
        formula <- as.formula(paste0(deparse(formula), " + ", "cluster(", cluster, ")"))

        model <- sapply(var_subgroup, function(var) {
          if (is.factor(data[[var]])) {
            unique_vals <- levels(data[[var]])
          } else {
            unique_vals <- sort(setdiff(unique(data[[var]]), NA))
          }
          lapply(unique_vals, function(value) {
            result <- run_coxph(var, value, data, formula, weights_option)
          })
        })
      }
      weights <- if (!is.null(weights)) {
        data[[weights]]
      } else {
        NULL
      }

      data %>%
        filter(!is.na(get(var_subgroup))) %>%
        select(dplyr::all_of(var_subgroup)) %>%
        table() %>%
        names() -> label_val
      xlev <- survival::coxph(formula, data = data)$xlevels


      # strata만 공식에 추가하는 경우 P for interaction에서 <NA>가 나타나는 문제가 있어 수정
      if (is.null(cluster) & is.null(weights) & !is.null(strata)) {
        model.int <- possible_coxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), data = data)
      } else {
        model.int <- tryCatch(eval(substitute(coxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), data = data, weights = .weights), list(.weights = weights))), error = function(e) NA)
        # if (!is.null(cluster)) {
        #   model.int <- eval(substitute(possible_coxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), data = data, weights = .weights), list(.weights = weights)))
        # } else {
        #   model.int <- tryCatch(eval(substitute(coxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), data = data, weights = .weights), list(.weights = weights))), error = function(e) NA)
        # }
      }


      # KM 구하기(categorical인 경우만)
      if (!is.numeric(data[[xlabel]])) {
        prop <- NULL
        try({
          res.kap.times <- data %>%
            filter(!is.na(get(var_subgroup))) %>%
            split(.[[var_subgroup]]) %>%
            purrr::map(~ survival::survfit(formula.km, data = .)) %>%
            purrr::map(~ summary(., times = time_eventrate, extend = T))

          prop <- matrix(nrow = length(res.kap.times), ncol = length(xlev[[1]]))
          colnames(prop) <- paste0(xlabel, "=", xlev[[1]])
          rownames(prop) <- names(res.kap.times)

          sub_xlev <- data %>%
            filter(!is.na(get(var_subgroup))) %>%
            split(.[[var_subgroup]]) %>%
            lapply(function(x) {
              sort(setdiff(unique(x[[xlabel]]), NA))
            })

          for (i in rownames(prop)) {
            if (length(sub_xlev[[i]]) == 1) {
              # prop[i, paste0(xlabel, "=", sub_xlev[[i]])] <- res.kap.times[["0"]][["surv"]]
              prop[i, ] <- res.kap.times[["0"]][["surv"]]
            } else if (length(sub_xlev[[i]]) > 1) {
              surv.df <- data.frame(res.kap.times[[i]][c("strata", "surv")])
              for (j in colnames(prop)) {
                tryCatch(prop[i, j] <- surv.df[surv.df$strata == j, "surv"],
                  error = function(e) {
                    prop[i, j] <- NA
                  }
                )
              }
            }
          }

          prop <- round(100 * (1 - prop), decimal.percent)
        }, silent = )
      } else {
        prop <- NULL
      }

      # pv_int 구하기
      if (any(is.na(model.int))) {
        pv_int <- NA
      } else if (sum(grepl(":", names(coef(model.int)))) == 1) {
        pvs_int <- model.int %>%
          summary() %>%
          coefficients()
        pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
        # if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      } else {
        model.int$call$formula <- as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula)))
        model.int$call$data <- as.name("data")
        pv_anova <- tryCatch(anova(model.int), error = function(e) NA)
        if (is.logical(pv_anova) & !is.null(cluster)) {
          warning("Warning: Anova test is not available for cluster data. So Interaction P value is NA when 3 and more categorical subgroup variable.")
        }
        pv_int <- tryCatch(round(pv_anova[nrow(pv_anova), 4], decimal.pvalue), error = function(e) NA)
      }
    }

    # Count, PE, CI, PV 계산
    model %>% purrr::map_dbl("n", .default = NA) -> Count

    if (ncoef < 2) {
      model %>%
        purrr::map("coefficients", .default = NA) %>%
        lapply(function(x) {
          round(exp(x[1:ncoef]), decimal.hr)
        }) %>%
        unlist() -> Point.Estimate

      model %>%
        lapply(function(x) {
          tryCatch(
            {
              round(exp(stats::confint(x)[1, ]), decimal.hr)
            },
            error = function(e) {
              return(matrix(nrow = 1, ncol = 2, dimnames = list(paste0(xlabel, xlev[[1]][-1]), c("2.5 %", "97.5 %"))))
            }
          )
        }) %>%
        Reduce(rbind, .) -> CI

      model %>%
        purrr::map(possible_pv) %>%
        purrr::map_dbl(~ round(., decimal.pvalue)) -> pv
    } else {
      model %>%
        purrr::map("coefficients", .default = NA) %>%
        lapply(function(x) {
          round(exp(x[1:ncoef]), decimal.hr)
        }) -> Point.Estimate

      model %>%
        purrr::map(possible_confint) %>%
        lapply(function(x) {
          tryCatch(
            {
              round(exp(x[1:ncoef, ]), decimal.hr)
            },
            error = function(e) {
              return(matrix(nrow = length(xlev[[1]]) - 1, ncol = 2, dimnames = list(paste0(xlabel, xlev[[1]][-1]), c("2.5 %", "97.5 %"))))
            }
          )
        }) -> CI

      model %>%
        lapply(function(x) {
          tryCatch(
            {
              round(summary(x)$coefficients[1:ncoef, 5], decimal.pvalue)
            },
            error = function(e) {
              return(rep(NA, length(xlev[[1]]) - 1))
            }
          )
        }) -> pv
    }

    # output 만들기
    if (ncoef < 2) {
      out <- data.frame(Variable = paste("  ", label_val), Count = Count, Percent = round(Count / sum(Count) * 100, decimal.percent), `Point Estimate` = Point.Estimate, Lower = CI[, 1], Upper = CI[, 2], check.names = F, row.names = NULL) %>%
        mutate(`P value` = unlist(ifelse(pv >= 0.001, pv, "<0.001")), `P for interaction` = NA)

      if (!is.null(prop)) {
        out <- data.frame(Variable = paste("  ", label_val), Count = Count, Percent = round(Count / sum(Count) * 100, decimal.percent), `Point Estimate` = Point.Estimate, Lower = CI[, 1], Upper = CI[, 2], check.names = F, row.names = NULL) %>%
          cbind(prop) %>%
          mutate(`P value` = unlist(ifelse(pv >= 0.001, pv, "<0.001")), `P for interaction` = NA)
      }

      rownames(out) <- NULL

      return(rbind(c(var_subgroup, rep(NA, ncol(out) - 2), ifelse(pv_int >= 0.001, pv_int, "<0.001")), out))
    } else {
      out <- data.frame(
        Variable = unlist(lapply(label_val, function(x) c(x, rep("", length(xlev[[1]]) - 1)))), Count = unlist(lapply(Count, function(x) c(x, rep("", length(xlev[[1]]) - 1)))), Percent = unlist(lapply(round(Count / sum(Count) * 100, decimal.percent), function(x) c(x, rep("", length(xlev[[1]]) - 1)))),
        Levels = rep(paste0(xlabel, "=", xlev[[1]]), length(label_val)), `Point Estimate` = unlist(lapply(Point.Estimate, function(x) c("Reference", x))), Lower = unlist(lapply(CI, function(x) c("", x[, 1]))), Upper = unlist(lapply(CI, function(x) c("", x[, 2]))), check.names = F
      ) %>%
        mutate(`P value` = unlist(lapply(pv, function(x) c("", ifelse(x >= 0.001, x, "<0.001")))), `P for interaction` = NA)

      if (!is.null(prop)) {
        out <- data.frame(
          Variable = unlist(lapply(label_val, function(x) c(x, rep("", length(xlev[[1]]) - 1)))), Count = unlist(lapply(Count, function(x) c(x, rep("", length(xlev[[1]]) - 1)))), Percent = unlist(lapply(round(Count / sum(Count) * 100, decimal.percent), function(x) c(x, rep("", length(xlev[[1]]) - 1)))),
          Levels = rep(paste0(xlabel, "=", xlev[[1]]), length(label_val)), `Point Estimate` = unlist(lapply(Point.Estimate, function(x) c("Reference", x))), Lower = unlist(lapply(CI, function(x) c("", x[, 1]))), Upper = unlist(lapply(CI, function(x) c("", x[, 2]))), check.names = F
        ) %>%
          mutate(KM = as.vector(t(prop)), `P value` = unlist(lapply(pv, function(x) c("", ifelse(x >= 0.001, x, "<0.001")))), `P for interaction` = NA)
      }

      rownames(out) <- NULL

      return(rbind(c(var_subgroup, rep(NA, ncol(out) - 2), ifelse(pv_int >= 0.001, pv_int, "<0.001")), out))
    }
  }
}

#' @title TableSubgroupMultiCox: Multiple sub-group analysis table for Cox/svycox model.
#' @description Multiple sub-group analysis table for Cox/svycox model.
#' @param formula formula with survival analysis.
#' @param var_subgroups Multiple sub-group variables for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param time_eventrate Time for kaplan-meier based event rate calculation, Default = 365 * 3
#' @param decimal.hr Decimal for hazard ratio, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @param line Include new-line between sub-group variables, Default: F
#' @param cluster Cluster variable for coxph, Default: NULL
#' @param strata Strata variable for coxph, Default: NULL
#' @param weights Weights variable for coxph, Default: NULL
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
#' TableSubgroupMultiCox(Surv(time, status) ~ sex,
#'   var_subgroups = c("kk", "kk1"),
#'   data = lung, time_eventrate = 100, line = TRUE
#' )
#'
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupMultiCox(Surv(time, status) ~ sex,
#'   var_subgroups = c("kk", "kk1"),
#'   data = data.design, time_eventrate = 100
#' )
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}}
#' @rdname TableSubgroupMultiCox
#' @export
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows

TableSubgroupMultiCox <- function(formula, var_subgroups = NULL, var_cov = NULL, data, time_eventrate = 3 * 365, decimal.hr = 2, decimal.percent = 1, decimal.pvalue = 3, line = F, cluster = NULL, strata = NULL, weights = NULL) {
  . <- NULL
  xlabel <- setdiff(as.character(formula)[[3]], "+")[1]

  out.all <- TableSubgroupCox(formula, var_subgroup = NULL, var_cov = var_cov, data = data, time_eventrate = time_eventrate, decimal.hr = decimal.hr, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, cluster = cluster, strata = strata, weights = weights)
  out.all <- dplyr::mutate_all(out.all, as.character)

  if (is.null(var_subgroups)) {
    return(out.all)
  } else {
    out.list <- lapply(var_subgroups, function(subgroup) {
      TableSubgroupCox(formula, var_subgroup = subgroup, var_cov = var_cov, data = data, time_eventrate = time_eventrate, decimal.hr = decimal.hr, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, cluster = cluster, strata = strata, weights = weights)
    })

    # out.list <- purrr::map(var_subgroups, ~ TableSubgroupCox(formula, var_subgroup = ., var_cov = var_cov, data = data, time_eventrate = time_eventrate, decimal.hr = decimal.hr, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, cluster = cluster, weights = weights_vec))
    if (line) {
      out.newline <- out.list %>% purrr::map(~ rbind(NA, .))
      result <- bind_rows(out.all, out.newline %>% dplyr::bind_rows() %>% dplyr::mutate_all(as.character))
      rownames(result) <- c(xlabel, 1:(nrow(result) - 1))
      return(result)
    } else {
      result <- bind_rows(out.all, out.list %>% dplyr::bind_rows() %>% dplyr::mutate_all(as.character))
      rownames(result) <- c(xlabel, 1:(nrow(result) - 1))
      return(result)
    }
  }
}
