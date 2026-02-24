#' @title TableSubgroupGLM: Sub-group analysis table for GLM and GLMM(lme4 package).
#' @description Sub-group analysis table for GLM.
#' @param formula formula with survival analysis.
#' @param var_subgroup 1 sub-group variable for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param family family, "gaussian" or "binomial" or 'poisson' or 'quasipoisson'
#' @param decimal.estimate Decimal for estimate, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @param labeldata Label info, made by `mk.lev` function, Default: NULL
#' @param count_by  Variable name to stratify counts by (string). Default: NULL.
#' @param event     If `TRUE`, show counts/metrics instead of only model estimates. Default: FALSE.
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
#' @importFrom stats glm coefficients anova gaussian quasibinomial poisson quasipoisson qnorm terms
#' @importFrom utils tail
#' @importFrom lme4 glmer fixef
#' @importFrom car Anova

TableSubgroupGLM <- function(formula, var_subgroup = NULL, var_cov = NULL, data, family = "binomial", decimal.estimate = 2, decimal.percent = 1, decimal.pvalue = 3, labeldata = NULL, count_by = NULL, event = FALSE) {
  . <- variable <- var_label <- val_label <- level <- NULL
  ### 경고문 ###
  if (is.null(count_by) && !(event)){
    fixed_effects <- attr(terms(as.formula(formula)), "term.labels")
    fixed_effects <- Filter(function(term) {
      !grepl("\\|", term)
    }, fixed_effects)
    if (length(fixed_effects) > 1) stop("Formula must contain only 1 independent variable")
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
    possible_glmer <- purrr::possibly(lme4::glmer, NA)
    possible_lmertest <- purrr::possibly(lmerTest::lmer, NA)
    xlabel <- setdiff(as.character(formula)[[3]], "+")[1]
    ncoef <- ifelse(any(class(data) == "survey.design"), ifelse(length(levels(data$variables[[xlabel]])) <= 2, 1, length(levels(data$variables[[xlabel]])) - 1),
                    ifelse(length(levels(data[[xlabel]])) <= 2, 1, length(levels(data[[xlabel]])) - 1)
    )
    var_cov <- setdiff(var_cov, c(as.character(formula[[3]]), var_subgroup))
    is_mixed_effect <- grepl("\\|", deparse(formula))
    family.svyglm <- gaussian()
    if (family == "binomial") family.svyglm <- quasibinomial()
    if (family == "poisson") family.svyglm <- poisson()
    if (family == "quasipoisson") family.svyglm <- quasipoisson()
    ### subgroup 지정 안 한 경우 ###
    if (is.null(var_subgroup)) {
      # 공변량 있는 경우 formula 변경
      if (!is.null(var_cov)) {
        formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
      }
      if (is_mixed_effect) {
        if (family == "gaussian") {
          model <- lmerTest::lmer(formula, data = data)
        } else {
          model <- lme4::glmer(formula, data = data, family = family)
        }
        xlev <- NA
        xlabel <- attr(terms(formula), "term.labels")[1]
        xlev <- tryCatch(
          {
            levels(data[[xlabel]])
          },
          error = function(e) {
            warning("Failed to retrieve factor levels for fixed effect.")
            NA
          }
        )
        cc <- summary(model)$coefficients
        mo_sum <- summary(model)
        ncoef <- nrow(cc) - 1
        Point.Estimate <- if (family %in% c("binomial", "poisson", "quasipoisson")) {
          round(exp(lme4::fixef(model)), decimal.estimate)[2:(1 + ncoef)]
        } else {
          round(lme4::fixef(model), decimal.estimate)[2:(1 + ncoef)]
        }
        
        CI <- tryCatch(
          {
            ci_bounds <- confint(model, parm = "beta_", level = 0.95)
            if (family %in% c("binomial", "poisson", "quasipoisson")) {
              round(exp(ci_bounds), decimal.estimate)[-1, ]
            } else {
              round(ci_bounds, decimal.estimate)[-1, ]
            }
          },
          error = function(e) {
            cc <- summary(model)$coefficients
            matrix(
              c(
                cc[2:(1 + ncoef), 1] - qnorm(0.975) * cc[2:(1 + ncoef), 2],
                cc[2:(1 + ncoef), 1] + qnorm(0.975) * cc[2:(1 + ncoef), 2]
              ),
              ncol = 2,
              dimnames = list(rownames(cc)[2:(1 + ncoef)], c("2.5 %", "97.5 %"))
            ) %>%
              {
                if (family %in% c("binomial", "poisson", "quasipoisson")) round(exp(.), decimal.estimate) else round(., decimal.estimate)
              }
          }
        )
        
        # P-value 계산
        pv <- tryCatch(
          {
            round(cc[2:(1 + ncoef), grep("Pr", colnames(cc), value = TRUE)], decimal.pvalue)
          },
          error = function(e) {
            warning("P-value computation failed. Returning NA.")
            rep(NA, ncoef)
          }
        )
        response_var <- as.character(formula(model)[[2]])
        response_data <- data[[response_var]]
        event_count <- sum(response_data)
        used_data <- model@frame
        total_count <- nrow(used_data)
        model <- list()
        model$y <- c(event_count, rep("", total_count - 1))
      } else {
        if (any(class(data) == "survey.design")) {
          model <- survey::svyglm(formula, design = data, x = T, family = family.svyglm)
          # if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
        } else {
          model <- stats::glm(formula, data = data, x = T, family = family)
          
          # if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
        }
        
        xlev <- NA
        
        if (!is.null(model$xlevels[[xlabel]])) {
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
      }
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
        
        if (!is.null(labeldata)) {
          out$Levels <- paste0(labeldata[variable == xlabel, var_label[1]], "=", sapply(xlev, function(x) {
            labeldata[variable == xlabel & level == x, val_label]
          }))
        }
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
      
      if (!is_mixed_effect) {
        if (any(class(data) == "survey.design")) {
          ### survey data인 경우 ###
          vars_in_formula <- all.vars(as.formula(formula))
          complete_data <- data$variables[complete.cases(dplyr::select(data$variables, dplyr::all_of(vars_in_formula))), ]
          data$variables[[var_subgroup]] %>%
            table() %>%
            names() -> label_val
          label_val %>% purrr::map(~ possible_svyglm(formula, design = subset(data, get(var_subgroup) == .), x = TRUE, family = family.svyglm)) -> model
          xlev <- NA
          if (length(survey::svyglm(formula, design = data)$xlevels[[xlabel]]) > 0) {
            xlev <- survey::svyglm(formula, design = data)$xlevels[[xlabel]]
          }
          
          # pv_int 구하기
          # pv_int <- tryCatch(
          #   {
          #     pvs_int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula))), design = data, family = family.svyglm) %>%
          #       summary() %>%
          #       coefficients()
          #     pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
          #     pv_int
          #   },
          #   error = function(e) {
          #     return(NA)
          #   }
          # )
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
          
          model.int$call[[2]] <- as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep = ""), deparse(formula)))
          model.int$call[[3]] <- data.design
          #model.int$call[[4]] <- gaussian()
          model.int$call[[4]] <- family.svyglm
          # print(model.int$call)
          # print(family)
          # print(family.svyglm)
          # print(any(is.na(model.int)))
          # if (any(is.na(model.int))) {
          # } else if (sum(grepl(":", names(coef(model.int)))) > 1) {
          #   pv_anova <- anova(model.int, method = "Wald")
          #   pv_int <- round(pv_anova[[length(pv_anova)]][[7]], decimal.pvalue)
          # }
          
          if (is.logical(model.int)) {
            pv_int <- NA
          } else if (sum(grepl(":", names(coef(model.int)))) > 1) {
            pv_anova <- anova(model.int, method = "Wald")
            pv_int <- round(pv_anova[[length(pv_anova)]]$p[1], decimal.pvalue)
            #pv_int <- round(pv_anova[nrow(pv_anova), 5], decimal.pvalue)
          } else {
            pvs_int <- model.int %>%
              summary() %>%
              coefficients()
            pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
            # if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
          }
          
          Count <- as.vector(table(complete_data[[var_subgroup]]))
        } else {
          vars_in_formula <- all.vars(as.formula(formula))
          complete_data <- data[complete.cases(dplyr::select(data, dplyr::all_of(vars_in_formula))), ]
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
          
          Count <- as.vector(table(complete_data[[var_subgroup]]))
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
      }
      
      if (is_mixed_effect) {
        vars_in_formula <- all.vars(as.formula(formula))
        complete_data <- data[complete.cases(dplyr::select(data, dplyr::all_of(vars_in_formula))), ]
        model <- data %>%
          subset(!is.na(get(var_subgroup))) %>%
          group_split(get(var_subgroup)) %>%
          purrr::map(~ if (family == "gaussian") {
            possible_lmertest(formula, data = ., REML = FALSE)
          } else {
            possible_glmer(formula, data = ., family = family)
          })
        
        label_val <- data %>%
          subset(!is.na(get(var_subgroup))) %>%
          select(dplyr::all_of(var_subgroup)) %>%
          table() %>%
          names()
        
        xlabel <- attr(terms(formula), "term.labels")[1]
        xlev <- NA
        xlev <- tryCatch(
          {
            levels(data[[xlabel]])
          },
          error = function(e) {
            warning("Failed to retrieve factor levels for fixed effect.")
            NA
          }
        )
        
        # Interaction model for overall interaction p-value
        model.int <- tryCatch(
          if (length(xlev) > 1) {
            if (family == "gaussian") {
              possible_lmertest(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))),
                                data = data, REML = FALSE
              )
            } else {
              possible_glmer(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))),
                             data = data, family = family
              )
            }
          } else {
            NA
          },
          error = function(e) NA
        )
        # Calculate pv_int
        if (!inherits(model.int, "merMod")) { # Check if model is invalid or not an S4 object
          pv_int <- NA
        } else {
          coef_names <- names(lme4::fixef(model.int))
          if (sum(grepl(":", coef_names)) > 1) { # Check for more than one interaction term
            pv_anova <- car::Anova(model.int)
            interaction_row <- grep(":", rownames(pv_anova), value = TRUE)
            pr_row <- grep("Pr", colnames(pv_anova), value = TRUE)
            pv_int <- round(pv_anova[interaction_row, pr_row], decimal.pvalue)
          } else {
            pvs_int <- summary(model.int)$coefficients # Access coefficients table
            pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue) # Extract p-value for interaction
            formula_string <- deparse(formula(model.int)) # Extract the formula of the model
          }
        }
        # Calculate Count (subgroup sizes)
        Count <- as.vector(table(complete_data[[var_subgroup]]))
        
        # Calculate Point Estimate (PE), Confidence Interval (CI), and P-value (PV)
        if (family %in% c("binomial", "poisson", "quasipoisson")) {
          # For binomial/Poisson families
          Point.Estimate <- model %>%
            purrr::map(~ tryCatch(
              {
                coef <- lme4::fixef(.)
                if (is.null(coef)) {
                  return(rep(NA, max(length(xlev) - 1, 1)))
                }
                est <- rep(NA, max(length(xlev) - 1, 1))
                names(est) <- paste0(xlabel, xlev[-1])
                for (i in names(est)) {
                  tryCatch(est[i] <- coef[i], error = function(e) est[i] <- NA)
                }
                round(exp(est), decimal.estimate)
              },
              error = function(e) rep(NA, max(length(xlev) - 1, 1))
            ))
          
          CI <- model %>%
            purrr::map(function(model) {
              cc0 <- tryCatch(summary(model)$coefficients, error = function(e) NA)
              ci0 <- matrix(NA,
                            ncol = 2, nrow = max(length(xlev) - 1, 1),
                            dimnames = list(paste0(xlabel, xlev[-1]), c("2.5 %", "97.5 %"))
              )
              for (i in rownames(ci0)) {
                ci0[i, 1] <- tryCatch(cc0[i, 1] - qnorm(0.975) * cc0[i, 2], error = function(e) NA)
                ci0[i, 2] <- tryCatch(cc0[i, 1] + qnorm(0.975) * cc0[i, 2], error = function(e) NA)
              }
              round(exp(ci0), decimal.estimate)
            })
        } else {
          # For Gaussian families
          Point.Estimate <- model %>%
            purrr::map(~ tryCatch(
              {
                # 고정 효과 추출
                coef <- lme4::fixef(.)
                if (is.null(coef)) {
                  return(rep(NA, max(length(xlev) - 1, 1)))
                }
                
                # Point Estimate 계산
                est <- rep(NA, max(length(xlev) - 1, 1))
                names(est) <- paste0(xlabel, xlev[-1])
                for (i in names(est)) {
                  tryCatch(est[i] <- coef[i], error = function(e) est[i] <- NA)
                }
                round(est, decimal.estimate) # Gaussian에서는 exp() 필요 없음
              },
              error = function(e) rep(NA, max(length(xlev) - 1, 1))
            ))
          
          CI <- model %>%
            purrr::map(function(model) {
              cc0 <- tryCatch(summary(model)$coefficients, error = function(e) NA)
              ci0 <- matrix(NA,
                            ncol = 2, nrow = max(length(xlev) - 1, 1),
                            dimnames = list(paste0(xlabel, xlev[-1]), c("2.5 %", "97.5 %"))
              )
              for (i in rownames(ci0)) {
                ci0[i, 1] <- tryCatch(cc0[i, 1] - qnorm(0.975) * cc0[i, 2], error = function(e) NA)
                ci0[i, 2] <- tryCatch(cc0[i, 1] + qnorm(0.975) * cc0[i, 2], error = function(e) NA)
              }
              round(ci0, decimal.estimate)
            })
        }
        
        # Extract p-values for each subgroup
        pv <- model %>%
          purrr::map(function(model) {
            cc0 <- tryCatch(summary(model)$coefficients, error = function(e) NA)
            pvl <- rep(NA, max(length(xlev) - 1, 1))
            names(pvl) <- paste0(xlabel, xlev[-1])
            p_col <- grep("Pr", colnames(cc0), value = TRUE)
            if (length(p_col) == 0) {
              return(round(pvl, decimal.pvalue))
            }
            for (i in names(pvl)) {
              pvl[i] <- tryCatch(cc0[i, p_col], error = function(e) NA)
            }
            round(pvl, decimal.pvalue)
          })
      }
      # output 만들기
      if (ncoef < 2) {
        data.frame(Variable = paste("  ", label_val), Count = Count, Percent = round(Count / sum(Count) * 100, decimal.percent), `Point Estimate` = unlist(Point.Estimate), Lower = unlist(purrr::map(CI, 1)), Upper = unlist(purrr::map(CI, 2))) %>%
          dplyr::mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA) -> out
        
        if (!is.null(labeldata)) {
          out$Variable <- paste0(" ", sapply(label_val, function(x) {
            labeldata[variable == var_subgroup & level == x, val_label]
          }))
        }
        
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
        
        if (!is.null(labeldata)) {
          out$Variable <- unlist(lapply(label_val, function(x) c(labeldata[variable == var_subgroup & level == x, val_label], rep("", length(xlev) - 1))))
          out$Levels <- rep(paste0(labeldata[variable == xlabel, var_label[1]], "=", sapply(xlev, function(x) {
            labeldata[variable == xlabel & level == x, val_label]
          })), length(label_val))
        }
      }
      
      var_subgroup_rev <- var_subgroup
      if (!is.null(labeldata)) {
        var_subgroup_rev <- labeldata[variable == var_subgroup, var_label[1]]
      }
      
      
      return(rbind(c(var_subgroup_rev, rep(NA, ncol(out) - 2), ifelse(pv_int >= 0.001, pv_int, "<0.001")), out))
    }
  }
  if ((event) && is.null(count_by)) {
    original_output <- TableSubgroupGLM(formula = formula, var_subgroup = var_subgroup, var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, labeldata = labeldata, count_by = count_by, event = FALSE)
    count_output <- count_event_by_glm(formula = formula, data = data, count_by_var = count_by, var_subgroup = var_subgroup, decimal.percent = decimal.percent, family = family)
    if (!is.null(var_subgroup)) {
      for (i in 1:nrow(original_output)) {
        clean_variable <- trimws(original_output$Variable[i])
        if (clean_variable != "" && clean_variable %in% count_output[[var_subgroup]]) {
          match_row <- which(count_output[[var_subgroup]] == clean_variable)
          if (length(match_row) > 0) {
            original_output$Count[i] <- count_output$Metric[match_row]
          }
        }
      }
      return(original_output)
    } else {
      original_output$Count[1] <- count_output$Metric[1]
      return(original_output)
    }
  }
  if ((event) && !is.null(count_by)) {
    original_output <- TableSubgroupGLM(formula = formula, var_subgroup = var_subgroup, var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, labeldata = labeldata, count_by = NULL, event = FALSE)
    count_output <- count_event_by_glm(formula = formula, data = data, count_by_var = count_by, var_subgroup = var_subgroup, decimal.percent = decimal.percent, family = family)
    if (inherits(data, "survey.design")) {
      data <- data$variables
    } else {
      data <- data
    }
    count_by_levels <- sort(unique(data[[count_by]]), decreasing = TRUE)
    if (!is.null(labeldata)) {
      count_by_levels <- sapply(count_by_levels, function(x) {
        label <- labeldata[labeldata$variable == count_by & labeldata$level == x, "val_label"]
        if (length(label) > 0) {
          return(label)
        } else {
          return(x)
        }
      })
      count_output[[count_by]] <- sapply(count_output[[count_by]], function(x) {
        label <- labeldata[labeldata$variable == count_by & labeldata$level == x, "val_label"]
        if (length(label) > 0) {
          return(label)
        } else {
          return(x)
        }
      })
    }
    if (!is.null(var_subgroup)) {
      subgroup_levels <- unique(data[[var_subgroup]])
      for (countlevel in count_by_levels) {
        event_rate_col <- paste0("Count(", count_by, "=", countlevel, ")")
        original_output <- original_output %>%
          tibble::add_column(!!event_rate_col := NA, .after = "Count")
        for (sub_level in subgroup_levels) {
          level_label <- if (!is.null(labeldata)) {
            label <- as.character(labeldata[labeldata$variable == var_subgroup & labeldata$level == sub_level, "val_label"])[1]
          } else {
            sub_level
          }
          value_to_insert <- count_output[count_output[[count_by]] == countlevel & count_output[[var_subgroup]] == sub_level, "Metric"]
          value_to_insert <- value_to_insert[!is.na(value_to_insert)]
          if (length(value_to_insert) > 0) {
            if (!is.na(value_to_insert[1])) {
              original_output[[event_rate_col]][trimws(original_output[["Variable"]]) == level_label] <- value_to_insert[1]
            } else {
              original_output[[event_rate_col]][trimws(original_output[["Variable"]]) == level_label] <- ""
            }
          } else {
            original_output[[event_rate_col]][trimws(original_output[["Variable"]]) == level_label] <- ""
          }
        }
      }
      count_output_sub <- count_event_by_glm(formula = formula, data = data,
                                             count_by_var = NULL,
                                             var_subgroup = var_subgroup,
                                             decimal.percent = decimal.percent, family = family)
      if (!is.null(labeldata)) {
        count_output_sub[[var_subgroup]] <- sapply(
          count_output_sub[[var_subgroup]],
          function(x) {
            lab <- labeldata[labeldata$variable == var_subgroup &
                               labeldata$level    == x,
                             "val_label"]
            if (length(lab) > 0) lab else x
          }
        )
      }
      for (i in seq_len(nrow(original_output))) {
        clean_variable <- trimws(original_output$Variable[i])
        if (clean_variable %in% count_output_sub[[var_subgroup]]) {
          match_row <- which(count_output_sub[[var_subgroup]] == clean_variable)[1]
          original_output$Count[i] <- count_output_sub$Metric[match_row]
        }
      }
      return(original_output)
    } else {
      for (countlevel in count_by_levels) {
        event_rate_col <- paste0("Count(", count_by, "=", countlevel, ")")
        original_output <- original_output %>%
          tibble::add_column(!!event_rate_col := NA, .after = "Count")
        value_to_insert <- count_output[count_output[[count_by]] == countlevel, "Metric"]
        value_to_insert <- value_to_insert[!is.na(value_to_insert)]
        
        original_output[[event_rate_col]][trimws(original_output[["Variable"]]) == "Overall"] <- value_to_insert[1]
      }
      count_output <- count_event_by_glm(formula = formula, data = data, count_by_var = NULL, var_subgroup = var_subgroup, decimal.percent = decimal.percent, family = family)
      original_output$Count[1] <- count_output$Metric[1]
      return(original_output)
    }
  }
  if (!(event) && !is.null(count_by)) {
    original_output <- TableSubgroupGLM(formula = formula, var_subgroup = var_subgroup, var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, labeldata = labeldata, count_by = NULL, event = event)
    count_output <- count_event_by_glm(formula = formula, data = data, count_by_var = count_by, var_subgroup = var_subgroup, decimal.percent = decimal.percent, family = family)
    if (inherits(data, "survey.design")) {
      data <- data$variables
    } else {
      data <- data
    }
    count_by_levels <- sort(unique(data[[count_by]]), decreasing = TRUE)
    if (!is.null(labeldata)) {
      # count_by_levels와 count_output의 count_by 값을 라벨로 변환
      count_by_levels <- sapply(count_by_levels, function(x) {
        label <- labeldata[labeldata$variable == count_by & labeldata$level == x, "val_label"]
        if (length(label) > 0) {
          return(label)
        } else {
          return(x)
        }
      })
      
      count_output[[count_by]] <- sapply(count_output[[count_by]], function(x) {
        label <- labeldata[labeldata$variable == count_by & labeldata$level == x, "val_label"]
        if (length(label) > 0) {
          return(label)
        } else {
          return(x)
        }
      })
    }
    
    if (!is.null(var_subgroup)) {
      subgroup_levels <- unique(data[[var_subgroup]])
      for (countlevel in count_by_levels) {
        event_rate_col <- paste0("Count(", count_by, "=", countlevel, ")")
        original_output <- original_output %>%
          tibble::add_column(!!event_rate_col := NA, .after = "Count")
        for (sub_level in subgroup_levels) {
          level_label <- if (!is.null(labeldata)) {
            label <- as.character(labeldata[labeldata$variable == var_subgroup & labeldata$level == sub_level, "val_label"])[1]
          } else {
            sub_level
          }
          value_to_insert <- count_output[count_output[[count_by]] == countlevel & count_output[[var_subgroup]] == sub_level, "Count"]
          value_to_insert <- value_to_insert[!is.na(value_to_insert)]
          original_output[[event_rate_col]][trimws(original_output[["Variable"]]) == level_label] <- value_to_insert[1]
        }
      }
      
      return(original_output)
    } else {
      for (countlevel in count_by_levels) {
        event_rate_col <- paste0("Count(", count_by, "=", countlevel, ")")
        original_output <- original_output %>%
          tibble::add_column(!!event_rate_col := NA, .after = "Count")
        value_to_insert <- count_output[count_output[[count_by]] == countlevel, "Count"]
        value_to_insert <- value_to_insert[!is.na(value_to_insert)]
        
        original_output[[event_rate_col]][trimws(original_output[["Variable"]]) == "Overall"] <- value_to_insert[1]
      }
      return(original_output)
    }
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
#' @param labeldata Label info, made by `mk.lev` function, Default: NULL
#' @param count_by  Variable name to stratify counts by (string). Default: NULL.
#' @param event     If `TRUE`, show counts/metrics instead of only model estimates. Default: FALSE.
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


TableSubgroupMultiGLM <- function(formula, var_subgroups = NULL, var_cov = NULL, data, family = "binomial", decimal.estimate = 2, decimal.percent = 1, decimal.pvalue = 3, line = F, labeldata = NULL, count_by = NULL, event = FALSE) {
  . <- NULL
  out.all <- TableSubgroupGLM(formula, var_subgroup = NULL, var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, labeldata = labeldata, count_by = count_by, event = event)
  
  if (is.null(var_subgroups)) {
    return(out.all)
  } else {
    out.list <- purrr::map(var_subgroups, ~ TableSubgroupGLM(formula, var_subgroup = ., var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue, labeldata = labeldata, count_by = count_by, event = event))
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
