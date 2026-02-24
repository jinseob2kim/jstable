#' @title svyregress.display: table for svyglm.object
#' @description table for svyglm.object (survey package).
#' @param svyglm.obj svyglm.object
#' @param decimal digit, Default: 2
#' @param pcut.univariate pcut.univariate, Default: NULL
#' @return table
#' @details DETAILS
#' @examples
#' library(survey)
#' data(api)
#' apistrat$tt <- c(rep(1, 20), rep(0, nrow(apistrat) - 20))
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
#' ds <- svyglm(api00 ~ ell + meals + cname + mobility, design = dstrat)
#' ds2 <- svyglm(tt ~ ell + meals + cname + mobility, design = dstrat, family = quasibinomial())
#' svyregress.display(ds)
#' svyregress.display(ds2)
#' @rdname svyglm.display
#' @export
#' @importFrom survey svyglm
#' @importFrom stats update confint terms getCall


svyregress.display <- function(svyglm.obj, decimal = 2, pcut.univariate = NULL) {
  model <- svyglm.obj
  design.model <- model$survey.design
  terms_obj <- stats::terms(model)
  xs <- attr(terms_obj, "term.labels")
  offset_idx <- attr(terms_obj, "specials")$offset
  offset_terms <- if (!is.null(offset_idx) && length(offset_idx) > 0) {
    xs[offset_idx]
  } else {
    character(0)
  }
  if (length(offset_terms) == 0) {
    offset_terms <- xs[grepl("^offset\\(", xs)]
  }
  xs <- setdiff(xs, offset_terms)
  y <- names(model$model)[1]
  gaussianT <- ifelse(length(grep("gaussian", model$family)) == 1, T, F)
  current_offset <- stats::getCall(model)$offset
  
  model_data <- model.frame(model)
  base_vars <- xs[!grepl(":", xs) & xs %in% names(model_data)]
  if (length(base_vars) > 0) {
    categorical_vars <- base_vars[sapply(model_data[, base_vars, drop = FALSE], function(x) is.factor(x) | is.character(x))]
  } else {
    categorical_vars <- character(0)
  }
  
  

  if (length(xs) == 0) {
    stop("No independent variable")
  } else if (length(xs) == 1) {
    uni <- cbind(data.frame(coefNA(model))[-1, ], data.frame(stats::confint(model))[-1, ])
    rn.uni <- lapply(list(uni), rownames)
    # uni <- data.frame(summary(survey::svyglm(as.formula(paste(y, " ~ ", xs)), design = design.model, family = model$family))$coefficients)[-1, ]
    if (gaussianT) {
      summ <- paste(round(uni[, 1], decimal), " (", round(uni[, 5], decimal), ",", round(uni[, 6], decimal), ")", sep = "")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("Coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    } else {
      summ <- paste(round(exp(uni[, 1]), decimal), " (", round(exp(uni[, 5]), decimal), ",", round(exp(uni[, 6]), decimal), ")", sep = "")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    }
    rownames(uni.res) <- rownames(uni)
    res <- uni.res
  } else {
    update_args <- list(
      object = model,
      formula = stats::formula(paste(c(". ~ .", xs), collapse = " - ")),
      design = design.model
    )
    if (!is.null(current_offset)) {
      update_args$offset <- current_offset
    }
    basemodel <- do.call(stats::update, update_args)
    uni <- lapply(xs, function(v) {
      uni_model <- stats::update(basemodel, stats::formula(paste0(". ~ . +", v)), design = design.model)
      coef.df <- data.frame(coefNA(uni_model))[-1, ]
      confint.df <- data.frame(stats::confint(uni_model))[-1, ]

      conf.df <- data.frame(matrix(nrow = nrow(coef.df), ncol = 2))
      rownames(conf.df) <- rownames(coef.df)
      for (i in rownames(conf.df)) {
        conf.df[i, ] <- confint.df[i, ]
      }

      cbind(coef.df, conf.df)
    })

    # uni <- lapply(xs, function(v){
    #  summary(survey::svyglm(as.formula(paste(y, " ~ ", v)), design = design.model))$coefficients[-1, ]
    # })
    rn.uni <- lapply(uni, rownames)
    uni <- Reduce(rbind, uni)

    if (gaussianT) {
      summ <- paste(round(uni[, 1], decimal), " (", round(uni[, 5], decimal), ",", round(uni[, 6], decimal), ")", sep = "")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))))
      colnames(uni.res) <- c(paste("crude coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <- rownames(uni)
      
      if(is.null(pcut.univariate)){
        # mul <- summary(model)$coefficients[-1, ]
        mul <- cbind(coefNA(model)[-1, , drop = FALSE], stats::confint(model)[-1, , drop = FALSE])
        mul.summ <- paste(round(mul[, 1], decimal), " (", round(mul[, 5], decimal), ",", round(mul[, 6], decimal), ")", sep = "")
        mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
        colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
      }else{
        significant_coefs <- rownames(uni)[as.numeric(uni[, 4]) < pcut.univariate]
        terms_to_include <- character(0)
        
        if (length(categorical_vars) != 0){
          factor_vars_list <- lapply(categorical_vars, function(factor_var) {
            factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
            factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
            
            
            matches <- grep(paste0("^", factor_var_escaped), rownames(uni), value = TRUE)
            return(matches)
          })
          names(factor_vars_list) <- categorical_vars
          
          for (key in names(factor_vars_list)) {
            variables <- factor_vars_list[[key]]
            
            p_values <- uni[variables, 4]
            
            if (any(p_values < pcut.univariate, na.rm = TRUE)) {
              terms_to_include <- unique(c(terms_to_include, key))
            }
          }
        }
        
        for (term in xs) {
          if (grepl(":", term)) {
            interaction_parts <- strsplit(term, ":")[[1]]
            interaction_coefs <- rownames(uni)[grepl(term, rownames(uni), fixed = TRUE)]
            
            if (length(interaction_coefs) == 0) {
              interaction_mask <- rep(TRUE, nrow(uni))
              for (part in interaction_parts) {
                interaction_mask <- interaction_mask & grepl(part, rownames(uni), fixed = TRUE)
              }
              interaction_mask <- interaction_mask & grepl(":", rownames(uni), fixed = TRUE)
              interaction_coefs <- rownames(uni)[interaction_mask]
            }
            
            if (length(interaction_coefs) > 0) {
              p_values <- uni[interaction_coefs, 4]
              
              if (any(as.numeric(p_values) < pcut.univariate, na.rm = TRUE)) {
                main_effects <- strsplit(term, ":")[[1]]
                terms_to_include <- unique(c(terms_to_include, main_effects, term))
              }
            }
          } else if (!(term %in% categorical_vars)) {
            if (term %in% significant_coefs) {
              terms_to_include <- unique(c(terms_to_include, term))
            }
          }
        }
        
        significant_vars <- xs[xs %in% terms_to_include]
        
        if (length(significant_vars) == 0 ){
          
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = 2)
          colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
          rownames(mul.res) <- rownames(uni.res)
          
        }else{
          selected_rhs <- significant_vars
          if (length(offset_terms) > 0) {
            selected_rhs <- c(selected_rhs, offset_terms)
          }
          selected_formula <- as.formula(paste(y, "~", paste(selected_rhs, collapse = " + ")))
          glm_args <- list(formula = selected_formula, design = design.model, family = model$family)
          if (!is.null(current_offset)) {
            glm_args$offset <- current_offset
          }
          selected_model <- do.call(survey::svyglm, glm_args)
          mul <- cbind(coefNA(selected_model)[-1, , drop = FALSE], stats::confint(selected_model)[-1, , drop = FALSE])
          mul.summ <- paste(round(mul[, 1], decimal), " (", round(mul[, 5], decimal), ",", round(mul[, 6], decimal), ")", sep = "")
          mul.res1 <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
          colnames(mul.res1) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
          
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = 2)
          rownames(mul.res) <- rownames(uni.res)
          
          if (!is.null(mul.res1)) {
            mul_no_intercept <- mul.res1[!grepl("Intercept", rownames(mul.res1)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
              mul.res[var, ] <- mul_no_intercept[var,]
            }
          }
          colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
          rownames(mul.res) <- rownames(uni.res)
          
        }
      }
    } else {
      summ <- paste(round(exp(uni[, 1]), decimal), " (", round(exp(uni[, 5]), decimal), ",", round(exp(uni[, 6]), decimal), ")", sep = "")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))))
      colnames(uni.res) <- c(paste("crude OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <- rownames(uni)
      if(is.null(pcut.univariate)){
        # mul <- summary(model)$coefficients[-1, ]
        mul <- cbind(coefNA(model)[-1, , drop = FALSE], stats::confint(model)[-1, , drop = FALSE])
        mul.summ <- paste(round(exp(mul[, 1]), decimal), " (", round(exp(mul[, 5]), decimal), ",", round(exp(mul[, 6]), decimal), ")", sep = "")
        mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
        colnames(mul.res) <- c(paste("adj. OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
      }else{
        significant_coefs <- rownames(uni)[as.numeric(uni[, 4]) < pcut.univariate]
        terms_to_include <- character(0)
        
        if (length(categorical_vars) != 0){
          factor_vars_list <- lapply(categorical_vars, function(factor_var) {
            factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
            factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
            
            
            matches <- grep(paste0("^", factor_var_escaped), rownames(uni), value = TRUE)
            return(matches)
          })
          names(factor_vars_list) <- categorical_vars
          
          for (key in names(factor_vars_list)) {
            variables <- factor_vars_list[[key]]
            
            p_values <- uni[variables, 4]
            
            if (any(p_values < pcut.univariate, na.rm = TRUE)) {
              terms_to_include <- unique(c(terms_to_include, key))
            }
          }
        }
        
        for (term in xs) {
          if (grepl(":", term)) {
            interaction_parts <- strsplit(term, ":")[[1]]
            interaction_coefs <- rownames(uni)[grepl(term, rownames(uni), fixed = TRUE)]
            
            if (length(interaction_coefs) == 0) {
              interaction_mask <- rep(TRUE, nrow(uni))
              for (part in interaction_parts) {
                interaction_mask <- interaction_mask & grepl(part, rownames(uni), fixed = TRUE)
              }
              interaction_mask <- interaction_mask & grepl(":", rownames(uni), fixed = TRUE)
              interaction_coefs <- rownames(uni)[interaction_mask]
            }
            
            if (length(interaction_coefs) > 0) {
              p_values <- uni[interaction_coefs, 4]
              
              if (any(as.numeric(p_values) < pcut.univariate, na.rm = TRUE)) {
                main_effects <- strsplit(term, ":")[[1]]
                terms_to_include <- unique(c(terms_to_include, main_effects, term))
              }
            }
          } else if (!(term %in% categorical_vars)) {
            if (term %in% significant_coefs) {
              terms_to_include <- unique(c(terms_to_include, term))
            }
          }
        }
        
        significant_vars <- xs[xs %in% terms_to_include]
        
        if (length(significant_vars) == 0 ){
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = 2)
          colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
          rownames(mul.res) <- rownames(uni.res)
          
        }else{
          selected_rhs <- significant_vars
          if (length(offset_terms) > 0) {
            selected_rhs <- c(selected_rhs, offset_terms)
          }
          selected_formula <- as.formula(paste(y, "~", paste(selected_rhs, collapse = " + ")))
          glm_args <- list(formula = selected_formula, design = design.model, family = model$family)
          if (!is.null(current_offset)) {
            glm_args$offset <- current_offset
          }
          selected_model <- do.call(survey::svyglm, glm_args)
          mul <- cbind(coefNA(selected_model)[-1, , drop = FALSE], stats::confint(selected_model)[-1, , drop = FALSE])
          mul.summ <- paste(round(exp(mul[, 1]), decimal), " (", round(exp(mul[, 5]), decimal), ",", round(exp(mul[, 6]), decimal), ")", sep = "")
          mul.res1 <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
          colnames(mul.res1) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
          
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = 2)
          rownames(mul.res) <- rownames(uni.res)
          
          if (!is.null(mul.res1)) {
            mul_no_intercept <- mul.res1[!grepl("Intercept", rownames(mul.res1)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
              mul.res[var, ] <- mul_no_intercept[var,]
            }
          }
          colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
          rownames(mul.res) <- rownames(uni.res)
          
        }
        
        
      }
      
      
    }
    res <- cbind(uni.res[rownames(uni.res) %in% rownames(mul.res), ], mul.res)
    rownames(res) <- rownames(uni.res)
  }

  fix.all <- res
  mdata <- model$model
  ## rownames
  # fix.all.list = lapply(xs, function(x){fix.all[grepl(x, rownames(fix.all)),]})
  fix.all.list <- lapply(1:length(xs), function(x) {
    fix.all[rownames(fix.all) %in% rn.uni[[x]], ]
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)

  # rn.list = lapply(xs, function(x){rownames(fix.all)[grepl(x, rownames(fix.all))]})
  rn.list <- lapply(1:length(xs), function(x) {
    rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
  })
  varnum.2fac <- which(xs %in% names(model$xlevels)[lapply(model$xlevels, length) == 2])
  lapply(varnum.2fac, function(x) {
    rn.list[[x]] <<- paste(xs[x], ": ", model$xlevels[[xs[x]]][2], " vs ", model$xlevels[[xs[x]]][1], sep = "")
  })
  lapply(varnum.mfac, function(x) {
    var_name <- xs[x]
    if (grepl(":", var_name)) {
      components <- unlist(strsplit(var_name, ":"))
      are_all_factors <- all(sapply(components, function(comp) comp %in% names(model$xlevels)))
      
      if (are_all_factors) {
        ref <- paste(sapply(components, function(comp) model$xlevels[[comp]][1]), collapse = ":")
        rn.list[[x]] <<- c(paste(var_name, ": ref.=", ref, sep = ""), gsub(var_name, "   ", rn.list[[x]]))
      } else {
        factor_comp <- components[components %in% names(model$xlevels)]
        if (length(factor_comp) > 0) {
          multi_level_factors <- factor_comp[sapply(factor_comp, function(f) length(model$xlevels[[f]]) > 2)]
          if (length(multi_level_factors) > 0) {
            ref_levels <- sapply(multi_level_factors, function(f) model$xlevels[[f]][1])
            ref_string <- paste(ref_levels, collapse = ",")
            rn.list[[x]] <<- c(paste(var_name, ": ref.=", ref_string, sep = ""), gsub(var_name, "   ", rn.list[[x]]))
          } else {
            rn.list[[x]] <<- c(var_name, gsub(var_name, "   ", rn.list[[x]]))
          }
        } else {
          rn.list[[x]] <<- c(var_name, gsub(var_name, "   ", rn.list[[x]]))
        }
      }
    } else {
      if (var_name %in% names(model$xlevels)) {
        rn.list[[x]] <<- c(paste(var_name, ": ref.=", model$xlevels[[var_name]][1], sep = ""), gsub(var_name, "   ", rn.list[[x]]))
      } else {
        rn.list[[x]] <<- c(var_name, gsub(var_name, "   ", rn.list[[x]]))
      }
    }
    # rn.list[[x]] <<- c(paste(xs[x], ": ref.=", model$xlevels[[xs[x]]][1], sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
  })
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)

  # pv.colnum = which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  # for (i in pv.colnum){
  #  fix.all.unlist[, i] = ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), decimal + 1))
  # }


  outcome.name <- names(model$model)[1]


  if (gaussianT) {
    first.line <- paste("Linear regression predicting ", outcome.name, sep = "", "- weighted data\n")
    last.lines <- paste("No. of observations = ",
      length(model$y), "\n", "AIC value = ", round(
        model$aic,
        decimal + 2
      ), "\n", "\n",
      sep = ""
    )
  } else {
    first.line <- paste("Logistic regression predicting ", outcome.name, sep = "", "- weighted data\n")
    last.lines <- paste("No. of observations = ", nrow(model$model),
      "\n", "\n",
      sep = ""
    )
  }

  results <- list(
    first.line = first.line, table = fix.all.unlist,
    last.lines = last.lines
  )
  class(results) <- c("display", "list")
  return(results)
}
