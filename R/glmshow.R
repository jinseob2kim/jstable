#' @title coefNA: make coefficient table with NA
#' @description Make coefficient table with NA
#' @param model glm object (gaussian or binomial)
#' @return coefficient table with NA
#' @details DETAILS
#' @examples
#'
#' coefNA(glm(mpg ~ wt + qsec, data = mtcars))
#' @rdname coefNA
#' @export
#' @importFrom stats coef

coefNA <- function(model) {
  coef.rownames <- merge(coef(summary(model)), model$coefficients, by = 0, all = T)
  coef.matrix <- as.matrix(coef.rownames[, -c(1, ncol(coef.rownames))])
  rownames(coef.matrix) <- coef.rownames[, "Row.names"]
  return(coef.matrix[names(model$coefficients), , drop = FALSE])
}


#' @title glmshow.display: Show summary table of glm object.
#' @description Show summary table of glm object(regression, logistic).
#' @param glm.object glm.object
#' @param decimal digits, Default: 2
#' @param pcut.univariate pcut.univariate, Default: NULL
#' @return table
#' @details DETAILS
#' @examples
#' glmshow.display(glm(mpg ~ wt + qsec, data = mtcars))
#' @seealso
#'  \code{\link[stats]{glm}}
#' @rdname glmshow.display
#' @export
#' @importFrom stats glm cor predict formula
#' @importFrom magrittr %>%

glmshow.display <- function(glm.object, decimal = 2, pcut.univariate=NULL) {
  model <- glm.object
  if (!any(class(model) %in% c("lm", "glm"))) {
    stop("Model not from  GLM")
  }

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
  model_family <- family(model)$family
  family <- ifelse(grepl("gaussian", model_family), 1, 
                   ifelse(grepl("binomial", model_family), 2, 
                          ifelse(grepl("poisson", model_family), 3, 3)))
  
  data <- model$data
  xs.factor <- if (length(model$xlevels) != 0) {
    names(model$xlevels)
  } else {
    character(0)
  }
  ## table
  if (length(xs) == 0) {
    stop("No independent variable")
  } else if (length(xs) == 1) {
    uni <- data.frame(coefNA(glm.object))[-1, ]
    rn.uni <- lapply(list(uni), rownames)
    if (family == 1) {
      summ <- paste(round(uni[, 1], decimal), " (", round(uni[, 1] - 1.96 * uni[, 2], decimal), ",", round(uni[, 1] + 1.96 * uni[, 2], decimal), ")", sep = "")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("Coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    } else {
      summ <- paste(round(exp(uni[, 1]), decimal), " (", round(exp(uni[, 1] - 1.96 * uni[, 2]), decimal), ",", round(exp(uni[, 1] + 1.96 * uni[, 2]), decimal), ")", sep = "")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))), nrow = nrow(uni))
      if (family == 2) {
        colnames(uni.res) <- c(paste("OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
      } else {
        colnames(uni.res) <- c(paste("RR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
      }
    }
    rownames(uni.res) <- rownames(uni)
    res <- uni.res
  } else {
    model_call <- stats::getCall(model)
    current_offset <- model_call$offset
    update_args <- list(object = model,
                        formula = formula(paste(c(". ~ .", xs), collapse = " - ")),
                        data = data)
    if (!is.null(current_offset)) {
      update_args$offset <- current_offset
    }
    basemodel <- do.call(stats::update, update_args)

    # Create a list of univariate model results, one for each variable in xs.
    # We remove the intercept from each.
    uni_list <- lapply(xs, function(v) {
      uni_model <- stats::update(basemodel, formula(paste0(". ~ . +", v)), data = data)
      data.frame(coefNA(uni_model))[-1, , drop = FALSE]
    })

    # The rn.uni for grouping is based on the coefficients from each univariate model.
    rn.uni <- lapply(uni_list, rownames)

    # Get the coefficients from the full model to define the final table structure.
    mul_coef_names <- rownames(coefNA(model))[-1] # Exclude intercept

    # Create the final 'uni' matrix with the correct dimensions and NA values,
    # ensuring its rows match the multivariate model exactly.
    uni <- matrix(NA, nrow = length(mul_coef_names), ncol = ncol(coefNA(model)),
                  dimnames = list(mul_coef_names, colnames(coefNA(model))))

    # Create a list of all available univariate coefficients
    all_uni_coefs <- do.call(rbind, uni_list)

    # Fill the 'uni' matrix from the list of univariate results.
    # This ensures that each coefficient's crude estimate comes from its simplest model
    # and prevents overwriting (e.g., 'vs' crude estimate from 'y~vs', not 'y~vs+am+vs:am').
    for (coef_name in mul_coef_names) {
      # Find the first occurrence of the coefficient in the univariate results
      if (coef_name %in% rownames(all_uni_coefs)) {
        # Take the first one found, which corresponds to the simplest model
        uni[coef_name, ] <- as.matrix(all_uni_coefs[coef_name, , drop = FALSE][1, ])
      }
    }
    
    if (!is.null(pcut.univariate)) {
      uni_no_intercept <- uni[!grepl("Intercept", rownames(uni)), , drop = FALSE]
      significant_coefs <- rownames(uni_no_intercept)[as.numeric(uni_no_intercept[, 4]) < pcut.univariate]
      
      # Collect terms that should be included
      terms_to_include <- character(0)
      
      # Process factor variables
      if (length(model$xlevels) != 0){
        factor_vars_list <- lapply(xs.factor, function(factor_var) {
          factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
          factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
          
          matches <- grep(paste0("^", factor_var_escaped), rownames(coefNA(model)), value = TRUE)
          return(matches)
        })
        names(factor_vars_list) <- xs.factor
      
        for (key in names(factor_vars_list)) {
          variables <- factor_vars_list[[key]]
          
          p_values <- uni_no_intercept[variables, 4]
          
          if (any(p_values < pcut.univariate, na.rm = TRUE)) {
            terms_to_include <- unique(c(terms_to_include, key))
          }
        }
      }
      
      # Process continuous variables and interaction terms
      for (term in xs) {
        if (grepl(":", term)) {
          # This is an interaction term - treat like a factor variable
          # Find all coefficients related to this interaction term
          term_escaped <- gsub("\\(", "\\\\(", term)
          term_escaped <- gsub("\\)", "\\\\)", term_escaped)
          term_escaped <- gsub(":", ":", term_escaped)
          
          # Find all coefficients that match this interaction pattern
          interaction_coefs <- grep(paste0("^", term_escaped, "|:", strsplit(term, ":")[[1]][2]), 
                                   rownames(uni_no_intercept), value = TRUE)
          
          if (length(interaction_coefs) > 0) {
            # Get p-values for all interaction coefficients
            p_values <- uni_no_intercept[interaction_coefs, 4]
            
            # If ANY coefficient is significant, include the interaction term and main effects
            if (any(p_values < pcut.univariate, na.rm = TRUE)) {
              main_effects <- strsplit(term, ":")[[1]]
              terms_to_include <- unique(c(terms_to_include, main_effects, term))
            }
          }
        } else if (!(term %in% xs.factor)) {
          # This is a continuous variable
          if (term %in% significant_coefs) {
            terms_to_include <- unique(c(terms_to_include, term))
          }
        }
      }
      
      # Now create significant_vars maintaining the order from xs
      significant_vars <- xs[xs %in% terms_to_include]
    } else {
      significant_vars <- xs  
    }
    
    
    if (family == 1) {
      summ <- paste(round(uni[, 1], decimal), " (", round(uni[, 1] - 1.96 * uni[, 2], decimal), ",", round(uni[, 1] + 1.96 * uni[, 2], decimal), ")", sep = "")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))))
      colnames(uni.res) <- c(paste("crude coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <- rownames(uni)
      uni.res_no_intercept <- uni.res[!grepl("Intercept", rownames(uni)), , drop = FALSE]
      
      
      if (is.null(pcut.univariate)){
        mul <- coefNA(model)[-1, ]
        mul.summ <- paste(round(mul[, 1], decimal), " (", round(mul[, 1] - 1.96 * mul[, 2], decimal), ",", round(mul[, 1] + 1.96 * mul[, 2], decimal), ")", sep = "")
        mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
        colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
      }else{
        if (length(significant_vars) == 0 ){
          mul.res <- matrix(NA, nrow = nrow(uni.res_no_intercept), ncol = 2)
          rownames(mul.res) <- rownames(uni.res_no_intercept)
          colnames(mul.res) <- c("adj. coeff. (95%CI)", "adj. P value")
          
        }else{
          selected_rhs <- significant_vars
          if (length(offset_terms) > 0) {
            selected_rhs <- c(selected_rhs, offset_terms)
          }
          selected_formula <- as.formula(paste(y, "~", paste(selected_rhs, collapse = " + ")))
          glm_args <- list(formula = selected_formula, data = data, family = model$family)
          if (!is.null(current_offset)) {
            glm_args$offset <- current_offset
          }
          selected_model <- do.call(stats::glm, glm_args)
          mul <- coefNA(selected_model)
          mul.res <- matrix(NA, nrow = nrow(uni.res_no_intercept), ncol = 2)
          rownames(mul.res) <- rownames(uni.res_no_intercept)
          colnames(mul.res) <- c("adj. coeff. (95%CI)", "adj. P value")
          if (!is.null(mul)) {
            mul_no_intercept <- mul[!grepl("Intercept", rownames(mul)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
              if (var %in% rownames(mul.res)) {
                mul.res[var, ] <- c(
                  paste(round(mul[var, 1], decimal), " (", 
                        round(mul[var, 1] - 1.96 * mul[var, 2], decimal), ",", 
                        round(mul[var, 1] + 1.96 * mul[var, 2], decimal), ")", sep = ""),
                  ifelse(mul[var, 4] <= 0.001, "< 0.001", as.character(round(mul[var, 4], decimal + 1)))
                )
              }
            }
          }
      }
        
      }
    } else {
      k <- ifelse(family == 2, "OR", "RR")

      summ <- paste(round(exp(uni[, 1]), decimal), " (", round(exp(uni[, 1] - 1.96 * uni[, 2]), decimal), ",", round(exp(uni[, 1] + 1.96 * uni[, 2]), decimal), ")", sep = "")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))))
      colnames(uni.res) <- c(paste("crude ", k, ".(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <- rownames(uni)
      uni.res_no_intercept <- uni.res[!grepl("Intercept", rownames(uni)), , drop = FALSE]
      
      if (is.null(pcut.univariate)){
        mul <- coefNA(model)[-1, ]
        mul.summ <- paste(round(exp(mul[, 1]), decimal), " (", round(exp(mul[, 1] - 1.96 * mul[, 2]), decimal), ",", round(exp(mul[, 1] + 1.96 * mul[, 2]), decimal), ")", sep = "")
        mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
        colnames(mul.res) <- c(paste("adj. ", k, ".(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
      }else{
        if (length(significant_vars) == 0 ){
          mul.res <- matrix(NA, nrow = nrow(uni.res_no_intercept), ncol = 2)
          rownames(mul.res) <- rownames(uni.res_no_intercept)
          colnames(mul.res) <- c("adj. coeff. (95%CI)", "adj. P value")
          
        }else{
          selected_rhs <- significant_vars
          if (length(offset_terms) > 0) {
            selected_rhs <- c(selected_rhs, offset_terms)
          }
          selected_formula <- as.formula(paste(y, "~", paste(selected_rhs, collapse = " + ")))
          glm_args <- list(formula = selected_formula, data = data, family = model$family)
          if (!is.null(current_offset)) {
            glm_args$offset <- current_offset
          }
          selected_model <- do.call(stats::glm, glm_args)
          mul <- coefNA(selected_model)
          mul.res <- matrix(NA, nrow = nrow(uni.res_no_intercept), ncol = 2)
          rownames(mul.res) <- rownames(uni.res_no_intercept)
          colnames(mul.res) <- c("adj. coeff. (95%CI)", "adj. P value")
          
          if (!is.null(mul)) {
            mul_no_intercept <- mul[!grepl("Intercept", rownames(mul)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
              if (var %in% rownames(mul.res)) {
                mul.res[var, ] <- c(
                  paste(round(exp(mul[var, 1]), decimal), " (", 
                        round(exp(mul[var, 1] - 1.96 * mul[var, 2]), decimal), ",", 
                        round(exp(mul[var, 1] + 1.96 * mul[var, 2]), decimal), ")", sep = ""),
                  ifelse(mul[var, 4] <= 0.001, "< 0.001", as.character(round(mul[var, 4], decimal + 1)))
                )
              }
            }
          }
          
        }
    }
    }

    res <- cbind(uni.res_no_intercept, mul.res)
    rownames(res) <- rownames(mul.res)
  }

  ## label
  fix.all <- res

  ## rownames
  fix.all.list <- lapply(1:length(xs), function(x) {
    fix.all[rownames(fix.all) %in% rn.uni[[x]], ]
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)

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
        # Interaction with at least one continuous variable
        # Check if any component is a factor with more than 2 levels
        factor_comp <- components[components %in% names(model$xlevels)]
        if (length(factor_comp) > 0) {
          # Check if any factor has more than 2 levels
          multi_level_factors <- factor_comp[sapply(factor_comp, function(f) length(model$xlevels[[f]]) > 2)]
          if (length(multi_level_factors) > 0) {
            # Create ref string for the multi-level factor
            ref_levels <- sapply(multi_level_factors, function(f) model$xlevels[[f]][1])
            ref_string <- paste(ref_levels, collapse = ",")
            rn.list[[x]] <<- c(paste(var_name, ": ref.=", ref_string, sep = ""), gsub(var_name, "   ", rn.list[[x]]))
          } else {
            # All factors have only 2 levels or less
            rn.list[[x]] <<- c(var_name, gsub(var_name, "   ", rn.list[[x]]))
          }
        } else {
          # No factor component
          rn.list[[x]] <<- c(var_name, gsub(var_name, "   ", rn.list[[x]]))
        }
      }
    } else {
      # Not an interaction term
      if (var_name %in% names(model$xlevels)) {
        # It's a factor variable
        rn.list[[x]] <<- c(paste(var_name, ": ref.=", model$xlevels[[var_name]][1], sep = ""), gsub(var_name, "   ", rn.list[[x]]))
      } else {
        # It's not a factor, but has multiple rows (e.g., splines).
        # Just add header.
        rn.list[[x]] <<- c(var_name, gsub(var_name, "   ", rn.list[[x]]))
      }
    }
  })
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)

  # pv.colnum = which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  # for (i in pv.colnum){
  #  fix.all.unlist[, i] = ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), decimal + 1))
  # }


  outcome.name <- y


  if (family == 1) {
    first.line <- paste("Linear regression predicting ", outcome.name, sep = "", "\n")
    last.lines <- paste("No. of observations = ",
      length(model$y), "\n", "R-squared = ", round(cor(model$y, predict(model))^2, decimal + 2), "\n",
      "AIC value = ", round(model$aic, decimal + 2), "\n", "\n",
      sep = ""
    )
  } else {
    first.line <- paste("Logistic regression predicting ", outcome.name, sep = "", "\n")
    last.lines <- paste("No. of observations = ",
      length(model$y), "\n",
      "AIC value = ", round(model$aic, decimal + 2), "\n", "\n",
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

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 17562 rows and 24 variables:
#' \describe{
#'   \item{\code{ccode}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{cname}}{character COLUMN_DESCRIPTION}
#'   \item{\code{yy}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{mm}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{dd}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{date}}{character COLUMN_DESCRIPTION}
#'   \item{\code{nonacc}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{cardio}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{respir}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{influenza}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{meanpm10}}{double COLUMN_DESCRIPTION}
#'   \item{\code{meanso2}}{double COLUMN_DESCRIPTION}
#'   \item{\code{meanno2}}{double COLUMN_DESCRIPTION}
#'   \item{\code{meanco}}{double COLUMN_DESCRIPTION}
#'   \item{\code{maxco}}{double COLUMN_DESCRIPTION}
#'   \item{\code{maxo3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{meantemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{maxtemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{mintemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{meanhumi}}{double COLUMN_DESCRIPTION}
#'   \item{\code{meanpress}}{double COLUMN_DESCRIPTION}
#'   \item{\code{season}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{dow}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sn}}{integer COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"mort"
