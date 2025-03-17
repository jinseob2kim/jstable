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

  xs <- attr(model$terms, "term.labels")
  y <- names(model$model)[1]
  family <- ifelse(length(grep("gaussian", model$family)) == 1, 1, ifelse(length(grep("binomial", model$family)) >= 1, 2, 3))
  
  data <- model$data
  if (length(model$xlevels) != 0){
    #xs.factor <- names(model$xlevels)[sapply(model$xlevels, function(x){length(x) > 2})]
    xs.factor <- names(model$xlevels)
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
    basemodel <- stats::update(model, formula(paste(c(". ~ .", xs), collapse = " - ")), data = data)

    uni <- lapply(xs, function(v) {
      data.frame(coefNA(stats::update(basemodel, formula(paste0(". ~ . +", v)), data = data)))
      # data.frame(coefNA(stats::glm(as.formula(paste(y, " ~ ", v)), data = data, family = model$family)))[-1, ]
    })
    rn.uni <- lapply(uni, rownames)
    uni <- Reduce(rbind, uni)
    
    if (!is.null(pcut.univariate)) {
      uni_no_intercept <- uni[!grepl("Intercept", rownames(uni)), , drop = FALSE]
      significant_vars <- rownames(uni_no_intercept)[as.numeric(uni_no_intercept[, 4]) < pcut.univariate]
      
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
            significant_vars <- setdiff(significant_vars, variables)
            
            significant_vars <- unique(c(significant_vars, key))
          }
        }
      }
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
          selected_formula <- as.formula(paste(y, "~", paste(significant_vars, collapse = " + ")))
          selected_model <- stats::glm(selected_formula, data = data, family = model$family) 
          mul <- coefNA(selected_model)
          mul.res <- matrix(NA, nrow = nrow(uni.res_no_intercept), ncol = 2)
          rownames(mul.res) <- rownames(uni.res_no_intercept)
          colnames(mul.res) <- c("adj. coeff. (95%CI)", "adj. P value")
          if (!is.null(mul)) {
            mul_no_intercept <- mul[!grepl("Intercept", rownames(mul)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
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
          selected_formula <- as.formula(paste(y, "~", paste(significant_vars, collapse = " + ")))
          selected_model <- stats::glm(selected_formula, data = data, family = model$family) 
          mul <- coefNA(selected_model)
          mul.res <- matrix(NA, nrow = nrow(uni.res_no_intercept), ncol = 2)
          rownames(mul.res) <- rownames(uni.res_no_intercept)
          colnames(mul.res) <- c("adj. coeff. (95%CI)", "adj. P value")
          
          if (!is.null(mul)) {
            mul_no_intercept <- mul[!grepl("Intercept", rownames(mul)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
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
    if (grepl(":", xs[x])) {
      a <- unlist(strsplit(xs[x], ":"))[1]
      b <- unlist(strsplit(xs[x], ":"))[2]
      if (a %in% xs && b %in% xs) {
        ref <- paste0(a, model$xlevels[[a]][1], ":", b, model$xlevels[[b]][1])
        rn.list[[x]] <<- c(paste(xs[x], ": ref.=", ref, sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
      } else {
        rn.list[[x]] <<- c(paste(xs[x], ": ref.=NA", model$xlevels[[xs[x]]][1], sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
      }
    } else {
      rn.list[[x]] <<- c(paste(xs[x], ": ref.=", model$xlevels[[xs[x]]][1], sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
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

