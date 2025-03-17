#' @title svycoxph.display: table for svycoxph.object in survey package.
#' @description Table for complex design cox model.
#' @param svycoxph.obj svycoxph.object
#' @param decimal digit, Default: 2
#' @param pcut.univariate pcut.univariate, Default: NULL
#' @return List including table, metric, caption
#' @details DETAILS
#' @examples
#' library(survival)
#' data(pbc)
#' pbc$sex <- factor(pbc$sex)
#' pbc$stage <- factor(pbc$stage)
#' pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
#' biasmodel <- glm(randomized ~ age * edema, data = pbc, family = binomial)
#' pbc$randprob <- fitted(biasmodel)
#'
#' if (is.null(pbc$albumin)) pbc$albumin <- pbc$alb ## pre2.9.0
#'
#' dpbc <- survey::svydesign(
#'   id = ~1, prob = ~randprob, strata = ~edema,
#'   data = subset(pbc, randomized)
#' )
#'
#' model <- survey::svycoxph(Surv(time, status > 0) ~ sex + protime + albumin + stage,
#'   design = dpbc
#' )
#' svycox.display(model)
#' @seealso
#'  \code{\link[survey]{svycoxph}}
#'  \code{\link[stats]{AIC}}
#' @rdname svycox.display
#' @export
#' @importFrom survey svycoxph
#' @importFrom stats AIC update model.frame


svycox.display <- function(svycoxph.obj, decimal = 2, pcut.univariate = NULL) {
  model <- svycoxph.obj
  if (!any(class(model) == "svycoxph")) {
    stop("Model not from Survey cox model")
  }
  xf <- attr(model$terms, "term.labels")
  formula.surv <- as.character(model$formula)[2]
  design.model <- model$survey.design

  model_data <- stats::model.frame(model)
  categorical_vars <- xf[sapply(model_data[xf], function(x) is.factor(x) | is.character(x))]
  
  if (length(xf) == 1) {
    uni.res <- data.frame(summary(model)$coefficients)
    # uni.res <- data.frame(summary(survey::svycoxph(as.formula(paste(formula.surv, "~", xf, sep="")), design = design.model))$coefficients)
    names(uni.res)[ncol(uni.res)] <- "p"
    uni.res2 <- uni.res[, c("coef", grep("se", colnames(uni.res), value = T)[length(grep("se", colnames(uni.res)))], "z", "p")]
    unis <- list(uni.res2)

    fix.all <- coxExp(uni.res2, dec = decimal)
    colnames(fix.all) <- c("HR(95%CI)", "P value")
    # rownames(fix.all) = ifelse(mtype == "frailty", names(model$coefficients)[-length(model$coefficients)], names(model$coefficients))
    rownames(fix.all) <- names(model$coefficients)
  } else {

    unis <- lapply(xf, function(x) {
      tryCatch(
        {
          uni.res <- data.frame(summary(stats::update(model, formula(paste(paste(c(". ~ .", xf), collapse = " - "), " + ", x)), design = design.model))$coefficients)
          names(uni.res)[ncol(uni.res)] <- "p"
          uni.res2 <- uni.res[, c("coef", grep("se", colnames(uni.res), value = T)[length(grep("se", colnames(uni.res)))], "z", "p")]
          return(uni.res2)
        },
        error = function(e) {
          if (grepl(":", x)) {
            a <- unlist(strsplit(x, ":"))[1]
            b <- unlist(strsplit(x, ":"))[2]

            new.rn <- rownames(mul.res)[grepl(a, rownames(mul.res)) & grepl(b, rownames(mul.res))]
            uni.res2 <- data.frame(matrix(nrow = length(new.rn), ncol = 4))
            rownames(uni.res2) <- new.rn
            colnames(uni.res2) <- c("coef", "robust.se", "z", "p")

            return(uni.res2)
          } else {
            new.rn <- rownames(mul.res)[rownames(mul.res) %in% paste0(x, model$xlevels[[x]])]
            uni.res2 <- data.frame(matrix(nrow = length(new.rn), ncol = 4))
            rownames(uni.res2) <- new.rn
            colnames(uni.res2) <- c("coef", "robust.se", "z", "p")

            return(uni.res2)
          }
        }
      )
    })

    unis2 <- Reduce(rbind, unis)
    uni.res <- unis2
    if (is.null(pcut.univariate)){
      mul.res <- data.frame(summary(model)$coefficients)
      colnames(mul.res)[ncol(mul.res)] <- "p"
      
    }else{
      significant_vars <- rownames(uni.res)[as.numeric(uni.res[, 4]) < pcut.univariate]
      if (length(categorical_vars) != 0){
        factor_vars_list <- lapply(categorical_vars, function(factor_var) {
          factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
          factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
          
          
          matches <- grep(paste0("^", factor_var_escaped), rownames(uni.res), value = TRUE)
          return(matches)
        })
        names(factor_vars_list) <- categorical_vars
        
        for (key in names(factor_vars_list)) {
          variables <- factor_vars_list[[key]]
          
          p_values <- uni.res[variables, 4]
          
          if (any(p_values < pcut.univariate, na.rm = TRUE)) {
            significant_vars <- setdiff(significant_vars, variables)
            
            significant_vars <- unique(c(significant_vars, key))
          }
        }
      }
      if (length(significant_vars) == 0 ){
        
        mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = ncol(uni.res))
        colnames(mul.res) <- colnames(uni.res)
        rownames(mul.res) <- rownames(uni.res)
        
      }else{
        
        
        selected_formula <- as.formula(paste(formula.surv, "~", paste(significant_vars, collapse = " + ")))
        selected_model <- survey::svycoxph(selected_formula, design = design.model ) 
        
        mul <- data.frame(summary(selected_model)$coefficients)
        colnames(mul)[ncol(mul)] <- "p"
        
        mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = ncol(mul))
        rownames(mul.res) <- rownames(uni.res)
        
        if (!is.null(mul)) {
          mul_no_intercept <- as.matrix(mul[!grepl("Intercept", rownames(mul)), , drop = FALSE])
          
          
          for (var in rownames(mul_no_intercept)) { 
            mul.res[var, ] <- mul_no_intercept[var,]
          }
        }
        colnames(mul.res) <- colnames(mul)
        rownames(mul.res) <- rownames(uni.res)
        
        
      }
      
      
    }

    uni.res <- uni.res[rownames(uni.res) %in% rownames(mul.res), ] ## set

    fix.all <- cbind(coxExp(uni.res, dec = decimal), coxExp(mul.res[rownames(uni.res), names(uni.res)], dec = decimal))
    colnames(fix.all) <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
    rownames(fix.all) <- rownames(uni.res)
  }

  ## rownames
  rn.uni <- lapply(unis, rownames)

  fix.all.list <- lapply(1:length(xf), function(x) {
    if (grepl(":", xf[x])) {
      a <- unlist(strsplit(xf[x], ":"))[1]
      b <- unlist(strsplit(xf[x], ":"))[2]

      fix.all[grepl(a, rownames(fix.all)) & grepl(b, rownames(fix.all)), ]
    } else {
      fix.all[rownames(fix.all) %in% rn.uni[[x]], ]
    }
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)

  rn.list <- lapply(1:length(xf), function(x) {
    rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
  })
  varnum.2fac <- which(xf %in% names(model$xlevels)[lapply(model$xlevels, length) == 2])
  lapply(varnum.2fac, function(x) {
    rn.list[[x]] <<- paste(xf[x], ": ", model$xlevels[[xf[x]]][2], " vs ", model$xlevels[[xf[x]]][1], sep = "")
  })
  lapply(varnum.mfac, function(x) {
    if (grepl(":", xf[x])) {
      a <- unlist(strsplit(xf[x], ":"))[1]
      b <- unlist(strsplit(xf[x], ":"))[2]

      if (a %in% xf && b %in% xf) {
        ref <- paste0(a, model$xlevels[[a]][1], ":", b, model$xlevels[[b]][1])
        rn.list[[x]] <<- c(paste(xf[x], ": ref.=", ref, sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
      } else {
        rn.list[[x]] <<- c(paste(xf[x], ": ref.=NA", sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
      }
    } else {
      rn.list[[x]] <<- c(paste(xf[x], ": ref.=", model$xlevels[[xf[x]]][1], sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
    }
  })
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)
  pv.colnum <- which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  for (i in pv.colnum) {
    fix.all.unlist[, i] <- ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), decimal + 1))
  }


  ## metric
  no.obs <- model$n
  no.event <- model$nevent

  ## From survey package
  extractAIC.svycoxph <- function(fit, scale, k = 2, ...) {
    Delta <- solve(fit$inv.info, fit$var)
    deltabar <- mean(diag(Delta))
    d <- -2 * fit$ll[1]
    c(eff.p = sum(diag(Delta)), AIC = d + k * sum(diag(Delta)), deltabar = deltabar)
  }

  aic <- round(extractAIC.svycoxph(model, k = 2)[2], decimal)
  metric.mat <- cbind(c(NA, no.obs, no.event, aic), matrix(NA, 4, ncol(fix.all) - 1))
  rownames(metric.mat) <- c(NA, "No. of observations", "No. of events", "AIC")

  ## caption
  surv.string <- as.character(attr(model$terms, "variables")[[2]])
  time.var.name <- surv.string[2]
  status.var.name <- surv.string[3]
  intro <- paste("Survey cox model on time ('", time.var.name, "') to event ('", status.var.name, "')", sep = "")

  return(list(table = fix.all.unlist, metric = metric.mat, caption = intro))
}
