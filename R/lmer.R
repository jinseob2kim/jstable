#' @title lmerExp: transform the unit of coefficients (internal function)
#' @description Transform the unit of coefficients to "Coeff", "OR" or "RR"
#' @param lmer.coef lmer coefficients.
#' @param family Family: "gaussian", "binomial", "poisson", "quasipoisson", etc..., Default: 'binomial'
#' @param dec Decimal point
#' @return The transforemed coefficients(95% CI), p-value
#' @details DETAILS
#' @examples
#' # EXAMPLE1
#' @rdname lmerExp
#' @importFrom stats pnorm


lmerExp <- function(lmer.coef, family = "binomial", dec) {
  if (class(lmer.coef)[1] == "numeric") {
    lmer.coef <- t(data.frame(lmer.coef))
  }
  pv <- 2 * (1 - pnorm(abs(lmer.coef[, 3])))
  
  if (family == "binomial") {
    OR <- paste(round(exp(lmer.coef[, 1]), dec), " (", round(exp(lmer.coef[, 1] - 1.96 * lmer.coef[, 2]), dec), ", ", round(exp(lmer.coef[, 1] + 1.96 * lmer.coef[, 2]), dec), ")", sep = "")
    result <- cbind(OR, pv)
  } else if (family %in% c(NULL, "gaussian")) {
    coeff <- paste(round(lmer.coef[, 1], dec), " (", round(lmer.coef[, 1] - 1.96 * lmer.coef[, 2], dec), ", ", round(lmer.coef[, 1] + 1.96 * lmer.coef[, 2], dec), ")", sep = "")
    result <- cbind(coeff, pv)
  } else if (family %in% c("poisson", "quasipoisson")) {
    RR <- paste(round(exp(lmer.coef[, 1]), dec), " (", round(exp(lmer.coef[, 1] - 1.96 * lmer.coef[, 2]), dec), ", ", round(exp(lmer.coef[, 1] + 1.96 * lmer.coef[, 2]), dec), ")", sep = "")
    result <- cbind(RR, pv)
  }
  rownames(result) <- rownames(lmer.coef) 
  return(result[ , , drop = F]) 
}



#' @title lmer.display: table for "lmerMod" or "glmerMod" object (lme4 package)
#' @description Make mixed effect model results from "lmerMod" or "glmerMod" object (lme4 package)
#' @param lmerMod.obj "lmerMod" or "glmerMod" object
#' @param dec Decimal, Default: 2
#' @param pcut.univariate pcut.univariate, Default: NULL
#' @param data_for_univariate data for univariate model, Default: NULL
#' @param ci.ranef Show confidence interval of random effects?, Default: F
#' @return Table: fixed & random effect
#' @details DETAILS
#' @examples
#' library(geepack)
#' data(dietox)
#' dietox$Cu <- as.factor(dietox$Cu)
#' l1 <- lme4::lmer(Weight ~ Time + Cu + (1 | Pig) + (1 | Evit), data = dietox)
#' lmer.display(l1)
#' @rdname lmer.display
#' @export
#' @importFrom lme4 lmer glmer confint.merMod
#' @importFrom stats update formula reformulate

lmer.display <- function(lmerMod.obj, dec = 2, ci.ranef = F, pcut.univariate = NULL, data_for_univariate = NULL) {
  xvar <- NULL
  sl <- summary(lmerMod.obj)
  fixef <- sl$coefficients[-1, ]
  
  mdata <- lmerMod.obj@frame
  forms <- as.character(sl$call[[2]])
  y <- forms[2]
  xfr <- strsplit(forms[3], " \\+ ")[[1]]
  xr <- xfr[grepl("\\|", xfr)]
  xf <- xfr[!grepl("\\|", xfr)]
  family.lmer <- ifelse(is.null(sl$family), "gaussian", sl$family)
  uni.res <- ""
  
  categorical_vars <- xf[sapply(mdata[xf], is.factor)]
  if (is.null(data_for_univariate)) {
    basemodel <- update(lmerMod.obj, stats::formula(paste(c(". ~ .", xf), collapse = " - ")), data = mdata)
    unis <- lapply(xf, function(x) {
      summary(stats::update(basemodel, stats::formula(paste0(". ~ . +", x)), data = mdata))$coefficients
    })
    names(unis) <- xf  
    # uni.res <- unis2[rownames(unis2) != "(Intercept)", ]
    unis2 <- Reduce(rbind, unis)
    }
    else {
      random_part <- if (length(xr) > 0) paste(xr, collapse = "+") else ""
      unis <- lapply(xf, function(v) {
        keep <- complete.cases(data_for_univariate[, c(y ,v), drop = F]) 
        if (!any(keep)) {
          coef_ncol <- ncol(fixef)  
          return(matrix(nrow = 0, ncol = coef_ncol))
        } 
        terms <- if (length(xr) > 0) c(v, xr) else xr
        f_uni <- stats::reformulate(termlabels = terms, response = y)
        uni_mod <- update(lmerMod.obj, f_uni, data = data_for_univariate[keep, , drop = F])
        
        summary(uni_mod)$coefficients
      })
      unis <- Filter(function(x) is.matrix(x) && nrow(x) >0, unis)
      names(unis) <- xf
      unis2 <- Reduce(rbind, unis)
    }
  adj.res <- sl$coefficients
  adj.res <- adj.res[rownames(adj.res) != "(Intercept)", , drop = F]
  uni.res <- matrix(nrow = dim(adj.res)[1], ncol = dim(adj.res)[2])
  rownames(uni.res) <- rownames(adj.res)
  colnames(uni.res) <- colnames(adj.res)
  for (i in 1:nrow(adj.res)) {
    try(uni.res[rownames(adj.res)[i], ] <- unis2[rownames(adj.res)[i], ],
        silent = T)
  }
  
  
  
  if (length(xf) == 1) {
    fix.all <- lmerExp(uni.res, family = family.lmer, dec = dec)
    family.label <- colnames(fix.all)[1]
    colnames(fix.all) <- c(paste(family.label, "(95%CI)", sep = ""), "P value")
    rownames(fix.all) <- rownames(sl$coefficients)[-1]
  } else {
    if(is.null(pcut.univariate)){
      fix.all <- cbind(lmerExp(uni.res, family = family.lmer, dec = dec), lmerExp(fixef, family = family.lmer, dec = dec))
      family.label <- colnames(fix.all)[1]
      colnames(fix.all) <- c(paste("crude ", family.label, "(95%CI)", sep = ""), "crude P value", paste("adj. ", family.label, "(95%CI)", sep = ""), "adj. P value")
      rownames(fix.all) <- rownames(sl$coefficients)[-1]
      
    }else{
      uni.val <- lmerExp(uni.res, family = family.lmer, dec = dec)
      significant_vars <- rownames(uni.val)[as.numeric(uni.val[, 2]) < pcut.univariate]
      
      
      if (length(categorical_vars) != 0){
        factor_vars_list <- lapply(categorical_vars, function(factor_var) {
          factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
          factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
          
          
          matches <- grep(paste0("^", factor_var_escaped), rownames(uni.val), value = T)
          return(matches)
        })
        names(factor_vars_list) <- categorical_vars
        
        for (key in names(factor_vars_list)) {
          variables <- factor_vars_list[[key]]
          
          p_values <- uni.val[variables, 2]
          
          if (any(p_values < pcut.univariate, na.rm = T)) {
            significant_vars <- setdiff(significant_vars, variables)
            
            significant_vars <- unique(c(significant_vars, key))
          }
        }
      }
      
      if (length(significant_vars) == 0 ){
        
        mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = 2)
        fix.all <- cbind(lmerExp(uni.res, family = family.lmer, dec = dec), mul.res)
        family.label <- colnames(fix.all)[1]
        colnames(fix.all) <- c(paste("crude ", family.label, "(95%CI)", sep = ""), "crude P value", paste("adj. ", family.label, "(95%CI)", sep = ""), "adj. P value")
        rownames(fix.all) <- rownames(sl$coefficients)[-1]
        
        
        
      }else{
        
        selected_formula <- as.formula(paste(y, "~", paste(significant_vars, collapse = " + "),"+", paste(xr,collapse = "+")))
        
        # Use data_for_univariate if provided, otherwise use mdata
        if (!is.null(data_for_univariate)) {
          # Filter for complete cases of selected variables
          idx_multi <- complete.cases(data_for_univariate[, c(y, significant_vars), drop = F])
          df_multi <- data_for_univariate[idx_multi, ]
          selected_model <- lme4::lmer(selected_formula, data = df_multi)
        } else {
          selected_model <- lme4::lmer(selected_formula, data = mdata)
        }
        
        selected_summary <- summary(selected_model)
        mul<-selected_summary$coefficients[-1,,drop = F]
        mulexp<-lmerExp(mul, family = family.lmer, dec = dec)
        
        mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = 2)
        rownames(mul.res) <- rownames(uni.res)
        #colnames(mul.res) <- c("coef","exp.coef.","se.coef.","robust.se" ,"z", "Pr...z..")
        #colnames(mul.res) <- colnames(data.frame(coefNA(model)))
        if (!is.null(mulexp)) {
          mul_no_intercept <- mulexp[!grepl("Intercept", rownames(mul)), , drop = F]
          
          
          for (var in rownames(mul_no_intercept)) { 
            mul.res[var, ] <- mul_no_intercept[var,]
          }
        }
      }
      fix.all <- cbind(lmerExp(uni.res, family = family.lmer, dec = dec), mul.res)
      family.label <- colnames(fix.all)[1]
      colnames(fix.all) <- c(paste("crude ", family.label, "(95%CI)", sep = ""), "crude P value", paste("adj. ", family.label, "(95%CI)", sep = ""), "adj. P value")
      rownames(fix.all) <- rownames(sl$coefficients)[-1]
      
      
    }
    
    
  }
  
  ranef <- data.frame(sl$varcor)[, c(1, 4)]
  ranef.out <- cbind(as.character(round(ranef[, 2], dec)), matrix(NA, nrow(ranef), ncol(fix.all) - 1))
  rownames(ranef.out) <- ranef[, 1]
  if (ci.ranef) {
    n_theta <- length(lme4::getME(lmerMod.obj, "theta"))
    ranef.ci <- confint.merMod(lmerMod.obj, parm = seq_len(n_theta), oldNames = F)^2
    ranef.paste <- paste(round(ranef$vcov, dec), " (", round(ranef.ci[, 1], dec), ",", round(ranef.ci[, 2], dec), ")", sep = "")
    ranef.out <- cbind(ranef.paste, matrix(NA, nrow(ranef), 3))
    rownames(ranef.out) <- ranef[, 1]
  }
  
  ranef.na <- rbind(rep(NA, ncol(fix.all)), ranef.out)
  rownames(ranef.na)[1] <- "Random effects"
  colnames(ranef.na) <- colnames(fix.all)
  
  
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
    # fix.all[grepl(x, rownames(fix.all)), ]
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)
  
  rn.list <- lapply(1:length(xf), function(x) {
    if (grepl(":", xf[x])) {
      a <- unlist(strsplit(xf[x], ":"))[1]
      b <- unlist(strsplit(xf[x], ":"))[2]
      
      rownames(fix.all)[grepl(a, rownames(fix.all)) & grepl(b, rownames(fix.all))]
    } else {
      rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
    }
    # rownames(fix.all)[grepl(x, rownames(fix.all))]
  })
  varnum.2fac <- which(lapply(xf, function(x) {
    length(sapply(mdata, levels)[[x]])
  }) == 2)
  lapply(varnum.2fac, function(x) {
    rn.list[[x]] <<- paste(xf[x], ": ", levels(mdata[, xf[x]])[2], " vs ", levels(mdata[, xf[x]])[1], sep = "")
  })
  lapply(varnum.mfac, function(x) {
    if (grepl(":", xf[x])) {
      a <- unlist(strsplit(xf[x], ":"))[1]
      b <- unlist(strsplit(xf[x], ":"))[2]
      
      if (a %in% xf && b %in% xf) {
        ref <- paste0(a, levels(mdata[, a])[1], ":", b, levels(mdata[, b])[1])
        rn.list[[x]] <<- c(paste(xf[x], ": ref.=", ref, sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
      } else {
        rn.list[[x]] <<- c(paste(xf[x], ": ref.=NA", sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
      }
    } else {
      rn.list[[x]] <<- c(paste(xf[x], ": ref.=", levels(mdata[, xf[x]])[1], sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
    }
    # rn.list[[x]] <<- c(paste(xf[x], ": ref.=", levels(mdata[, xf[x]])[1], sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
  })
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)
  tb.df <- as.data.frame(rbind(fix.all.unlist, ranef.na))
  
  
  ## metric
  # Use selected_model's metrics if pcut was applied, otherwise use original model
  if (!is.null(pcut.univariate) && exists("selected_model")) {
    selected_summary <- summary(selected_model)
    no.grp <- selected_summary$ngrps
    no.obs <- nrow(selected_model@frame)
    ll <- round(selected_summary$logLik[[1]], dec)
    aic <- round(selected_summary$AICtab, dec)[1]
  } else {
    no.grp <- sl$ngrps
    no.obs <- nrow(mdata)
    ll <- round(sl$logLik[[1]], dec)
    aic <- round(sl$AICtab, dec)[1]
  }
  met <- c(NA, no.grp, no.obs, ll, aic)
  met.mat <- cbind(met, matrix(NA, length(met), ncol(fix.all) - 1))
  rownames(met.mat) <- c("Metrics", paste("No. of groups (", rownames(met.mat)[2:(length(no.grp) + 1)], ")", sep = ""), "No. of observations", "Log-likelihood", "AIC value")
  met.mat[, 1] <- as.character(met.mat[, 1])
  colnames(met.mat) <- colnames(tb.df)
  tb.lmerMod <- rbind(tb.df, met.mat)
  lapply(seq(2, ncol(fix.all), by = 2), function(x) {
    tb.lmerMod[, x] <<- as.numeric(as.vector(tb.lmerMod[, x]))
  })
  
  ## caption
  cap.lmerMod <- paste(sl$methTitle, " : ", y, " ~ ", forms[3], sep = "")
  return(list(table = tb.lmerMod, caption = cap.lmerMod))
}