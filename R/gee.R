#' @title geeUni: The coefficient of univariate gee (internal function)
#' @description Extract the coefficients of univariate gee using geeglm function (geepack package).
#' @param y Dependant variable
#' @param x Independent variable
#' @param data Data
#' @param id.vec Vector of id (should be ordered)
#' @param family Family: "gaussian", "binomial", "poisson", "quasipoisson", etc...
#' @param cor.type Correlation structure, Default: 'exchangeable'
#' @return coefficient, standard error, p-value
#' @details DETAILS
#' @examples
#' library(geepack)
#' data(dietox)
#' dietox$Cu <- as.factor(dietox$Cu)
#' gee.uni <- geeUni("Weight", "Time",
#'   data = dietox, id.vec = dietox$Pig,
#'   family = "gaussian", cor.type = "exchangeable"
#' )
#' @rdname geeUni
#' @importFrom geepack geeglm
#' @importFrom stats as.formula update
#' @export


geeUni <- function(y, x, data, id.vec, family, cor.type = "exchangeable") {
  form <- as.formula(paste(y, "~", x))
  res <- geepack::geeglm(form, data = data, family = family, id = id.vec, corstr = cor.type)
  coef <- summary(res)$coefficients[-1, -3]
  return(coef)
}



#' @title geeExp: transform the unit of coefficients (internal function)
#' @description Transform the unit of coefficients to "Coeff", "OR" or "RR"
#' @param gee.coef geeUni object.
#' @param family Family: "gaussian", "binomial", "poisson", "quasipoisson", etc..., Default: 'binomial'
#' @param dec Decimal point
#' @return The transforemed coefficients(95% CI), p-value
#' @details DETAILS
#' @examples
#' library(geepack)
#' data(dietox)
#' dietox$Cu <- as.factor(dietox$Cu)
#' gee.uni <- geeUni("Weight", c("Time", "Cu"),
#'   data = dietox, id.vec = dietox$Pig,
#'   family = "gaussian", cor.type = "exchangeable"
#' )
#' gee.exp <- geeExp(gee.uni, "binomial", 2)
#' @rdname geeExp
#' @export


geeExp <- function(gee.coef, family = "binomial", dec) {
  if (family == "binomial") {
    OR <- ifelse(is.na(gee.coef[,1]), NA, paste(round(exp(gee.coef[, 1]), dec), " (", round(exp(gee.coef[, 1] - 1.96 * gee.coef[, 2]), dec), ",", round(exp(gee.coef[, 1] + 1.96 * gee.coef[, 2]), dec), ")", sep = ""))
    return(cbind(OR, gee.coef[, 3]))
  } else if (family == "gaussian") {
    coeff <- ifelse(is.na(gee.coef[,1]),NA,paste(round(gee.coef[, 1], dec), " (", round(gee.coef[, 1] - 1.96 * gee.coef[, 2], dec), ",", round(gee.coef[, 1] + 1.96 * gee.coef[, 2], dec), ")", sep = ""))
    return(cbind(coeff, gee.coef[, 3]))
  } else if (family %in% c("poisson", "quasipoisson")) {
    RR <- ifelse(is.na(gee.coef[,1]),NA,paste(round(exp(gee.coef[, 1]), dec), " (", round(exp(gee.coef[, 1] - 1.96 * gee.coef[, 2]), dec), ",", round(exp(gee.coef[, 1] + 1.96 * gee.coef[, 2]), dec), ")", sep = ""))
    return(cbind(RR, gee.coef[, 3]))
  }
}



#' @title geeglm.display
#' @description Make gee results from "geeglm" object
#' @param geeglm.obj "geeglm" object
#' @param decimal Decimal, Default: 2
#' @param pcut.univariate pcut.univariate, Default: NULL
#' @param data_for_univariate data for univariate model, Default: NULL
#' @return List: caption, main table, metrics table
#' @details DETAILS
#' @examples
#' library(geepack)
#' data(dietox)
#' dietox$Cu <- as.factor(dietox$Cu)
#' gee01 <- geeglm(Weight ~ Time + Cu,
#'   id = Pig, data = dietox,
#'   family = gaussian, corstr = "ex"
#' )
#' geeglm.display(gee01)
#' @seealso
#'  \code{\link[data.table]{data.table-package}}
#'  \code{\link[stats]{complete.cases}}
#' @rdname geeglm.display
#' @export
#' @importFrom data.table data.table
#' @importFrom stats complete.cases update formula

geeglm.display <- function(geeglm.obj, decimal = 2, pcut.univariate=NULL, data_for_univariate = NULL) {
  family.gee <- geeglm.obj$family[[1]]
  corstr.gee <- geeglm.obj$corstr
  y <- as.character(geeglm.obj$terms[[2]])
  xs <- names(geeglm.obj$model)[-1]
  
  # Initialize final_model to track which model is used for metrics
  final_model <- geeglm.obj
  
  # Initialize xs.factor
  xs.factor <- NULL
  if (length(geeglm.obj$xlevels) != 0){
    #xs.factor <- names(geeglm.obj$xlevels)[sapply(geeglm.obj$xlevels, function(x){length(x) > 2})]
    xs.factor <- names(geeglm.obj$xlevels)
  }
  
  ## rownames
  geeglm.obj$data <- data.table::data.table(geeglm.obj$data)
  # nomiss <- stats::complete.cases(geeglm.obj$data[, c(y, xs), with = F])
  # basemodel <- stats::update(geeglm.obj, stats::formula(paste(c(". ~ .", xs), collapse = " - ")), data = geeglm.obj$data)
  # gee.uni.list <- lapply(xs, function(x){geeUni(y, x, data = geeglm.obj$data[nomiss, ], id.vec = geeglm.obj$id, family = family.gee, cor.type = corstr.gee)})
  if (is.null(data_for_univariate)) {
    basemodel <- stats::update(geeglm.obj, stats::formula(paste(c(". ~ .", xs), collapse = " - ")), data = geeglm.obj$data)
    # gee.uni.list <- lapply(xs, function(x){geeUni(y, x, data = geeglm.obj$data[nomiss, ], id.vec = geeglm.obj$id, family = family.gee, cor.type = corstr.gee)})
    gee.uni.list <- lapply(xs, function(x) {
      summary(stats::update(basemodel, stats::formula(paste0(". ~ . +", x)), data = geeglm.obj$data))$coefficients[-1, -3]
    })
    rn.uni <- lapply(gee.uni.list, rownames)
    gee.uni <- Reduce(rbind, gee.uni.list)
  }
    else {
      gee.uni.list <- lapply(xs, function(x) {
        idx <- complete.cases(data_for_univariate[, c(y, x), drop = FALSE])
        df_uni <- data_for_univariate[idx, ]
        id_var <- all.vars(geeglm.obj$call$id)[1]
        id_uni <- df_uni[[id_var]]
        fit_uni <- geepack::geeglm(
          as.formula(paste(y, "~", x)),
          data   = df_uni,
          id     = id_uni,
          family = family.gee,
          corstr = corstr.gee
        )
        summary(fit_uni)$coefficients[-1, -3]
      })
      rn.uni <- lapply(gee.uni.list, rownames)
      gee.uni <- Reduce(rbind, gee.uni.list)
    }

  if (length(xs) == 1) {
    gee.res <- geeExp(gee.uni, family = family.gee, dec = decimal)
    gee.res.list <- lapply(1:length(xs), function(x) {
      gee.res[rownames(gee.uni) %in% rn.uni[[x]], ]
    })
    varnum.mfac <- which(lapply(gee.res.list, length) > ncol(gee.res))
    lapply(varnum.mfac, function(x) {
      gee.res.list[[x]] <<- rbind(rep(NA, ncol(gee.res)), gee.res.list[[x]])
    })
    gee.res.modi <- Reduce(rbind, gee.res.list)
    if (nrow(gee.uni) == 1) {
      gee.res.modi <- t(data.frame(gee.res.modi))
    }

    family.label <- colnames(gee.res.modi)[1]
    colnames(gee.res.modi) <- c(paste(family.label, "(95%CI)", sep = ""), "P value")
  } else {
    if (is.null(pcut.univariate)){
      # When no pcut, use all variables but may need to refit with data_for_univariate
      if (!is.null(data_for_univariate)) {
        # Refit model with data_for_univariate
        idx_multi <- complete.cases(data_for_univariate[, c(y, xs), drop = FALSE])
        df_multi <- data_for_univariate[idx_multi, ]
        id_var <- all.vars(geeglm.obj$call$id)[1]
        id_multi <- df_multi[[id_var]]
        
        refitted_model <- geepack::geeglm(formula(geeglm.obj), 
                                          data = df_multi, 
                                          family = geeglm.obj$family[[1]], 
                                          id = id_multi, 
                                          corstr = geeglm.obj$corstr)
        gee.multi <- summary(refitted_model)$coefficients[-1, -3]
        final_model <- refitted_model  # Update final_model
      } else {
        gee.multi <- summary(geeglm.obj)$coefficients[-1, -3]
      }
      gee.na <- gee.multi
    }else{
      significant_vars <- rownames(gee.uni)[as.numeric(gee.uni[, 3]) < pcut.univariate]

      
      if (!is.null(xs.factor) && length(xs.factor) > 0){
        factor_vars_list <- lapply(xs.factor, function(factor_var) {
          factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
          factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
          
          #matches <- grep(paste0("^", factor_var_escaped), rownames(coefNA(model)), value = TRUE)
          matches <- grep(paste0("^", factor_var_escaped), rownames(gee.uni), value = TRUE)
  
          return(matches)
        })
        names(factor_vars_list) <- xs.factor
        
        for (key in names(factor_vars_list)) {
          variables <- factor_vars_list[[key]]
          
          p_values <- gee.uni[variables, 3]
          
          if (any(p_values < pcut.univariate, na.rm = TRUE)) {
            significant_vars <- setdiff(significant_vars, variables)
            
            significant_vars <- unique(c(significant_vars, key))
          }
        }
      }
      
      selected_formula <- as.formula(paste(y, "~", paste(significant_vars, collapse = " + ")))
      
      # Use data_for_univariate if provided, otherwise use geeglm.obj$data
      if (!is.null(data_for_univariate)) {
        # Filter for complete cases of selected variables
        idx_multi <- complete.cases(data_for_univariate[, c(y, significant_vars), drop = FALSE])
        df_multi <- data_for_univariate[idx_multi, ]
        id_var <- all.vars(geeglm.obj$call$id)[1]
        id_multi <- df_multi[[id_var]]
        
        selected_model <- geepack::geeglm(selected_formula, 
                                          data = df_multi, 
                                          family = family.gee, 
                                          id = id_multi, 
                                          corstr = corstr.gee)
        final_model <- selected_model  # Update final_model
      } else {
        selected_model <- geepack::geeglm(selected_formula, 
                                          data = geeglm.obj$data, 
                                          family = family.gee, 
                                          id = geeglm.obj$id, 
                                          corstr = corstr.gee)
        final_model <- selected_model  # Update final_model
      }
      
      gee.multi <- summary(selected_model)$coefficients[-1, -3]
      
      
      gee.na <- as.data.frame(matrix(NA, nrow = nrow(gee.uni), ncol = ncol(gee.uni)))
      colnames(gee.na) <- colnames(gee.uni)
      rownames(gee.na) <- rownames(gee.uni)
      
      row_indices <- match(rownames(gee.multi), rownames(gee.uni))
      gee.na[row_indices, ] <- gee.multi
      
    }
    
    gee.res <- cbind(geeExp(gee.uni, family = family.gee, dec = decimal), geeExp(gee.na, family = family.gee, dec = decimal))
    gee.res.list <- lapply(1:length(xs), function(x) {
      gee.res[rownames(gee.uni) %in% rn.uni[[x]], ]
    })
    varnum.mfac <- which(lapply(gee.res.list, length) > ncol(gee.res))
    lapply(varnum.mfac, function(x) {
      gee.res.list[[x]] <<- rbind(rep(NA, ncol(gee.res)), gee.res.list[[x]])
    })
    gee.res.modi <- Reduce(rbind, gee.res.list)
    family.label <- colnames(gee.res.modi)[1]
    colnames(gee.res.modi) <- c(paste("crude ", family.label, "(95%CI)", sep = ""), "crude P value", paste("adj. ", family.label, "(95%CI)", sep = ""), "adj. P value")
  }



  rn.list <- lapply(1:length(xs), function(x) {
    rownames(gee.uni)[rownames(gee.uni) %in% rn.uni[[x]]]
  })
  varnum.2fac <- which(lapply(xs, function(x) {
    length(geeglm.obj$xlevels[[x]])
  }) == 2)
  lapply(varnum.2fac, function(x) {
    rn.list[[x]] <<- paste(xs[x], ": ", geeglm.obj$xlevels[[xs[x]]][2], " vs ", geeglm.obj$xlevels[[xs[x]]][1], sep = "")
  })
  lapply(varnum.mfac, function(x) {
    rn.list[[x]] <<- c(paste(xs[x], ": ref.=", geeglm.obj$xlevels[[xs[x]]][1], sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
  })


  rownames(gee.res.modi) <- unlist(rn.list)
  # out = as.data.frame(gee.res.modi)
  # lapply(seq(2, ncol(gee.res), by =2), function(x){out[, x] <<- as.numeric(as.vector(out[, x]))})
  out <- gee.res.modi
  pv.colnum <- which(colnames(out) %in% c("P value", "crude P value", "adj. P value"))
  for (i in pv.colnum) {
    out[, i] <- ifelse(as.numeric(out[, i]) < 0.001, "< 0.001", round(as.numeric(out[, i]), decimal + 1))
  }

  ## Metric
  # final_model is already set to the appropriate model
  info.gee <- as.character(c(NA, round(as.numeric(summary(final_model)$corr[1]), decimal + 1), length(unique(final_model$id)), length(final_model$y)))
  info.df <- cbind(info.gee, matrix(NA, 4, ncol(gee.res) - 1))
  colnames(info.df) <- colnames(out)
  # lapply(seq(2, ncol(gee.res), by =2), function(x){info.df[, x] <<- as.numeric(info.df[, x])})
  rownames(info.df) <- c("", "Estimated correlation parameters", "No. of clusters", "No. of observations")

  ## Caption
  cap.gee <- paste("GEE(", family.gee, ") predicting ", y, " by ", paste(xs, collapse = ", "), " - Group ", as.character(geeglm.obj$call$id)[length(as.character(geeglm.obj$call$id))], sep = "")

  return(list(caption = cap.gee, table = out, metric = info.df))
}
