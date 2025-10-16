#' @title cox2.display: table for coxph.object with model option: TRUE - allow "frailty" or "cluster" model
#' @description Table for coxph.object with model option: TRUE - allow "frailty" or "cluster" model
#' @param cox.obj.withmodel coxph.object with model option: TRUE
#' @param dec Decimal point, Default: 2
#' @param event_msm Character or numeric vector of destination states to keep (multi-state models only)
#' @param pcut.univariate pcut.univariate, Default: NULL
#' @param data_for_univariate data for univariate model, Default: NULL
#' @return Table, cluster/frailty info, metrics, caption
#' @details GEE like - cluster, Mixed effect model like - frailty
#' @examples
#' library(survival)
#' data(lung)
#' fit1 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), data = lung, model = TRUE)
#' fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), data = lung, model = TRUE)
#' cox2.display(fit1)
#' cox2.display(fit2)
#' @rdname cox2.display
#' @export
#' @importFrom survival coxph cluster frailty Surv
#' @importFrom data.table is.data.table as.data.table .SD
#' @importFrom stats formula update AIC na.omit setNames
#'
#'


cox2.display <- function(cox.obj.withmodel, dec = 2, event_msm = NULL, pcut.univariate = NULL, data_for_univariate = NULL) {
  if (!is.null(data_for_univariate) && !data.table::is.data.table(data_for_univariate)) {
    data_for_univariate <- data.table::as.data.table(data_for_univariate)
  }
  model <- cox.obj.withmodel
  if (!any(class(model) == "coxph")) {
    stop("Model not from Cox model")
  }
  model_states <- model$states
  if (!is.null(model_states)) {
    model_states <- as.character(model_states)
    allowed_idx    <- which(model_states != "(s0)")
    allowed_labels <- model_states[allowed_idx]
  } else {
    allowed_idx <- integer()
    allowed_labels <- character()
  }
  
  if (is.null(model_states) && !is.null(event_msm)) {
    stop("'event_msm' can only be used when the model includes multi-state information")
  }
  filtered_state_labels <- NULL
  rn.uni_filtered <- NULL
  use_event_filter <- FALSE
  xf <- attr(model$terms, "term.labels") # Independent vars
  xf_keep <- xf
  xf.old <- xf
  xc <- NULL
  xc.vn <- NULL
  x.weight <- model$call$weight
  mtype <- "normal"
  x.id <- model$call$id
  
  if (length(grep("strata", xf)) > 0) {
    xf <- xf[-grep("strata", xf)]
  } else if (length(grep("frailty\\(", xf)) > 0) {
    xf <- xf[-grep("frailty\\(", xf)]
    mtype <- "frailty"
    xc <- setdiff(xf.old, xf)
    xc.vn <- strsplit(strsplit(xc, "frailty\\(")[[1]][2], "\\)")[[1]][1]
  } else if (!(is.null(model$call$cluster))) {
    mtype <- "cluster"
    # xfull <-  strsplit(as.character(model$call[[2]][3]), " \\+ ")[[1]]
    # xc <- setdiff(xfull, xf)
    # xf <- ifelse(length(grep("cluster\\(",xf)) > 0, xf[-grep("cluster\\(",xf)], xf)
    # xc <- setdiff(xf.old, xf)
    xc <- as.character(model$call$cluster)
    xc.vn <- xc
    # xc.vn <- strsplit(strsplit(xc, "cluster\\(")[[1]][2], "\\)")[[1]][1]
  }
  
  # handle no event case
  if (model$nevent == 0) {
    # metrics
    no.obs <- model$n
    no.event <- model$nevent
    aic <- NA # no event
    c_index <- "NA (NA)" # no event
    
    # vars
    var_names <- xf  # variable names
    if (length(var_names) == 0) var_names <- "No variables"
    if (length(var_names) == 1) {
      col_names <- c("HR(95%CI)", "P value")
    } else {
      col_names <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
    }
    fix.all.unlist <- matrix(NA, nrow = length(var_names), ncol = length(col_names), 
                             dimnames = list(var_names, col_names))
    
    # metric table
    metric.mat <- cbind(c(NA, no.obs, no.event, aic, c_index), matrix(NA, 5, max(1, ncol(fix.all.unlist) - 1)))
    rownames(metric.mat) <- c(NA, "No. of observations", "No. of events", "AIC", "C-Index")
    
    # caption
    surv.string <- as.character(attr(model$terms, "variables")[[2]])
    time.var.name <- paste(sapply(2:(length(surv.string) - 1), function(i) paste(surv.string[i])), collapse = ", ")
    status.var.name <- surv.string[length(surv.string)]
    intro <- paste("Cox model on time ('", time.var.name, "') to event ('", status.var.name, "')", sep = "")
    
    # type info
    cvname_for_caption <- if (is.null(xc.vn)) "N/A" else xc.vn
    if (mtype == "cluster") {
      intro <- paste("Marginal", intro, "- Group", cvname_for_caption)
    } else if (mtype == "frailty") {
      intro <- paste("Frailty", intro, "- Group", cvname_for_caption)
    }
    
    # fixed effect
    var_names <- xf
    if (length(var_names) == 0) var_names <- "No variables"
    if (length(var_names) == 1) {
      col_names <- c("HR(95%CI)", "P value")
    } else {
      col_names <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
    }
    fix.all.unlist <- matrix(NA, nrow = length(var_names), ncol = length(col_names), 
                             dimnames = list(var_names, col_names))
    
    # random effect
    ranef.mat <- NULL
    if (mtype == "cluster" || mtype == "frailty") {
      ranef.mat <- cbind(c(NA, NA), matrix(NA, 2, max(1, ncol(fix.all.unlist) - 1)))
      if (mtype == "cluster") {
        rownames(ranef.mat) <- c("cluster", xc.vn)
      } else { # frailty
        rownames(ranef.mat) <- c("frailty", xc.vn)
      }
    }
    
    # return
    if (is.null(ranef.mat)) {
      return(list(table = fix.all.unlist, metric = metric.mat, caption = intro))
    } else {
      return(list(table = fix.all.unlist, ranef = ranef.mat, metric = metric.mat, caption = intro))
    }
  }
  
  formula.surv <- as.character(model$formula)[2]
  formula.ranef <- paste(" + ", xc, sep = "")
  mdata <- model$model
  if (length(xc) == 0) {
    formula.ranef <- NULL
  } else {
    names(mdata)[names(mdata) == xc] <- xc.vn
  }
  #categorical_vars <- attr(terms(model), "term.labels")[sapply(mdata[attr(terms(model), "term.labels")], is.factor)]
  categorical_vars <- xf[sapply(mdata[xf], is.factor)]
  
  # if (is.null(data)){
  #  mdata = data.frame(get(as.character(model$call)[3]))
  # } else{
  #  mdata = data.frame(data)
  # }
  
  target_state_idx <- integer()
  target_state_labels <- character()

  if (!is.null(model_states)) {
    if (!is.null(event_msm)) {
      msm_values <- as.vector(event_msm)
      if (length(msm_values) == 0 || all(is.na(msm_values))) {
        stop("event_msm must contain at least one non-missing value.")
      }

      if (length(allowed_idx) == 0) {
        stop("The model does not contain any destination states beyond the origin state '(s0)'.")
      }

      available_states <- paste(sprintf("%d: %s", allowed_idx, allowed_labels), collapse = ", ")

      resolve_state <- function(val) {
        if (is.na(val)) return(NA_integer_)
        if (is.numeric(val)) {
          if (!is.finite(val)) return(NA_integer_)
          if (!isTRUE(all.equal(val, as.integer(val)))) return(NA_integer_)
          idx <- as.integer(val)
          if (idx %in% allowed_idx) return(idx)
          return(NA_integer_)
        }
        val_chr <- trimws(as.character(val))
        if (identical(val_chr, "") || identical(val_chr, "(s0)")) return(NA_integer_)
        match_idx <- match(val_chr, model_states)
        if (!is.na(match_idx) && match_idx %in% allowed_idx) return(match_idx)
        suppressWarnings(idx_chr <- as.integer(val_chr))
        if (!is.na(idx_chr) && idx_chr %in% allowed_idx) return(idx_chr)
        NA_integer_
      }

      single_destination <- length(allowed_idx) == 1
      if (single_destination) {
        allowed_label <- allowed_labels[1]
        valid_tokens  <- unique(c(allowed_label, as.character(allowed_idx)))

        if (length(msm_values) > 1) {
          stop(sprintf("Only one destination state ('%s') is available; event_msm cannot contain multiple values.",
                       allowed_label))
        }

        if (!all(msm_values %in% valid_tokens)) {
          baseline_label <- model_states[1]
          stop(sprintf(
            "event_msm must match the available destination state '%s'. Baseline states such as '%s' are not allowed.",
            allowed_label, baseline_label))
        }

        target_state_idx    <- allowed_idx
        target_state_labels <- allowed_label
      } else {
        state_idx_all <- vapply(msm_values, resolve_state, integer(1), USE.NAMES = FALSE)
        if (anyNA(state_idx_all)) {
          bad_vals <- unique(as.character(msm_values[is.na(state_idx_all)]))
          stop(sprintf(
            "No valid states matched 'event_msm'. event_msm must refer to destination states reported by model$states (excluding '%s'). Available states: %s",
            model_states[1], available_states))
        }
        state_idx <- unique(state_idx_all)

        target_state_idx    <- state_idx
        target_state_labels <- model_states[state_idx]
      }
    }
  } else if (!is.null(event_msm)) {
    stop("'event_msm' can only be used when the model includes multi-state information")
  }

  use_event_filter <- length(target_state_idx) > 0 && length(allowed_idx) > 1

  if (length(xf) == 1) {
    uni.res <- data.frame(summary(model)$coefficients)
    # uni.res <- data.frame(summary(coxph(as.formula(paste("mdata[, 1]", "~", xf, formula.ranef, sep="")), data = mdata))$coefficients)
    rn.uni <- lapply(list(uni.res), rownames)
    names(uni.res)[ncol(uni.res)] <- "p"
    uni.res2 <- NULL
    if (mtype == "normal") {
      uni.res2 <- uni.res[, c(1, 3, 4, 5)]
      if (length(grep("robust.se", names(uni.res))) > 0) {
        uni.res2 <- uni.res[, c(1, 4, 5, 6)]
      }
    } else if (mtype == "cluster") {
      uni.res2 <- uni.res[, c(1, 4, 5, 6)]
    } else {
      uni.res2 <- uni.res[-nrow(uni.res), c(1, 3, 4, 6)]
    }
    fix.all <- coxExp(uni.res2, dec = dec)
    colnames(fix.all) <- c("HR(95%CI)", "P value")

    if (mtype == "frailty") {
      # rownames(fix.all) <- c(names(model$coefficients), "frailty")
      rownames(fix.all) <- names(model$coefficients)
    } else {
      rownames(fix.all) <- names(model$coefficients)
    }

    if (use_event_filter) {
      pattern <- paste(paste0(":", target_state_idx, "$"), collapse = "|")
      row_keep <- grepl(pattern, rownames(fix.all))
      kept_row_names <- rownames(fix.all)[row_keep]

      if (length(kept_row_names) == 0) {
        available_states <- paste(sprintf("%d: %s", seq_along(model_states), model_states), collapse = ", ")
        stop(sprintf("No rows matched the supplied event_msm values. Available states: %s", available_states))
      }

      fix.all <- fix.all[kept_row_names, , drop = FALSE]
      uni.res <- uni.res[kept_row_names, , drop = FALSE]
      rn.uni  <- lapply(rn.uni, function(r) r[r %in% kept_row_names])
      keep_var <- lengths(rn.uni) > 0
      rn.uni   <- rn.uni[keep_var]
      xf_keep  <- xf_keep[keep_var]
      filtered_state_labels <- target_state_labels
    }
  } else {
    countings <- length(unlist(attr(mdata[[1]], "dimnames")[2]))
    mdata2 <- cbind(matrix(sapply(mdata[, 1], `[[`, 1), ncol = countings), mdata[, -1])
    names(mdata2)[1:countings] <- as.character(model$formula[[2]][2:(countings + 1)])
    
    if (!is.null(x.weight)) {
      names(mdata2)[names(mdata2) == "(weights)"] <- as.character(x.weight)
    }
    if (!is.null(xc.vn)) {
      names(mdata2)[ncol(mdata2)] <- xc.vn
    }
    if (!is.null(x.id)){
      names(mdata2)[names(mdata2)== "(id)"] <- as.character(x.id)
    }
    
    if (length(grep("strata", xf.old)) > 0){
      if (grepl(",", grep("strata", xf.old, value = T))){
        strata_v <- grep("strata", xf.old, value = T)
        lab <- mdata2[[strata_v]]
        s <- as.character(lab)
        parts <- strsplit(s, ",\\s*")              # 각 행을 "inst=3","sex=1"로 쪼개기
        # 모든 key 모으기
        keys <- unique(unlist(lapply(parts, function(v) sub("=.*", "", v))))
        out_v <- stats::setNames(as.data.frame(matrix(NA_character_, nrow=length(s), ncol=length(keys))), keys)
        # 값 채우기
        for (i in seq_along(parts)) {
          for (kv in parts[[i]]) {
            kv2 <- strsplit(kv, "=")[[1]]
            out_v[i, kv2[1]] <- kv2[2]
          }
        }
        
        mdata2 <- cbind(mdata2, out_v)
        
      } else {
        strata_v <- grep("strata", xf.old, value = T)
        strata_vars <- sub(".*\\(([^)]+)\\).*", "\\1", strata_v)
        levs <- sub(paste0("^", strata_vars, "="), "", mdata2[[strata_v]])
        if (all(grepl("^-?[0-9.]+$", levs))) {
          mdata2[[strata_vars]] <- as.numeric(levs)
        } else {
          mdata2[[strata_vars]] <- levs
        }
      }
    }
    
    
    
    
    if (!is.null(model_states)) {
      if (is.null(data_for_univariate)) {
        baseformula <- stats::formula(paste(c(". ~ .", xf), collapse = " - "))
        unis <- lapply(xf, function(x) {
          newfit <- update(model, stats::formula(paste(c(baseformula, x), collapse = "+")))
          uni.res <- data.frame(summary(newfit)$coefficients)
          if (grepl(":", x)) {
            uni.res <- uni.res[rownames(uni.res) %in% rownames(summary(model)$coefficients), ]
          } else {
            uni.res <- uni.res[grep(x, rownames(uni.res)), ]
          }
          # uni.res <- uni.res[c(2:nrow(uni.res), 1), ]
          # uni.res <- data.frame(summary(coxph(as.formula(paste("mdata[, 1]", "~", x, formula.ranef, sep="")), data = mdata))$coefficients)
          names(uni.res)[ncol(uni.res)] <- "p"
          uni.res2 <- NULL
          if (mtype == "normal") {
            uni.res2 <- uni.res[, c(1, 3, 4, 5)]
            if (length(grep("robust.se", names(uni.res))) > 0) {
              uni.res2 <- uni.res[, c(1, 4, 5, 6)]
            }
          } else if (mtype == "cluster") {
            uni.res2 <- uni.res[, c(1, 4, 5, 6)]
          } else {
            uni.res2 <- uni.res[, c(1, 3, 4, 6)]
          }
          return(uni.res2)
        })
        keep <- !vapply(unis, is.null, logical(1))
        unis        <- unis[keep]
        xf_keep     <- xf[keep]
        if (length(unis) == 0) stop("All univariate fits failed")
        rn.uni <- lapply(unis, rownames)
        unis2 <- Reduce(rbind, unis)
        uni.res <- unis2
        colnames(uni.res) <- c("coef","se","z","p")
      } else {unis <- lapply(xf, function(x) {
        lhs      <- paste(deparse(model$call$formula[[2]]), collapse = "")
        randTerm <- if (mtype=="cluster") {
          paste0("+cluster(", xc.vn, ")")
        } else if (mtype=="frailty") {
          paste0("+frailty(", xc.vn, ")")
        } else ""
        uni_fmla <- as.formula(paste0(lhs, " ~ ", x, randTerm))
        
        needed   <- all.vars(model$call$formula[[2]])
        if (randTerm!="") needed <- c(needed, xc.vn)
        # Handle both data.frame and data.table
        cols_to_check <- c(needed, x)
        subset_data <- data_for_univariate[, .SD, .SDcols = cols_to_check]
        df_uni <- data_for_univariate[complete.cases(subset_data), ]
        
        # Check if variable has variation
        if (is.factor(df_uni[[x]])) {
          if (length(unique(df_uni[[x]])) <= 1) {
            return(NULL)  # Skip variables with no variation
          }
        }
        
        tryCatch({
          fit_uni <- survival::coxph(uni_fmla, data = df_uni, model = T, na.action = na.omit)
          cm  <- summary(fit_uni)$coefficients
          cols <- c(1,
                    which(colnames(cm) %in% c("se(coef)","robust.se")),
                    which(colnames(cm)=="z"),
                    which(colnames(cm) %in% c("Pr(>|z|)","Pr(>|t|)")))[1:4]
          return(cm[, cols, drop=FALSE])
        }, error = function(e) {
          return(NULL)  # Return NULL if model fails
        })
      })
      keep <- !vapply(unis, is.null, logical(1))
      unis        <- unis[keep]
      xf_keep     <- xf[keep]
      if (length(unis) == 0) stop("All univariate fits failed")
      rn.uni <- lapply(unis, rownames)
      unis2 <- Reduce(rbind, unis)
      uni.res <- unis2
      colnames(uni.res) <- c("coef","se","z","p")
      
      }
      
      if (is.null(pcut.univariate)){
        mul.res <- data.frame(coefNA(model))
      }else{
        # Filter out NA p-values
        p_values <- as.numeric(uni.res[, 4])
        significant_vars <- rownames(uni.res)[!is.na(p_values) & p_values < pcut.univariate]
        
        if (length(categorical_vars) != 0){
          factor_vars_list <- lapply(categorical_vars, function(factor_var) {
            factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
            factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
            
            
            matches <- grep(paste0("^", factor_var_escaped), rownames(coefNA(model)), value = TRUE)
            return(matches)
          })
          names(factor_vars_list) <- categorical_vars
          
          for (key in names(factor_vars_list)) {
            variables <- factor_vars_list[[key]]
            
            # Check which variables are actually in uni.res
            variables_in_uni <- variables[variables %in% rownames(uni.res)]
            
            if (length(variables_in_uni) > 0) {
              p_values <- uni.res[variables_in_uni, 4]
              
              if (any(p_values < pcut.univariate, na.rm = TRUE)) {
                significant_vars <- setdiff(significant_vars, variables_in_uni)
                
                significant_vars <- unique(c(significant_vars, key))
              }
            }
          }
        }
        
        # Store selected_model for metrics if pcut.univariate is used
        selected_model <- NULL
        if (length(significant_vars) == 0 ){
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = ncol(data.frame(coefNA(model))))
          rownames(mul.res) <- rownames(uni.res)
          #colnames(mul.res) <- c("coef","exp.coef.","se.coef.","robust.se" ,"z", "Pr...z..")
          colnames(mul.res) <- colnames(data.frame(coefNA(model)))
        }else{
          if(mtype=="normal"){
            selected_formula <- as.formula(paste(formula.surv, "~", paste(significant_vars, collapse = " + ")))
          }else{
            selected_formula <- as.formula(paste(formula.surv, "~", paste(significant_vars, collapse = " + "),"+",paste0(mtype, '(',xc.vn,')')))
          }
          # Use data_for_univariate if provided, otherwise use mdata2
          data_for_multi <- if (!is.null(data_for_univariate)) data_for_univariate else mdata2
          selected_model <- coxph(selected_formula, data = data_for_multi, model = TRUE) 
          mul <- coefNA(selected_model)
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = ncol(data.frame(coefNA(model))))
          rownames(mul.res) <- rownames(uni.res)
          #colnames(mul.res) <- c("coef","exp.coef.","se.coef.","robust.se" ,"z", "Pr...z..")
          colnames(mul.res) <- colnames(data.frame(coefNA(model)))
          if (!is.null(mul)) {
            mul_no_intercept <- mul[!grepl("Intercept", rownames(mul)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
              mul.res[var, ] <- mul_no_intercept[var,]
            }
          }
        }
        
      }
      
      
      # ==============================================================
      
      if (!use_event_filter) {
        
        uni.res <- uni.res[rownames(uni.res) %in% rownames(mul.res), ]
        colnames(mul.res)[ncol(mul.res)] <- "p"
        
        # Determine the correct SE column from the multivariate model results
        se_col_name <- if ("robust.se" %in% colnames(mul.res)) {
          "robust.se"
        } else if ("se.coef." %in% colnames(mul.res)) {
          "se.coef."
        } else {
          "se(coef)"
        }
        se_col_idx   <- which(colnames(mul.res) == se_col_name)
        
        # Create mul_for_exp with the correct columns for coxExp: coef, se, p
        p_col_idx    <- which(colnames(mul.res) == "p")
        coef_col_idx <- which(colnames(mul.res) == "coef")
        
        # Ensure mul_for_exp has columns in the order coxExp expects: coef, se, p
        mul_for_exp <- mul.res[rownames(uni.res), c(coef_col_idx, se_col_idx, p_col_idx), drop = FALSE]
        
        fix.all <- cbind(
          coxExp(uni.res,    dec = dec),
          coxExp(mul_for_exp, dec = dec)
        )
        colnames(fix.all) <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
        rownames(fix.all) <- rownames(uni.res)
        
      } else {
        
        pattern <- paste(paste0(":", target_state_idx, "$"), collapse = "|")
        row_keep <- grepl(pattern, rownames(uni.res))
        kept_row_names <- rownames(uni.res)[row_keep]
        
        
        if (length(kept_row_names) == 0) {
          available_states <- paste(sprintf("%d: %s", seq_along(model_states), model_states), collapse = ", ")
          stop(sprintf("No rows matched the supplied event_msm values. Available states: %s", available_states))
        }
        
        uni.res <- uni.res[kept_row_names, , drop = FALSE]
        mul.res <- mul.res[kept_row_names, , drop = FALSE]
        colnames(mul.res)[ncol(mul.res)] <- "p"
        
        rownames(uni.res) <- kept_row_names
        rownames(mul.res) <- kept_row_names
        
        
        se_col_name <- if ("robust.se" %in% colnames(mul.res)) {
          "robust.se"
        } else if ("se.coef." %in% colnames(mul.res)) {
          "se.coef."
        } else {
          "se(coef)"
        }
        
        mul_for_exp <- mul.res[rownames(uni.res), c("coef", se_col_name, "p"), drop = FALSE]
        
        fix.all <- cbind(
          coxExp(uni.res,    dec = dec),
          coxExp(mul_for_exp, dec = dec)
        )
        colnames(fix.all) <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
        rownames(fix.all) <- rownames(uni.res)
        
        rn.uni   <- lapply(rn.uni, function(r) r[r %in% kept_row_names])
        keep_var <- lengths(rn.uni) > 0
        rn.uni   <- rn.uni[keep_var]
        xf_keep  <- xf_keep[keep_var]
        filtered_state_labels <- target_state_labels
        
        
        # rn.uni_filtered <- rn.uni
        # rownames(fix.all) <- kept_row_names
        if (length(target_state_idx) == 1) {
            suffix_pattern <- paste0(":", target_state_idx, "$")
            row_map_vals <- sub(suffix_pattern, "", kept_row_names)
            idx_simple <- !grepl(":", row_map_vals)
            row_map_vals[idx_simple] <- sub("_[0-9]+$", "", row_map_vals[idx_simple])
            row_map <- setNames(row_map_vals, kept_row_names)

            rownames(fix.all) <- row_map[rownames(fix.all)]
            rn.uni <- lapply(rn.uni, function(r) {
              mapped <- row_map[r]
              mapped <- mapped[!is.na(mapped)]
              mapped
            })
          } else {
            rownames(fix.all) <- kept_row_names
          }
          rn.uni_filtered <- rn.uni
      }
      

    } else {
      if (is.null(data_for_univariate)) {
        basemodel <- update(model, stats::formula(paste(c(". ~ .", xf), collapse = " - ")), data = mdata2)
        
        unis <- lapply(xf, function(x) {
          newfit <- update(basemodel, stats::formula(paste0(". ~ . +", x)), data = mdata2)
          uni.res <- data.frame(summary(newfit)$coefficients)
          if (grepl(":", x)) {
            uni.res <- uni.res[rownames(uni.res) %in% rownames(summary(model)$coefficients), ]
          } else {
            uni.res <- uni.res[grep(x, rownames(uni.res)), ]
          }
          # uni.res <- uni.res[c(2:nrow(uni.res), 1), ]
          # uni.res <- data.frame(summary(coxph(as.formula(paste("mdata[, 1]", "~", x, formula.ranef, sep="")), data = mdata))$coefficients)
          names(uni.res)[ncol(uni.res)] <- "p"
          # if ("robust.se" %in% names(uni.res)) {
          #   uni.res$robust.se <- NULL
          # }
          
          uni.res2 <- NULL
          if (mtype == "normal") {
            uni.res2 <- uni.res[, c(1, 3, 4, 5)]
            if (length(grep("robust.se", names(uni.res))) > 0) {
              uni.res2 <- uni.res[, c(1, 4, 5, 6)]
            }
          } else if (mtype == "cluster") {
            uni.res2 <- uni.res[, c(1, 4, 5, 6)]
          } else {
            uni.res2 <- uni.res[, c(1, 3, 4, 6)]
          }
          return(uni.res2)
        })
        keep <- !vapply(unis, is.null, logical(1))
        unis        <- unis[keep]
        xf_keep     <- xf[keep]
        if (length(unis) == 0) stop("All univariate fits failed")
        rn.uni <- lapply(unis, rownames)
        unis2 <- Reduce(rbind, unis)
        uni.res <- unis2
        colnames(uni.res) <- c("coef","se","z","p")
      } else {
        unis <- lapply(xf, function(x) {
          lhs      <- paste(deparse(model$call$formula[[2]]), collapse = "")
          randTerm <- if (mtype=="cluster") {
            paste0("+cluster(", xc.vn, ")")
          } else if (mtype=="frailty") {
            paste0("+frailty(", xc.vn, ")")
          } else ""
          uni_fmla <- as.formula(paste0(lhs, " ~ ", x, randTerm))
          
          needed   <- all.vars(model$call$formula[[2]])
          if (randTerm!="") needed <- c(needed, xc.vn)
          # Handle both data.frame and data.table
          cols_to_check <- c(needed, x)
          subset_data <- data_for_univariate[, .SD, .SDcols = cols_to_check]
          df_uni <- data_for_univariate[complete.cases(subset_data), ]
          
          # Check if variable has variation
          if (is.factor(df_uni[[x]])) {
            if (length(unique(df_uni[[x]])) <= 1) {
              return(NULL)  # Skip variables with no variation
            }
          }
          
          tryCatch({
            fit_uni  <- survival::coxph(uni_fmla,
                                        data      = df_uni,
                                        model     = TRUE,
                                        na.action = na.omit)
            
            cm       <- summary(fit_uni)$coefficients
            cols     <- c(1,
                          which(colnames(cm) %in% c("se(coef)","robust.se")),
                          which(colnames(cm)=="z"),
                          which(colnames(cm) %in% c("Pr(>|z|)","Pr(>|t|)")))[1:4]
            return(cm[, cols, drop=FALSE])
          }, error = function(e) {
            return(NULL)  # Return NULL if model fails
          })
        })
        keep <- !vapply(unis, is.null, logical(1))
        unis        <- unis[keep]
        xf_keep     <- xf[keep]
        if (length(unis) == 0) stop("All univariate fits failed")
        rn.uni <- lapply(unis, rownames)
        unis2 <- Reduce(rbind, unis)
        uni.res <- unis2
        colnames(uni.res) <- c("coef","se","z","p")
      }
      
      
      if(is.null(pcut.univariate)){
        mul.res <- data.frame(coefNA(model))
      }else{
        
        # Filter out NA p-values
        p_values <- as.numeric(uni.res[, 4])
        significant_vars <- rownames(uni.res)[!is.na(p_values) & p_values < pcut.univariate]
        
        if (length(categorical_vars) != 0){
          factor_vars_list <- lapply(categorical_vars, function(factor_var) {
            factor_var_escaped <- gsub("\\(", "\\\\(", factor_var)  # "(" → "\\("
            factor_var_escaped <- gsub("\\)", "\\\\)", factor_var_escaped)  # ")" → "\\)"
            
            
            matches <- grep(paste0("^", factor_var_escaped), rownames(coefNA(model)), value = TRUE)
            return(matches)
          })
          names(factor_vars_list) <- categorical_vars
          
          for (key in names(factor_vars_list)) {
            variables <- factor_vars_list[[key]]
            
            # Check which variables are actually in uni.res
            variables_in_uni <- variables[variables %in% rownames(uni.res)]
            
            if (length(variables_in_uni) > 0) {
              p_values <- uni.res[variables_in_uni, 4]
              
              if (any(p_values < pcut.univariate, na.rm = TRUE)) {
                significant_vars <- setdiff(significant_vars, variables_in_uni)
                
                significant_vars <- unique(c(significant_vars, key))
              }
            }
          }
        }
        
        # Store selected_model for metrics if pcut.univariate is used
        selected_model <- NULL
        if (length(significant_vars) == 0 ){
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = ncol(data.frame(coefNA(model))))
          rownames(mul.res) <- rownames(uni.res)
          #colnames(mul.res) <- c("coef","exp.coef.","se.coef.","robust.se" ,"z", "Pr...z..")
          colnames(mul.res) <- colnames(data.frame(coefNA(model)))
        }else{
          if(mtype=="normal"){
            selected_formula <- as.formula(paste(formula.surv, "~", paste(significant_vars, collapse = " + ")))
          }else{
            selected_formula <- as.formula(paste(formula.surv, "~", paste(significant_vars, collapse = " + "),"+",paste0(mtype, '(',xc.vn,')')))
          }
          # Use data_for_univariate if provided, otherwise use mdata2
          data_for_multi <- if (!is.null(data_for_univariate)) data_for_univariate else mdata2
          selected_model <- coxph(selected_formula, data = data_for_multi, model = TRUE) 
          mul <- coefNA(selected_model)
          mul.res <- matrix(NA, nrow = nrow(uni.res), ncol = ncol(data.frame(coefNA(model))))
          rownames(mul.res) <- rownames(uni.res)
          #colnames(mul.res) <- c("coef","se(coef)","se2","Chisq","DF","p" )
          colnames(mul.res) <- colnames(data.frame(coefNA(model)))
          if (!is.null(mul)) {
            mul_no_intercept <- mul[!grepl("Intercept", rownames(mul)), , drop = FALSE]
            
            
            for (var in rownames(mul_no_intercept)) { 
              mul.res[var, ] <- mul_no_intercept[var,]
            }
          }
          
        }
        
      }
      
      uni.res <- uni.res[rownames(uni.res) %in% rownames(mul.res), ]
      colnames(mul.res)[ncol(mul.res)] <- "p"
      
      # Determine the correct SE column from the multivariate model results
      se_col_name <- if ("robust.se" %in% colnames(mul.res)) {
        "robust.se"
      } else if ("se.coef." %in% colnames(mul.res)) {
        "se.coef."
      } else {
        "se(coef)"
      }
      se_col_idx <- which(colnames(mul.res) == se_col_name)
      
      # Create mul_for_exp with the correct columns for coxExp: coef, se, p
      p_col_idx <- which(colnames(mul.res) == "p")
      coef_col_idx <- which(colnames(mul.res) == "coef")
      
      # Ensure mul_for_exp has columns in the order coxExp expects: coef, se, p
      mul_for_exp <- mul.res[rownames(uni.res), c(coef_col_idx, se_col_idx, p_col_idx), drop = FALSE]
      
      fix.all <- cbind(
        coxExp(uni.res,    dec = dec),
        coxExp(mul_for_exp, dec = dec)
      )
      colnames(fix.all) <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
      rownames(fix.all) <- rownames(uni.res)
    }
  }
  ## rownames
  
  xf_keep <- xf_keep[!grepl("^strata\\(", xf_keep)]
  
  fix.all.list <- lapply(seq_along(xf_keep), function(x) {
    fix.all[rownames(fix.all) %in% rn.uni[[x]], ]
  })
  varnum.mfac_candidates <- which(lapply(fix.all.list, length) > ncol(fix.all))
  categorical_vars_keep <- intersect(xf_keep, categorical_vars)
  varnum.mfac <- varnum.mfac_candidates[xf_keep[varnum.mfac_candidates] %in% categorical_vars_keep]
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)
  
  # if (!is.null(model_states) && use_event_filter && !is.null(rn.uni_filtered)) {
  #   rn.list <- rn.uni_filtered
  # } else {
  #   rn.list <- lapply(seq_along(xf_keep), function(x) {
  #     rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
  #   })
  #   varnum.2fac <- which(lapply(xf, function(x) {
  #     length(sapply(mdata, levels)[[x]])
  #   }) == 2)
  #   lapply(varnum.2fac, function(x) {
  #     rn.list[[x]] <<- paste(xf[x], ": ", levels(mdata[, xf[x]])[2], " vs ", levels(mdata[, xf[x]])[1], sep = "")
  #   })
  #   lapply(varnum.mfac, function(x) {
  #     if (grepl(":", xf[x])) {
  #       a <- unlist(strsplit(xf[x], ":"))[1]
  #       b <- unlist(strsplit(xf[x], ":"))[2]
  # 
  #       if (a %in% xf && b %in% xf) {
  #         ref <- paste0(a, levels(mdata[, a])[1], ":", b, levels(mdata[, b])[1])
  #         rn.list[[x]] <<- c(paste(xf[x], ": ref.=", ref, sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
  #       } else {
  #         rn.list[[x]] <<- c(paste(xf[x], ": ref.=NA", model$xlevels[[xf[x]]][1], sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
  #       }
  #     } else {
  #       rn.list[[x]] <<- c(paste(xf[x], ": ref.=", levels(mdata[, xf[x]])[1], sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
  #     }
  #   })
  # }
  
    rn.list <- lapply(seq_along(xf_keep), function(x) {
      rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
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
          rn.list[[x]] <<- c(paste(xf[x], ": ref.=", ref, sep = ""), rn.list[[x]])
        } else {
          rn.list[[x]] <<- c(paste(xf[x], ": ref.=NA", model$xlevels[[xf[x]]][1], sep = ""), rn.list[[x]])
        }
      } else {
        rn.list[[x]] <<- c(paste(xf[x], ": ref.=", levels(mdata[, xf[x]])[1], sep = ""), rn.list[[x]])
      }
    })
    
    # if (!is.null(model_states) && use_event_filter && !is.null(rn.uni_filtered)) {
    #   rn.list <- rn.uni_filtered
    
    # if(!is.null(model_states) && length(event_msm) == length(model_states)-1 ){
    #   rownames(fix.all.unlist) <- unlist(rn.list)
    # }
  
  
  
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rn_vector <- unlist(rn.list)
  if (length(rn_vector) == nrow(fix.all.unlist)) {
    rownames(fix.all.unlist) <- rn_vector
  } else if (is.null(model_states)) {
    rownames(fix.all.unlist) <- rn_vector
  }
  
  pv.colnum <- which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  for (i in pv.colnum) {
    fix.all.unlist[, i] <- ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), dec + 1))
  }
  
  
  ## random effect
  # ranef = unlist(model$vcoef)
  # ranef.out = round(ranef, dec)
  ranef.mat <- NULL
  if (mtype == "cluster") {
    ranef.mat <- cbind(c(NA, NA), matrix(NA, length(xc) + 1, ncol(fix.all) - 1))
    # clname = strsplit(xc, "\\(")[[1]]
    clname <- c("cluster", xc)
    cvname <- strsplit(paste(clname[-1], collapse = "("), "\\)")[[1]]
    cvname <- paste(cvname[length(cvname)], collapse = ")")
    rownames(ranef.mat) <- c(clname[1], cvname)
  } else if (mtype == "frailty") {
    ranef.mat <- cbind(c(NA, NA), matrix(NA, length(xc) + 1, ncol(fix.all) - 1))
    clname <- strsplit(xc, "\\(")[[1]]
    cvname <- strsplit(paste(clname[-1], collapse = "("), "\\)")[[1]]
    cvname <- paste(cvname[length(cvname)], collapse = ")")
    rownames(ranef.mat) <- c(clname[1], cvname)
  }
  
  
  ## metric
  # Use selected_model metrics if pcut.univariate was applied, otherwise use original model
  metric_model <- if (!is.null(pcut.univariate) && exists("selected_model") && !is.null(selected_model)) selected_model else model
  
  no.obs <- metric_model$n
  no.event <- metric_model$nevent
  aic <- stats::AIC(metric_model)
  ccd <- metric_model$concordance
  concordance_value <- round(ccd["concordance"], 3)
  std_value <- round(ccd["std"], 3)
  c_index <- paste0(concordance_value, "(", std_value, ")")
  metric.mat <- cbind(c(NA, no.obs, no.event, aic, c_index), matrix(NA, 5, ncol(fix.all) - 1))
  rownames(metric.mat) <- c(NA, "No. of observations", "No. of events", "AIC", "C-Index")
  
  ## Integrated ll
  # ll = model$loglik[2]
  # aic = -2 * ll -2*model$df[1]
  
  ## caption
  surv.string <- as.character(attr(model$terms, "variables")[[2]])
  time.var.name <- paste(sapply(2:(length(surv.string) - 1), function(i) paste(surv.string[i])), collapse = ", ")
  status.var.name <- surv.string[length(surv.string)]
  intro <- paste("Cox model on time ('", time.var.name, "') to event ('", status.var.name, "')", sep = "")
  if (mtype == "cluster") {
    intro <- paste("Marginal", intro, "- Group", cvname)
  } else if (mtype == "frailty") {
    intro <- paste("Frailty", intro, "- Group", cvname)
  }
  if (!is.null(model_states)) {
    states <- paste(sapply(seq_along(model_states), function(i) paste(i, model_states[i], sep = ": ")), collapse = ", ")
    intro[2] <- paste("states", states)
    if (!is.null(filtered_state_labels)) {
      intro <- c(intro, paste("filtered states:", paste(filtered_state_labels, collapse = ", ")))
    }
  }
  
  var.names0 <- attr(model$terms, "term.labels")
  if (length(grep("strata", var.names0)) > 0) {
    intro <- paste(intro, " with '", var.names0[grep("strata", var.names0)], "'", sep = "")
  }
  
  if (is.null(ranef.mat)) {
    return(list(table = fix.all.unlist, metric = metric.mat, caption = intro))
  } else {
    return(list(table = fix.all.unlist, ranef = ranef.mat, metric = metric.mat, caption = intro))
  }
}
