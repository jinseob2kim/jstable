## Label data


#' @title Export label and level: one variable
#' @description Export label and level: one variable
#' @param data data
#' @param vname variable to export label and level
#' @return if continuous variable - (label, NA), categorical variable - (label, level)
#' @details DETAILS
#' @examples
#' lapply(names(iris), function(x) {
#'   jstable::mk.lev.var(iris, x)
#' })
#' @rdname mk.lev.var
#' @export

mk.lev.var <- function(data, vname) {
  v.vec <- data[[vname]]
  out <- ""
  if (is.numeric(v.vec)) {
    out <- c(vname, class(v.vec), NA)
  } else {
    v.level <- levels(v.vec)
    nr <- length(v.level)
    out <- cbind(rep(vname, nr), rep(class(v.vec), nr), v.level)
  }
  return(out)
}



#' @title Export label and level: multiple variable
#' @description Export label and level: multiple variable
#' @param data data
#' @return default label and level data
#' @details DETAILS
#' @examples
#' mk.lev(iris)
#' @rdname mk.lev
#' @export
#' @importFrom data.table data.table :=

mk.lev <- function(data) {
  variable <- level <- val_label <- NULL

  out.list <- lapply(names(data), function(x) {
    mk.lev.var(data, x)
  })
  out.dt <- data.table::data.table(Reduce(rbind, out.list))
  if (ncol(out.dt) != 3) {
    out.dt <- data.table::data.table(t(out.dt))
  }
  names(out.dt) <- c("variable", "class", "level")
  out.dt[, var_label := variable]
  out.dt[, val_label := level]
  return(out.dt[])
}






#' @title LabelepiDisplay: Apply label information to epiDisplay object using label data
#' @description Apply label information to epiDisplay.object using label data
#' @param epiDisplay.obj epiDisplay.object or glmshow.object
#' @param label Apply label information, Default: F
#' @param ref Label data made by mk.lev function
#' @return epiDisplay.object with label information
#' @details DETAILS
#' @examples
#' fit <- glm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#' fit.table <- glmshow.display(fit)
#' iris.label <- mk.lev(iris)
#' LabelepiDisplay(fit.table, label = TRUE, ref = iris.label)
#' @rdname LabelepiDisplay
#' @export
#' @importFrom data.table data.table :=


LabelepiDisplay <- function(epiDisplay.obj, label = F, ref) {
  lv2 <- variable <- level <- val_label <- NULL

  tb.main <- epiDisplay.obj$table
  tb.compact <- tb.main[!rownames(tb.main) == "", ]
  if (nrow(tb.main) <= 2) {
    tb.compact <- tb.main
  }


  ## Var label
  tb.rn <- gsub(" \\(cont. var.\\)", "", rownames(tb.compact))
  rownames(tb.compact) <- tb.rn

  if (nrow(tb.main) < 2 & label == T) {
    vname <- strsplit(rownames(tb.compact)[1], ": ")[[1]][1]
    cond.lv2 <- grepl(": ", rownames(tb.compact)[1]) & grepl(" vs ", rownames(tb.compact)[1])
    rownames(tb.compact) <- gsub(vname, ref[variable == vname, var_label][1], rownames(tb.compact))
    if (cond.lv2) {
      lv2 <- strsplit(strsplit(rownames(tb.compact)[1], ": ")[[1]][[2]], " vs ")[[1]]
      vll <- ref[variable == vname & level %in% lv2, c("level", "val_label")]
      rownames(tb.compact) <- paste(ref[variable == vname, var_label][1], ": ", vll[level == lv2[1], val_label], " vs ", vll[level == lv2[2], val_label], sep = "")
    }
  }

  if (nrow(tb.main) > 2 & label == T) {
    vn <- which(substr(tb.rn, 1, 1) != " ")
    vns <- c(vn, length(tb.rn) + 1)
    vl <- lapply(1:length(vn), function(x) {
      tb.rn[vns[x]:(vns[x + 1] - 1)]
    })
    vl_label <- lapply(vl, function(x) {
      vname <- strsplit(x[1], ": ")[[1]][1]
      cond.lv2 <- grepl(": ", x[1]) & grepl(" vs ", x[1])
      # x[1] <- gsub(vname, ref[variable == vname, var_label][1], x[1])
      if (cond.lv2) {
        lv2 <- strsplit(strsplit(x[1], ": ")[[1]][[2]], " vs ")[[1]]
        vll <- ref[variable == vname & level %in% lv2, c("level", "val_label")]
        x <- paste(ref[variable == vname, var_label][1], ": ", vll[level == lv2[1], val_label], " vs ", vll[level == lv2[2], val_label], sep = "")
        # x = gsub(paste(vll[2, 1], " vs ", vll[1,1], sep=""), paste(vll[2, 2], " vs ", vll[1,2], sep=""), x)
      } else if (ref[variable == vname, class][1] %in% c("factor", "character")) {
        x[1] <- paste(ref[variable == vname, var_label][1], ": ref.=", ref[variable == vname & level == strsplit(x[1], "\\.\\=")[[1]][2], val_label], sep = "")
        for (k in 2:length(x)) {
          x[k] <- paste("   ", ref[variable == vname & level == strsplit(x[k], "   ")[[1]][2], val_label], sep = "")
        }

        # for (y in ref[variable == vname, level]) {x = gsub(y, ref[variable == vname & level == y, val_label], x)}
      }
      return(x)
    })
    rownames(tb.compact) <- unlist(vl_label)
  }

  ll <- strsplit(epiDisplay.obj$last.lines, "\n")[[1]]
  ll.vec <- matrix(unlist(lapply(ll, function(x) {
    strsplit(x, " = ")
  })), ncol = 2, byrow = T)
  ll.mat <- matrix(rep("", nrow(ll.vec) * ncol(tb.compact)), nrow = nrow(ll.vec))
  ll.mat[, 1] <- ll.vec[, 2]
  rownames(ll.mat) <- ll.vec[, 1]
  out <- rbind(tb.compact, rep("", ncol(tb.compact)), ll.mat)

  if (nrow(tb.main) == 2) {
    out <- rbind(tb.compact, ll.mat)
  }

  p.colnum <- which(colnames(out) %in% c("P value", "adj. P value", "P(t-test)", "P(Wald's test)"))
  p.colnum <- p.colnum[length(p.colnum)]

  pn <- gsub("< ", "", out[, p.colnum])

  colnames(out)[p.colnum] <- ifelse(colnames(out)[p.colnum] == "P value", "P value", "adj. P value")
  sig <- ifelse(as.numeric(pn) <= 0.05, "**", "")
  return(cbind(out, sig))
}





#' @title LabeljsTable: Apply label information to jstable object using label data
#' @description Apply label information to table of geeglm.display, lmer.display, coxme.display using label data
#' @param obj.table table of geeglm.display, lmer.display, coxme.display
#' @param ref Label data made by mk.lev function
#' @return table of geeglm.display, lmer.display, coxme.display with label information
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1 | inst) + (1 | sex), lung)
#' fit.table <- coxme.display(fit)
#' lung.label <- mk.lev(lung)
#' LabeljsTable(fit.table$table, ref = lung.label)
#' @rdname LabeljsTable
#' @export
#' @importFrom data.table data.table :=

LabeljsTable <- function(obj.table, ref) {
  lv2 <- variable <- level <- val_label <- NULL

  tb.main <- obj.table
  tb.compact <- tb.main

  ## Var label
  tb.rn <- rownames(tb.compact)

  if (nrow(tb.main) == 1) {
    vname <- strsplit(rownames(tb.compact)[1], ": ")[[1]][1]
    cond.lv2 <- grepl(":", rownames(tb.compact)[1]) & grepl(" vs ", rownames(tb.compact)[1])
    rownames(tb.compact) <- gsub(vname, ref[variable == vname, var_label][1], rownames(tb.compact))
    if (cond.lv2) {
      lv2 <- strsplit(strsplit(rownames(tb.compact)[1], ": ")[[1]][[2]], " vs ")[[1]]
      vll <- ref[variable == vname & level %in% lv2, c("level", "val_label")]
      rownames(tb.compact) <- paste(ref[variable == vname, var_label][1], ": ", vll[level == lv2[1], val_label], " vs ", vll[level == lv2[2], val_label], sep = "")
    }
  }

  if (nrow(tb.main) > 1) {
    vn <- which(substr(tb.rn, 1, 1) != " ")
    vns <- c(vn, length(tb.rn) + 1)
    vl <- lapply(1:length(vn), function(x) {
      tb.rn[vns[x]:(vns[x + 1] - 1)]
    })
    vl_label <- lapply(vl, function(x) {
      vname <- strsplit(x[1], ": ")[[1]][1]
      
      # Check if it's an interaction term coefficient (like wt:vsStraight or wt:cyl)
      # Interaction coefficients contain ":" but not " vs " or "ref.="
      is_interaction_coef <- grepl(":", vname) && !grepl(" vs ", x[1]) && !grepl("ref\\.=", x[1])
      
      if (is_interaction_coef) {
        # This is an interaction coefficient (e.g., "wt:vsStraight" or "wt:cyl6")
        # We need to handle this specially
        
        # Split by colon to find the parts
        parts <- strsplit(vname, ":")[[1]]
        
        # For each part, check if it's a variable or a variable+level
        processed_parts <- sapply(parts, function(part) {
          # Check if this part is a base variable
          if (part %in% ref$variable) {
            # It's a base variable, replace with label
            label <- ref[variable == part, var_label][1]
            return(if (is.na(label)) part else label)
          } else {
            # It might be variable+level (like "vsStraight" or "cyl6")
            # Try to find which variable this belongs to
            for (var in unique(ref$variable)) {
              if (startsWith(part, var)) {
                # Found the variable
                var_label <- ref[variable == var, var_label][1]
                if (!is.na(var_label)) {
                  # Extract the level part
                  level_part <- substring(part, nchar(var) + 1)
                  # Check if we have a label for this level
                  level_label <- ref[variable == var & level == level_part, val_label][1]
                  if (!is.na(level_label)) {
                    return(paste0(var_label, ":", level_label))
                  } else {
                    return(paste0(var_label, ":", level_part))
                  }
                }
              }
            }
            # If no match found, return original
            return(part)
          }
        })
        
        # Reconstruct the interaction term
        x[1] <- paste(processed_parts, collapse = ":")
        
      } else if (grepl(":", vname) && grepl("ref\\.=", x[1])) {
        # This is an interaction header with ref (e.g., "wt:cyl: ref.=4")
        interaction_parts <- strsplit(vname, ":")[[1]]
        
        # Replace each part with its label
        interaction_labels <- sapply(interaction_parts, function(part) {
          label <- ref[variable == part, var_label][1]
          if (is.na(label)) part else label
        })
        
        # Reconstruct with ref
        interaction_label <- paste(interaction_labels, collapse = ":")
        ref_part <- strsplit(x[1], "ref\\.=")[[1]][2]
        
        # Try to replace ref level with label
        for (part in interaction_parts) {
          if (part %in% ref$variable) {
            ref_label <- ref[variable == part & level == ref_part, val_label][1]
            if (!is.na(ref_label)) {
              ref_part <- ref_label
              break
            }
          }
        }
        
        x[1] <- paste(interaction_label, ": ref.=", ref_part, sep = "")
        
        # Process remaining rows (factor levels in interaction)
        if (length(x) > 1) {
          for (k in 2:length(x)) {
            # This will be like "   6" or "   8" for interaction levels
            level_str <- trimws(x[k])
            
            # Try to find and replace with label
            for (part in interaction_parts) {
              if (part %in% ref$variable) {
                level_label <- ref[variable == part & level == level_str, val_label][1]
                if (!is.na(level_label)) {
                  x[k] <- paste("   ", level_label, sep = "")
                  break
                }
              }
            }
          }
        }
      } else {
        # Original code for non-interaction terms
        var_label <- ref[variable == vname, var_label][1]
        if (!is.na(var_label)) {
          # Use sub to replace only the first occurrence
          x[1] <- sub(paste0("^", vname), var_label, x[1])
        }
        
        cond.lv2 <- grepl(": ", x[1]) & grepl(" vs ", x[1])
        if (cond.lv2) {
          # Handle "vs: Straight vs V-shaped" format
          # Extract the levels from the current string after var_label replacement
          parts_after_label <- strsplit(x[1], ": ")[[1]]
          if (length(parts_after_label) == 2) {
            # The second part contains "Straight vs V-shaped" or similar
            level_part <- parts_after_label[2]
            # Extract the levels - they should match the original level names
            lv2 <- strsplit(level_part, " vs ")[[1]]
            
            # The levels should be the original factor levels
            # Check if var_label was mistakenly included in the level names
            lv2_clean <- lv2
            if (!is.na(var_label)) {
              lv2_clean <- gsub(var_label, "", lv2)
              lv2_clean <- trimws(lv2_clean)
              # If after removing var_label, we get empty strings, use original
              if (any(lv2_clean == "")) {
                lv2_clean <- lv2
              }
            }
            
            # Now find the val_labels
            vll <- ref[variable == vname & level %in% lv2_clean, c("level", "val_label")]
            if (nrow(vll) >= 2) {
              x[1] <- paste(var_label, ": ", 
                           vll[level == lv2_clean[1], val_label], " vs ", 
                           vll[level == lv2_clean[2], val_label], sep = "")
            } else {
              # If we can't find clean levels, try with original lv2
              vll <- ref[variable == vname & level %in% lv2, c("level", "val_label")]
              if (nrow(vll) >= 2) {
                x[1] <- paste(var_label, ": ", 
                             vll[level == lv2[1], val_label], " vs ", 
                             vll[level == lv2[2], val_label], sep = "")
              }
            }
          }
        } else if (vname %in% ref$variable && ref[variable == vname, class][1] %in% c("factor", "character")) {
          # Handle multi-level factors with ref
          if (grepl("ref\\.=", x[1])) {
            ref_level <- strsplit(x[1], "ref\\.=")[[1]][2]
            ref_label <- ref[variable == vname & level == ref_level, val_label][1]
            if (!is.na(ref_label)) {
              x[1] <- paste(ref[variable == vname, var_label][1], ": ref.=", ref_label, sep = "")
            }
          }
          
          # Process additional rows for factor levels
          if (length(x) > 1) {
            for (k in 2:length(x)) {
              level_str <- trimws(x[k])
              level_label <- ref[variable == vname & level == level_str, val_label][1]
              if (!is.na(level_label)) {
                x[k] <- paste("   ", level_label, sep = "")
              }
            }
          }
        }
      }
      return(x)
    })
    rownames(tb.compact) <- unlist(vl_label)
  }

  out <- tb.compact
  # sig.colnum = which(colnames(out) %in% c("P value", "adj. P value"))
  # pn = gsub("< ","", out[, sig.colnum])
  # sig = ifelse(as.numeric(pn) <= 0.05, "**", "")

  # pv.colnum = which(colnames(out) %in% c("P value", "crude P value", "adj. P value"))
  # for (i in pv.colnum){
  #  out[, i] = ifelse(as.numeric(out[, i]) < 0.001, "< 0.001", round(as.numeric(out[, i]), 3))
  # }
  return(out)
}




#' @title LabeljsRanef: Apply label information to jstable random effect object using label data
#' @description Apply label information to ranef object of jstable using label data
#' @param obj.ranef ranef of lmer.display, coxme.display, cox2.display
#' @param ref Label data made by mk.lev function
#' @return ranef of lmer.display, coxme.display, cox2.display with label information
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1 | inst) + (1 | sex), lung)
#' fit.table <- coxme.display(fit)
#' lung.label <- mk.lev(lung)
#' LabeljsTable(fit.table$table, ref = lung.label)
#' LabeljsRanef(fit.table$ranef, ref = lung.label)
#' @rdname LabeljsRanef
#' @export

LabeljsRanef <- function(obj.ranef, ref) {
  variable <- NULL

  ranef <- obj.ranef
  ranef.split <- strsplit(rownames(ranef)[-1], "\\(")
  ranef.vname <- unlist(lapply(ranef.split, function(x) {
    x[[1]]
  }))
  ranef.vname.label <- sapply(ranef.vname, function(x) {
    ref[variable == x, var_label][1]
  })
  if (length(ranef.split) == 1) {
    rownames(ranef)[-1] <- ranef.vname.label
  } else {
    rownames(ranef)[-1] <- paste(ranef.vname.label, "(", unlist(lapply(ranef.split, function(x) {
      x[[2]]
    })), sep = "")
  }
  return(ranef)
}




#' @title LabeljsMetric: Apply label information to jstable metric object using label data
#' @description Apply label information to metric object of jstable using label data
#' @param obj.metric metric of lmer.display, coxme.display
#' @param ref Label data made by mk.lev function
#' @return metric of lmer.display, coxme.display with label information
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1 | inst) + (1 | sex), lung)
#' fit.table <- coxme.display(fit)
#' lung.label <- mk.lev(lung)
#' LabeljsTable(fit.table$table, ref = lung.label)
#' LabeljsRanef(fit.table$ranef, ref = lung.label)
#' LabeljsMetric(fit.table$metric, ref = lung.label)
#' @rdname LabeljsMetric
#' @export

LabeljsMetric <- function(obj.metric, ref) {
  variable <- NULL

  metric <- obj.metric
  rname <- rownames(metric)
  group.rnum <- grep("No. of group", rname)
  group.vars <- unlist(lapply(strsplit(rname[group.rnum], "\\("), function(x) {
    x[[2]]
  }))
  group.vname <- unlist(strsplit(group.vars, "\\)"))
  group.vname.label <- sapply(group.vname, function(x) {
    ref[variable == x, var_label][1]
  })
  rownames(metric)[group.rnum] <- paste("No. of group(", group.vname.label, ")", sep = "")
  return(metric)
}




#' @title LabeljsMixed: Apply label information to jstable object using label data
#' @description Apply label information to object of jstable using label data
#' @param obj lmer.display, coxme.display
#' @param ref Label data made by mk.lev function
#' @return lmer.display, coxme.display with label information
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1 | inst) + (1 | sex), lung)
#' fit.table <- coxme.display(fit)
#' lung.label <- mk.lev(lung)
#' LabeljsMixed(fit.table, ref = lung.label)
#' @rdname LabeljsMixed
#' @export

LabeljsMixed <- function(obj, ref) {
  variable <- NULL

  out <- list()
  out$table <- LabeljsTable(obj$table, ref = ref)
  out$ranef <- LabeljsRanef(obj$ranef, ref = ref)
  out$metric <- LabeljsMetric(obj$metric, ref = ref)
  out$caption <- obj$caption
  if (grep("Mixed effects Cox model", obj$caption) == 1) {
    surv.vname <- strsplit(obj$caption, "'")[[1]][c(2, 4)]
    for (vn in surv.vname) {
      out$caption <- gsub(paste("'", vn, "'", sep = ""), paste("'", ref[variable == vn, var_label][1], "'", sep = ""), out$caption)
    }
    group.vname.comma <- strsplit(obj$caption, "- Group ")[[1]][2]
    group.vname <- strsplit(group.vname.comma, ", ")[[1]]
    group.vname.label <- sapply(group.vname, function(x) {
      ref[variable == x, var_label][1]
    })
    out$caption <- gsub(group.vname.comma, paste(group.vname.label, collapse = ", "), out$caption)
  }

  return(out)
}



#' @title LabeljsCox: Apply label information to cox2.display object using label data
#' @description Apply label information to cox2.display object using label data
#' @param obj cox2.display object
#' @param ref Label data made by mk.lev function
#' @return cox2.display object with label information
#' @details DETAILS
#' @examples
#' library(survival)
#' fit <- coxph(Surv(time, status) ~ sex + ph.ecog + ph.karno + cluster(inst),
#'   data = lung, model = TRUE
#' )
#' fit.table <- cox2.display(fit)
#' lung.label <- mk.lev(lung)
#' LabeljsCox(fit.table, ref = lung.label)
#' @rdname LabeljsCox
#' @export

LabeljsCox <- function(obj, ref) {
  variable <- NULL

  out <- list()
  out$table <- LabeljsTable(obj$table, ref = ref)
  if (!is.null(obj$ranef)) {
    out$ranef <- LabeljsRanef(obj$ranef, ref = ref)
  }
  out$metric <- obj$metric
  out$caption <- obj$caption
  surv.vname <- strsplit(obj$caption, "'")[[1]][c(2, 4)]
  for (vn in surv.vname) {
    out$caption <- gsub(paste("'", vn, "'", sep = ""), paste("'", ref[variable == vn, var_label][1], "'", sep = ""), out$caption)
  }
  if (length(grep("- Group", obj$caption)) >= 1) {
    group.vname.comma <- strsplit(obj$caption, "- Group ")[[1]][2]
    group.vname <- strsplit(group.vname.comma, ", ")[[1]]
    group.vname.label <- sapply(group.vname, function(x) {
      ref[variable == x, var_label][1]
    })
    out$caption <- gsub(group.vname.comma, paste(group.vname.label, collapse = ", "), out$caption)
  }

  return(out)
}




#' @title LabeljsGeeglm: Apply label information to geeglm.display object using label data
#' @description Apply label information to geeglm.display object using label data
#' @param obj geeglm.display object
#' @param ref Label data made by mk.lev function
#' @return geeglm.display object with label information
#' @details DETAILS
#' @examples
#' library(geepack)
#' library(jstable)
#' data(dietox)
#' dietox$Cu <- as.factor(dietox$Cu)
#' gee01 <- geeglm(Weight ~ Time + Cu,
#'   id = Pig, data = dietox,
#'   family = gaussian, corstr = "ex"
#' )
#' g1 <- geeglm.display(gee01)
#' LabeljsGeeglm(g1, ref = mk.lev(dietox))
#' @rdname LabeljsGeeglm
#' @export

LabeljsGeeglm <- function(obj, ref) {
  variable <- NULL

  out <- list()
  out$table <- LabeljsTable(obj$table, ref = ref)
  out$metric <- obj$metric
  out$caption <- obj$caption
  cap.split <- strsplit(obj$caption, "predicting ")[[1]]
  yxc <- cap.split[2]
  yxc1 <- strsplit(yxc, " by ")[[1]]
  y <- yxc1[1]
  x <- strsplit(yxc1[2], " - Group ")[[1]]
  xx <- strsplit(x[1], ", ")[[1]]
  xc <- x[2]
  out$caption <- paste(cap.split[1], "predicting ", ref[variable == y, var_label][1], " by ", paste(sapply(xx, function(vn) {
    ref[variable == vn, var_label][1]
  }), collapse = ", "), " - Group ", ref[variable == xc, var_label][1], sep = "")

  return(out)
}
