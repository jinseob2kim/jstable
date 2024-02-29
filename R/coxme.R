#' @title coxmeTable: Summary table of coxme.object(internal function)
#' @description Extract fixed effect table in coxme.object
#' @param mod coxme.object
#' @return beta, se, z, p of fixed effects
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), lung)
#' jstable:::coxmeTable(fit)
#' @rdname coxmeTable
#' @importFrom coxme fixef
#' @importFrom stats pnorm


coxmeTable <- function(mod) {
  if (!any(class(mod) == "coxme")) {
    stop("Model not from mixed effects Cox model")
  }
  beta <- coxme::fixef(mod)
  nvar <- length(beta)
  nfrail <- nrow(mod$variance) - nvar
  se <- sqrt(diag(as.matrix(mod$variance))[nfrail + 1:nvar])
  z <- beta / se
  p <- 2 * (1 - pnorm(abs(z)))
  # p<- signif(1 - pchisq((beta/se)^2, 1), 2)
  table <- data.frame(cbind(beta, se, z, p))
  return(table)
}




#' @title coxExp: transform the unit of coefficients in cox model(internal function)
#' @description Transform the unit of coefficients to "HR"
#' @param cox.coef cox model coefficients
#' @param dec Decimal point
#' @return The transforemed coefficients(95% CI), p-value
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), lung)
#' jstable:::coxExp(jstable:::coxmeTable(fit))
#' @rdname coxExp
#' @importFrom stats pnorm qnorm

coxExp <- function(cox.coef, dec) {
  HR <- paste(round(exp(cox.coef[, 1]), dec), " (", round(exp(cox.coef[, 1] - stats::qnorm(0.975) * cox.coef[, 2]), dec), ",", round(exp(cox.coef[, 1] + stats::qnorm(0.975) * cox.coef[, 2]), dec), ")", sep = "")
  pv <- cox.coef[, "p"]
  # pv = 2*(1-pnorm(abs(cox.coef[, "z"])))
  return(cbind(HR, pv))
}



#' @title extractAIC.coxme: Extract AIC from coxme.object
#' @description Extract AIC from coxme.object
#' @param fit coxme.object
#' @param scale NULL
#' @param k numeric specifying the 'weight' of the equivalent degrees of freedom (=: edf) part in the AIC formula.
#' @param ... further arguments (currently unused in base R).
#' @return AIC(Integreted, Penalized)
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), lung)
#' extractAIC(fit)
#' @rdname extractAIC.coxme
#' @export



extractAIC.coxme <- function(fit, scale = NULL, k = 2, ...) {
  loglik <- fit$loglik + c(0, 0, fit$penalty)
  chi1 <- 2 * diff(loglik[1:2])
  chi2 <- 2 * diff(loglik[c(1, 3)])
  c(chi1 - k * fit$df[1], chi2 - k * fit$df[2])
}




#' @title coxme.display: table for coxme.object (coxme package)
#' @description Make mixed effect model results from coxme.object (coxme package)
#' @param coxme.obj coxme.object
#' @param dec Decimal point, Default: 2
#' @return Fixed effect table, random effect, metrics, caption
#' @details DETAILS
#' @examples
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), lung)
#' coxme.display(fit)
#' @rdname coxme.display
#' @export
#' @importFrom coxme coxme

coxme.display <- function(coxme.obj, dec = 2) {
  model <- coxme.obj
  if (!any(class(model) == "coxme")) {
    stop("Model not from mixed effects Cox model")
  }

  xf <- attr(model$terms, "term.labels") # Independent vars
  xstrata <- grep("strata", xf, value = T)
  if (length(xstrata) > 0) {
    xf <- xf[-grep("strata", xf)]
  }

  formula.surv <- as.character(model$formulaList$fixed)[2]
  formula.ranef <- as.character(model$formulaList$random)
  mdata <- data.frame(get(as.character(model$call)[3]))

  if (length(xf) == 1) {
    # uni.res = coxmeTable(coxme(as.formula(paste(formula.surv, "~", xf," + ", formula.ranef, sep="")), data = mdata))
    uni.res <- coxmeTable(model)
    rn.uni <- lapply(list(uni.res), rownames)
    fix.all <- coxExp(uni.res, dec = dec)
    colnames(fix.all) <- c("HR(95%CI)", "P value")
    rownames(fix.all) <- names(model$coefficients)
  } else {
    unis <- lapply(xf, function(x) {
      forms <- paste0(formula.surv, "~", x, " + ", formula.ranef)
      if (length(xstrata) > 0) {
        forms <- paste0(forms, " + ", xstrata)
      }
      return(coxmeTable(coxme(as.formula(forms), data = mdata)))
    })
    rn.uni <- lapply(unis, rownames)
    unis2 <- Reduce(rbind, unis)
    uni.res <- unis2
    fix.all <- cbind(coxExp(uni.res, dec = dec), coxExp(coxmeTable(model), dec = dec))
    colnames(fix.all) <- c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
    rownames(fix.all) <- names(model$coefficients)
  }

  ## rownames
  fix.all.list <- lapply(1:length(xf), function(x) {
    fix.all[rownames(fix.all) %in% rn.uni[[x]], ]
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)

  rn.list <- lapply(1:length(xf), function(x) {
    rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
  })
  varnum.2fac <- which(lapply(xf, function(x) {
    length(sapply(mdata, levels)[[x]])
  }) == 2)
  lapply(varnum.2fac, function(x) {
    rn.list[[x]] <<- paste(xf[x], ": ", levels(mdata[, xf[x]])[2], " vs ", levels(mdata[, xf[x]])[1], sep = "")
  })
  lapply(varnum.mfac, function(x) {
    rn.list[[x]] <<- c(paste(xf[x], ": ref.=", levels(mdata[, xf[x]])[1], sep = ""), gsub(xf[x], "   ", rn.list[[x]]))
  })
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)
  pv.colnum <- which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  for (i in pv.colnum) {
    fix.all.unlist[, i] <- ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), dec + 1))
  }


  ## random effect
  # ranef = unlist(model$vcoef)
  # ranef.out = round(ranef, dec)
  ranef.out <- lapply(model$vcoef, function(x) {
    round(x, dec)
  })
  ranef.mat <- cbind(c(NA, unlist(ranef.out)), matrix(NA, length(unlist(ranef.out)) + 1, ncol(fix.all) - 1))
  rownames(ranef.mat) <- c("Random effect", paste(names(ranef.out), "(", unlist(lapply(ranef.out, names)), ")", sep = ""))


  ## metric
  no.grp <- unlist(lapply(model$frail, length))
  no.obs <- model$n[2]
  no.event <- model$n[1]
  metric.mat <- cbind(c(NA, no.grp, no.obs, no.event), matrix(NA, length(no.grp) + 3, ncol(fix.all) - 1))
  rownames(metric.mat) <- c(NA, paste("No. of groups(", names(no.grp), ")", sep = ""), "No. of observations", "No. of events")

  ## Integrated ll
  # ll = model$loglik[2]
  # aic = -2 * ll -2*model$df[1]

  ## caption
  surv.string <- as.character(attr(model$terms, "variables")[[2]])
  time.var.name <- surv.string[2]
  status.var.name <- surv.string[3]
  intro <- paste("Mixed effects Cox model on time ('", time.var.name, "') to event ('", status.var.name, "')", " - Group ", paste(names(model$vcoef), collapse = ", "), sep = "")
  var.names0 <- attr(model$terms, "term.labels")
  if (length(grep("strata", var.names0)) > 0) {
    intro <- paste(intro, " with '", var.names0[grep("strata", var.names0)], "'", sep = "")
  }

  return(list(table = fix.all.unlist, ranef = ranef.mat, metric = metric.mat, caption = intro))
}
