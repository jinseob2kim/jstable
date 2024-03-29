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
    OR <- paste(round(exp(lmer.coef[, 1]), dec), " (", round(exp(lmer.coef[, 1] - 1.96 * lmer.coef[, 2]), dec), ",", round(exp(lmer.coef[, 1] + 1.96 * lmer.coef[, 2]), dec), ")", sep = "")
    return(cbind(OR, pv))
  } else if (family %in% c(NULL, "gaussian")) {
    coeff <- paste(round(lmer.coef[, 1], dec), " (", round(lmer.coef[, 1] - 1.96 * lmer.coef[, 2], dec), ",", round(lmer.coef[, 1] + 1.96 * lmer.coef[, 2], dec), ")", sep = "")
    return(cbind(coeff, pv))
  } else if (family %in% c("poisson", "quasipoisson")) {
    RR <- paste(round(exp(lmer.coef[, 1]), dec), " (", round(exp(lmer.coef[, 1] - 1.96 * lmer.coef[, 2]), dec), ",", round(exp(lmer.coef[, 1] + 1.96 * lmer.coef[, 2]), dec), ")", sep = "")
    return(cbind(RR, pv))
  }
}







#' @title lmer.display: table for "lmerMod" or "glmerMod" object (lme4 package)
#' @description Make mixed effect model results from "lmerMod" or "glmerMod" object (lme4 package)
#' @param lmerMod.obj "lmerMod" or "glmerMod" object
#' @param dec Decimal, Default: 2
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
#' @importFrom stats update formula

lmer.display <- function(lmerMod.obj, dec = 2, ci.ranef = F) {
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
  basemodel <- update(lmerMod.obj, stats::formula(paste(c(". ~ .", xf), collapse = " - ")), data = mdata)
  unis <- lapply(xf, function(x) {
    summary(stats::update(basemodel, stats::formula(paste0(". ~ . +", x)), data = mdata))$coefficients
  })
  unis2 <- Reduce(rbind, unis)
  uni.res <- unis2[rownames(unis2) != "(Intercept)", ]


  if (length(xf) == 1) {
    fix.all <- lmerExp(uni.res, family = family.lmer, dec = dec)
    family.label <- colnames(fix.all)[1]
    colnames(fix.all) <- c(paste(family.label, "(95%CI)", sep = ""), "P value")
    rownames(fix.all) <- rownames(sl$coefficients)[-1]
  } else {
    fix.all <- cbind(lmerExp(uni.res, family = family.lmer, dec = dec), lmerExp(fixef, family = family.lmer, dec = dec))
    family.label <- colnames(fix.all)[1]
    colnames(fix.all) <- c(paste("crude ", family.label, "(95%CI)", sep = ""), "crude P value", paste("adj. ", family.label, "(95%CI)", sep = ""), "adj. P value")
    rownames(fix.all) <- rownames(sl$coefficients)[-1]
  }

  ranef <- data.frame(sl$varcor)[, c(1, 4)]
  ranef.out <- cbind(as.character(round(ranef[, 2], dec)), matrix(NA, nrow(ranef), ncol(fix.all) - 1))
  rownames(ranef.out) <- ranef[, 1]
  if (ci.ranef) {
    ranef.ci <- confint.merMod(lmerMod.obj, parm = 1:nrow(ranef), oldNames = F)^2
    ranef.paste <- paste(round(ranef$vcov, dec), " (", round(ranef.ci[, 1], dec), ",", round(ranef.ci[, 2], dec), ")", sep = "")
    ranef.out <- cbind(ranef.paste, matrix(NA, nrow(ranef), 3))
    rownames(ranef.out) <- ranef[, 1]
  }

  ranef.na <- rbind(rep(NA, ncol(fix.all)), ranef.out)
  rownames(ranef.na)[1] <- "Random effects"
  colnames(ranef.na) <- colnames(fix.all)


  ## rownames
  fix.all.list <- lapply(xf, function(x) {
    fix.all[grepl(x, rownames(fix.all)), ]
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)

  rn.list <- lapply(xf, function(x) {
    rownames(fix.all)[grepl(x, rownames(fix.all))]
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
  tb.df <- as.data.frame(rbind(fix.all.unlist, ranef.na))


  ## metric
  no.grp <- sl$ngrps
  no.obs <- nrow(mdata)
  ll <- round(sl$logLik[[1]], dec)
  aic <- round(sl$AICtab, dec)[1]
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
