#' @title lmerExp: transform the unit of coefficients (internal function)
#' @description Transform the unit of coefficients to "Coeff", "OR" or "RR"
#' @param lmer.coef lmer coefficients.
#' @param family Family: "gaussian", "binomial", "poisson", "quasipoisson", etc..., Default: 'binomial'
#' @param dec Decimal point
#' @return The transforemed coefficients(95% CI), p-value
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   #EXAMPLE1
#'  }
#' }
#' @rdname lmerExp
#' @importFrom stats pnorm


lmerExp = function(lmer.coef, family ="binomial", dec){
  pv = 2*(1-pnorm(abs(lmer.coef[,3])))
  if (family == "binomial"){
    OR = paste(round(exp(lmer.coef[,1]), dec), " (", round(exp(lmer.coef[,1] - 1.96*exp(lmer.coef[,2])), dec), ",", round(exp(lmer.coef[,1] + 1.96*exp(lmer.coef[,2])), dec),")", sep="")
    return(cbind(OR, pv))
  } else if (family %in% c(NULL, "gaussian")){
    coeff = paste(round(lmer.coef[,1], dec), " (", round(lmer.coef[,1] - 1.96*lmer.coef[,2], dec), ",", round(lmer.coef[,1] + 1.96*lmer.coef[,2], dec), ")", sep="")
    return(cbind(coeff, pv))
  } else if (family %in% c("poisson", "quasipoisson")){
    RR = paste(round(exp(lmer.coef[,1]), dec), " (", round(exp(lmer.coef[,1] - 1.96*exp(lmer.coef[,2])), dec), ",", round(exp(lmer.coef[,1] + 1.96*exp(lmer.coef[,2])), dec),")", sep="")
    return(cbind(RR, pv))
  }
} 







#' @title lmer.display: table for "lmerMod" of "glmerMod" object (lme4 package)
#' @description Make mixed effect model results from "lmerMod" or "glmerMod" object (lme4 package)
#' @param lmerMod.obj "lmerMod" or "glmerMod" object
#' @param dec Decimal, Default: 2
#' @param ci.ranef Show confidence interval of random effects?, Default: F
#' @return Table: fixed & random effect
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(geepack)
#'  data(dietox)
#'  dietox$Cu <- as.factor(dietox$Cu)
#'  l1 = lmer(Weight ~ Time + Cu + (1|Pig) + (1|Evit), data = dietox)
#'  lmer.display(l1)
#'  }
#' }
#' @rdname lmer.display
#' @export 
#' @importFrom lme4 lmer glmer confint.merMod

lmer.display = function(lmerMod.obj, dec = 2, ci.ranef = F){
  sl = summary(lmerMod.obj)
  fixef = sl$coefficients[-1,]
  ranef = data.frame(sl$varcor)[,c(1,4)]
  ranef.out = cbind(as.character(round(ranef[,2], dec)), matrix(NA, nrow(ranef), 3))
  rownames(ranef.out) = ranef[,1]
  if (ci.ranef){
    ranef.ci = confint.merMod(lmerMod.obj, parm = 1:nrow(ranef), oldNames = F)^2
    ranef.paste = paste(round(ranef$vcov, dec)," (", round(ranef.ci[,1], dec), ",", round(ranef.ci[,2], dec),")", sep="")
    ranef.out = cbind(ranef.paste, matrix(NA, nrow(ranef), 3))
    rownames(ranef.out) = ranef[,1]
  }
  ranef.na = rbind(rep(NA, 4), ranef.out)
  
  no.grp = sl$ngrps
  mdata = lmerMod.obj@frame
  forms = as.character(sl$call[[2]])
  y = forms[2]
  xfr = strsplit(forms[3]," \\+ ")[[1]]
  xr = xfr[grepl("\\|", xfr)]
  xf = xfr[!grepl("\\|", xfr)]
  family.lmer = ifelse(is.null(sl$family), "gaussian", sl$family)
  uni.res = ""
  if (family.lmer == "gaussian"){
    unis = lapply(xf, function(x){summary(lmer(as.formula(paste(y, "~", x," + ", paste(xr, collapse =" + "), sep="")), data = mdata))$coefficients})
    unis2 = Reduce(rbind, unis)
    uni.res <- unis2[rownames(unis2) != "(Intercept)",]
  } else{
    unis = lapply(xf, function(x){summary(glmer(as.formula(paste(y, "~", x," + ", paste(xr, collapse =" + "), sep="")), data = mdata, family = family.lmer))$coefficients})
    unis2 = Reduce(rbind, unis)
    uni.res <- unis2[rownames(unis2) != "(Intercept)",]
  }
  fix.all = cbind(lmerExp(uni.res, family=family.lmer, dec = dec), lmerExp(fixef, family=family.lmer, dec = dec))
  family.label = colnames(fix.all)[1]
  colnames(fix.all) = c(paste("crude ", family.label, ".(95%CI)",sep = ""), "crude P value", paste("adj. ", family.label, ".(95%CI)",sep = ""), "adj. P value")
  colnames(ranef.na) = colnames(fix.all)
  tb.df = as.data.frame(rbind(fix.all, ranef.na))
  lapply(c(2,4), function(x){tb.df[, x] <- as.numeric(as.vector(tb.df[, x]))})
  return(tb.df)
}
