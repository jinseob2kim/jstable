#' @title cox2.display: table for coxph.object - allow "frailty" or "cluster" model
#' @description Table for coxph.object - allow "frailty" or "cluster" model  
#' @param cox.obj coxph.object
#' @param dec Decimal point, Default: 2
#' @return Table, cluster/frailty info, metrics, caption
#' @details GEE like - cluster, Mixed effect model like - frailty
#' @examples 
#' \dontrun{
#' if(interactive()){
#' library(survival)
#'  data(lung)
#'  fit1 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), lung)
#'  fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frail(inst), lung)
#'  cox2.display(fit1)
#'  cox2.display(fit2)
#'  }
#' }
#' @rdname cox2.display
#' @export 
#' @importFrom survival coxph 

cox2.display <- function (cox.obj, dec = 2) 
{
  model <- cox.obj
  if(!any(class(model)=="coxph")){stop("Model not from Cox model")}
  
  xf <- attr(model$terms, "term.labels") # Independent vars
  xf.old <- xf
  xc <- NULL
  mtype <- "normal"
  
  if(length(grep("strata", xf)) > 0){
    xf <- xf[-grep("strata",xf)]
  } else if(length(grep("frailty", xf)) > 0){
    xf <- xf[-grep("frailty",xf)]
    mtype <- "frailty"
    xc <- setdiff(xf.old, xf)
  } else if(summary(model)$used.robust == T){
    mtype <- "cluster"
    xfull <-  strsplit(as.character(model$call[[2]][3]), " \\+ ")[[1]]
    xc <- setdiff(xfull, xf)
  }
   
  formula.surv = as.character(model$formula)[2]
  formula.ranef = paste(" + ", xc, sep = "")
  if (is.null(xc)){ 
    formula.ranef = NULL
    }
    
  mdata = data.frame(get(as.character(model$call)[3]))
  
  if(length(xf) == 1){
    uni.res = data.frame(summary(coxph(as.formula(paste(formula.surv, "~", xf, formula.ranef, sep="")), data = mdata))$coefficients)
    names(uni.res)[ncol(uni.res)] = "p"
    uni.res2 = NULL
    if (mtype == "normal"){
      uni.res2 <- uni.res[, c(1, 3, 4, 5)]
    } else if (mtype == "cluster"){
      uni.res2 <- uni.res[, c(1, 4, 5, 6)]
    } else {
      uni.res2 <- uni.res[-nrow(uni.res), c(1, 3, 4, 6)]
    }
    fix.all = coxExp(uni.res, dec = dec)
    colnames(fix.all) = c("HR(95%CI)", "P value")
    rownames(fix.all) = ifelse(mtype == "frailty", names(model$coefficients)[-length(model$coefficients)], names(model$coefficients))
  } else{
    unis <- lapply(xf, function(x){
      uni.res = data.frame(summary(coxph(as.formula(paste(formula.surv, "~", x, formula.ranef, sep="")), data = mdata))$coefficients)
      names(uni.res)[ncol(uni.res)] = "p"
      uni.res2 = NULL
      if (mtype == "normal"){
        uni.res2 <- uni.res[, c(1, 3, 4, 5)]
      } else if (mtype == "cluster"){
        uni.res2 <- uni.res[, c(1, 4, 5, 6)]
      } else {
        uni.res2 <- uni.res[-nrow(uni.res), c(1, 3, 4, 6)]
      }
      return(uni.res2)
      })
    unis2 <- Reduce(rbind, unis)
    uni.res <- unis2
    mul.res <- data.frame(summary(model)$coefficients)
    colnames(mul.res)[ncol(mul.res)] <- "p"
    fix.all = cbind(coxExp(uni.res, dec = dec), coxExp(mul.res[rownames(uni.res), names(uni.res)], dec = dec))
    colnames(fix.all) = c("crude HR(95%CI)", "crude P value", "adj. HR(95%CI)", "adj. P value")
    rownames(fix.all) = rownames(uni.res)
  }
  
  ## rownames
  fix.all.list = lapply(xf, function(x){fix.all[grepl(x, rownames(fix.all)),]})      
  varnum.mfac = which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x){fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])})
  fix.all.unlist = Reduce(rbind, fix.all.list)
  
  rn.list = lapply(xf, function(x){rownames(fix.all)[grepl(x, rownames(fix.all))]})
  varnum.2fac = which(lapply(xf, function(x){length(sapply(mdata, levels)[[x]])}) == 2)
  lapply(varnum.2fac, function(x){rn.list[[x]] <<- paste(xf[x], ": ", levels(mdata[, xf[x]])[2], " vs ", levels(mdata[, xf[x]])[1], sep="")})
  lapply(varnum.mfac, function(x){rn.list[[x]] <<- c(paste(xf[x],": ref.=", levels(mdata[, xf[x]])[1], sep=""), gsub(xf[x],"   ", rn.list[[x]]))})
  if (class(fix.all.unlist) == "character"){
    fix.all.unlist = t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) = unlist(rn.list)
  pv.colnum = which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  for (i in pv.colnum){
    fix.all.unlist[, i] = ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), dec + 1))
  }
  
  
  ## random effect
  #ranef = unlist(model$vcoef)
  #ranef.out = round(ranef, dec)
  ranef.mat = NULL
  if (mtype == "cluster"){
    ranef.mat <- cbind(c(NA, NA), matrix(NA, length(xc) + 1, ncol(fix.all) - 1))
    clname = strsplit(xc, "\\(")[[1]]
    cvname = strsplit(paste(clname[-1], collapse = "("), "\\)")[[1]]
    cvname = paste(cvname[length(cvname)], collapse = ")")
    rownames(ranef.mat) = c(clname[1], cvname)
  } else if (mtype == "frailty"){
    ranef.mat <- cbind(c(NA, NA), matrix(NA, length(xc) + 1, ncol(fix.all) - 1))
    clname = strsplit(xc, "\\(")[[1]]
    cvname = strsplit(paste(clname[-1], collapse = "("), "\\)")[[1]]
    cvname = paste(cvname[length(cvname)], collapse = ")")
    rownames(ranef.mat) = c(clname[1], cvname)
  }
  

  ## metric
  #no.grp = unlist(lapply(model$frail, length))
  no.obs = model$n
  no.event = model$nevent
  metric.mat = cbind(c(NA, no.obs, no.event), matrix(NA, 3, ncol(fix.all) - 1))
  rownames(metric.mat) = c(NA, "No. of observations", "No. of events")
  
  ## Integrated ll
  #ll = model$loglik[2]
  #aic = -2 * ll -2*model$df[1] 
  
  ## caption
  surv.string <- as.character(attr(model$terms, "variables")[[2]])
  time.var.name <- surv.string[2]
  status.var.name <-  surv.string[3]
  intro <- paste("Cox model on time ('", time.var.name, "') to event ('", status.var.name, "')", sep="")
  if (mtype == "cluster"){
    intro <- paste("Marginal", intro, "- Group", cvname)
  } else if (mtype == "frailty"){
    intro <- paste("Frailty", intro, "- Group", cvname)
  }
  
  var.names0 <- attr(model$terms, "term.labels")
  if(length(grep("strata", var.names0))>0) {intro <- paste(intro, " with '", var.names0[grep("strata", var.names0)], "'", sep="" )}
  
  if (is.null(ranef.mat)){
    return(list(table = fix.all.unlist, metric = metric.mat, caption = intro))
  } else{
    return(list(table = fix.all.unlist, ranef = ranef.mat, metric = metric.mat, caption = intro)) 
  }
}