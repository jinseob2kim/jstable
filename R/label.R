## Label data 


#' @title Export label and level: one variable
#' @description Export label and level: one variable
#' @param data data
#' @param vname variable to export label and level
#' @return if continuous variable - (label, NA), categorical variable - (label, level)
#' @details DETAILS
#' @examples 
#'  lapply(names(iris), function(x){jstable::mk.lev.var(iris, x)})
#' @rdname mk.lev.var
#' @export 

mk.lev.var = function(data , vname){
  v.vec = data[[vname]]
  out = ""
  if (is.numeric(v.vec)){
    out = c(vname, class(v.vec), NA)
  } else{
    v.level = levels(v.vec)
    nr = length(v.level)
    out = cbind(rep(vname, nr), rep(class(v.vec), nr), v.level)
  }
  return(out)
}



#' @title Export label and level: multiple variable
#' @description Export label and level: multiple variable
#' @param data data
#' @return default label and level data
#' @details DETAILS
#' @examples 
#'  mk.lev(iris)
#' @rdname mk.lev
#' @export 
#' @importFrom data.table data.table :=

mk.lev = function(data){
  
  variable <- level <- val_label <- NULL
  
  out.list = lapply(names(data), function(x){mk.lev.var(data, x)})
  out.dt = data.table::data.table(Reduce(rbind, out.list))
  names(out.dt) = c("variable", "class","level")
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
#'  fit <- glm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#'  fit.table <- glmshow.display(fit)
#'  iris.label <- mk.lev(iris)
#'  LabelepiDisplay(fit.table, label = TRUE, ref = iris.label)
#' @rdname LabelepiDisplay
#' @export 
#' @importFrom data.table data.table :=


LabelepiDisplay = function(epiDisplay.obj, label = F, ref){
  
  variable <- level <- val_label <- NULL
  
  tb.main <- epiDisplay.obj$table
  tb.compact <- tb.main[!rownames(tb.main)=="", ]
  if (nrow(tb.main)  <= 2){
    tb.compact <- tb.main
  }

  
  ## Var label
  tb.rn = gsub(" \\(cont. var.\\)", "", rownames(tb.compact))
  rownames(tb.compact) = tb.rn
 
  if (nrow(tb.main) < 2 & label == T){
    vname <- strsplit(rownames(tb.compact)[1], ":")[[1]][1]
    rownames(tb.compact) <- gsub(vname, ref[variable == vname, var_label][1], rownames(tb.compact))
    if (length(ref[variable == vname, level]) == 2){
      vll <- ref[variable == vname, c("level", "val_label")]
      rownames(tb.compact) <- gsub(paste(vll[2, 1], " vs ", vll[1,1], sep=""), paste(vll[2, 2], " vs ", vll[1,2], sep=""), rownames(tb.compact))
    }
  }
  
  if (nrow(tb.main) > 2 & label == T){
    vn <- which(substr(tb.rn, 1, 1) != " ")
    vns <- c(vn, length(tb.rn)+1 )
    vl <- lapply(1:length(vn), function(x){tb.rn[vns[x]:(vns[x+1]-1)]})
    vl_label <- lapply(vl, function(x){
      vname <- strsplit(x[1], ":")[[1]][1]
      #x[1] <- gsub(vname, ref[variable == vname, var_label][1], x[1])
      if (length(ref[variable == vname, level]) == 2){
        vll = ref[variable == vname, c("level", "val_label")]
        x <- paste(ref[variable == vname, var_label][1], ": ", vll[2, 2], " vs ", vll[1, 2], sep = "")
        #x = gsub(paste(vll[2, 1], " vs ", vll[1,1], sep=""), paste(vll[2, 2], " vs ", vll[1,2], sep=""), x)
      } else if (ref[variable == vname, class][1] %in% c("factor", "character")){
        x[1] <- paste(ref[variable == vname, var_label][1], ": ref.=", ref[variable == vname, val_label][1], sep = "")
        for (k in 2:length(x)){
          x[k] <- paste("   ", ref[variable == vname & level == strsplit(x[k], "   ")[[1]][2], val_label], sep = "")
        }
        
        #for (y in ref[variable == vname, level]) {x = gsub(y, ref[variable == vname & level == y, val_label], x)}
      }
      return(x)
    })
    rownames(tb.compact) <- unlist(vl_label)
  }
  
  ll = strsplit(epiDisplay.obj$last.lines,"\n")[[1]]
  ll.vec = matrix(unlist(lapply(ll,function(x){strsplit(x," = ")})), ncol =2, byrow=T)
  ll.mat = matrix(rep("", nrow(ll.vec)* ncol(tb.compact)), nrow = nrow(ll.vec))  
  ll.mat[,1] = ll.vec[,2]
  rownames(ll.mat) = ll.vec[,1]
  out = rbind(tb.compact, rep("", ncol(tb.compact)), ll.mat)
  
  if (nrow(tb.main) == 2){
    out = rbind(tb.compact, ll.mat)
  }
  
  p.colnum = which(colnames(out) %in% c("P value", "adj. P value", "P(t-test)", "P(Wald's test)")) 
  p.colnum = p.colnum[length(p.colnum)]
  
  pn = gsub("< ","", out[, p.colnum])
  
  colnames(out)[p.colnum] = ifelse(colnames(out)[p.colnum] == "P value", "P value", "adj. P value")
  sig = ifelse(as.numeric(pn) <= 0.05, "**", "")
  return(cbind(out,sig))
}





#' @title LabeljsTable: Apply label information to jstable object using label data
#' @description Apply label information to table of geeglm.display, lmer.display, coxme.display using label data
#' @param obj.table table of geeglm.display, lmer.display, coxme.display
#' @param ref Label data made by mk.lev function
#' @return table of geeglm.display, lmer.display, coxme.display with label information
#' @details DETAILS
#' @examples 
#'  library(coxme)
#'  fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1|inst) +(1|sex), lung)
#'  fit.table <- coxme.display(fit)
#'  lung.label <- mk.lev(lung)
#'  LabeljsTable(fit.table$table, ref = lung.label)
#' @rdname LabeljsTable
#' @export 
#' @importFrom data.table data.table :=

LabeljsTable = function(obj.table, ref){
  
  variable <- level <- val_label <- NULL
  
  tb.main <- obj.table
  tb.compact <- tb.main
  
  ## Var label
  tb.rn = rownames(tb.compact)

  if (nrow(tb.main) == 1){
    vname = strsplit(rownames(tb.compact)[1], ":")[[1]][1]
    rownames(tb.compact) = gsub(vname, ref[variable == vname, var_label][1], rownames(tb.compact))
    if (length(ref[variable == vname, level]) == 2){
      vll = ref[variable == vname, c("level", "val_label")]
      rownames(tb.compact) = gsub(paste(vll[2, 1], " vs ", vll[1,1], sep=""), paste(vll[2, 2], " vs ", vll[1,2], sep=""), rownames(tb.compact))
    }
  }
  
  if (nrow(tb.main) > 1){
    vn = which(substr(tb.rn, 1, 1) != " ")
    vns = c(vn, length(tb.rn)+1 )
    vl = lapply(1:length(vn), function(x){tb.rn[vns[x]:(vns[x+1]-1)]})
    vl_label = lapply(vl, function(x){
      vname <- strsplit(x[1], ":")[[1]][1]
      #x[1] <- gsub(vname, ref[variable == vname, var_label][1], x[1])
      if (length(ref[variable == vname, level]) == 2){
        vll = ref[variable == vname, c("level", "val_label")]
        x <- paste(ref[variable == vname, var_label][1], ": ", vll[2, 2], " vs ", vll[1, 2], sep = "")
        #x = gsub(paste(vll[2, 1], " vs ", vll[1,1], sep=""), paste(vll[2, 2], " vs ", vll[1,2], sep=""), x)
      } else if (ref[variable == vname, class][1] %in% c("factor", "character")){
        x[1] <- paste(ref[variable == vname, var_label][1], ": ref.=", ref[variable == vname, val_label][1], sep = "")
        for (k in 2:length(x)){
          x[k] <- paste("   ", ref[variable == vname & level == strsplit(x[k], "   ")[[1]][2], val_label], sep = "")
        }
        
        #for (y in ref[variable == vname, level]) {x = gsub(y, ref[variable == vname & level == y, val_label], x)}
      }
      return(x)
    })
    rownames(tb.compact) = unlist(vl_label)
  }
  
  out = tb.compact
  #sig.colnum = which(colnames(out) %in% c("P value", "adj. P value")) 
  #pn = gsub("< ","", out[, sig.colnum])
  #sig = ifelse(as.numeric(pn) <= 0.05, "**", "")
  
  #pv.colnum = which(colnames(out) %in% c("P value", "crude P value", "adj. P value"))
  #for (i in pv.colnum){
  #  out[, i] = ifelse(as.numeric(out[, i]) < 0.001, "< 0.001", round(as.numeric(out[, i]), 3))
  #}
  return(out)
}
  



#' @title LabeljsRanef: Apply label information to jstable random effect object using label data
#' @description Apply label information to ranef object of jstable using label data
#' @param obj.ranef ranef of lmer.display, coxme.display, cox2.display
#' @param ref Label data made by mk.lev function
#' @return ranef of lmer.display, coxme.display, cox2.display with label information
#' @details DETAILS
#' @examples 
#'  library(coxme)
#'  fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1|inst) +(1|sex), lung)
#'  fit.table <- coxme.display(fit)
#'  lung.label <- mk.lev(lung)
#'  LabeljsTable(fit.table$table, ref = lung.label)
#'  LabeljsRanef(fit.table$ranef, ref = lung.label)
#' @rdname LabeljsRanef
#' @export 

LabeljsRanef = function(obj.ranef, ref){
  
  variable <- NULL
  
  ranef <- obj.ranef
  ranef.split <- strsplit(rownames(ranef)[-1], "\\(")
  ranef.vname <- unlist(lapply(ranef.split, function(x){x[[1]]}))
  ranef.vname.label <- sapply(ranef.vname, function(x){ref[variable == x, var_label][1]})
  if (length(ranef.split) ==1){
    rownames(ranef)[-1] <- ranef.vname.label
  } else{
    rownames(ranef)[-1] <- paste(ranef.vname.label, "(", unlist(lapply(ranef.split, function(x){x[[2]]})), sep="")
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
#'  library(coxme)
#'  fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1|inst) +(1|sex), lung)
#'  fit.table <- coxme.display(fit)
#'  lung.label <- mk.lev(lung)
#'  LabeljsTable(fit.table$table, ref = lung.label)
#'  LabeljsRanef(fit.table$ranef, ref = lung.label)
#'  LabeljsMetric(fit.table$metric, ref = lung.label)
#' @rdname LabeljsMetric
#' @export 

LabeljsMetric = function(obj.metric, ref){
  
  variable <- NULL
  
  metric <- obj.metric
  rname <- rownames(metric)
  group.rnum <- grep("No. of group", rname)
  group.vars <- unlist(lapply(strsplit(rname[group.rnum], "\\("), function(x){x[[2]]}))   
  group.vname <- unlist(strsplit(group.vars, "\\)"))
  group.vname.label <- sapply(group.vname, function(x){ref[variable == x, var_label][1]})
  rownames(metric)[group.rnum] <- paste("No. of group(", group.vname.label, ")", sep="")
  return(metric)
}




#' @title LabeljsMixed: Apply label information to jstable object using label data
#' @description Apply label information to object of jstable using label data
#' @param obj lmer.display, coxme.display
#' @param ref Label data made by mk.lev function
#' @return lmer.display, coxme.display with label information
#' @details DETAILS
#' @examples 
#'  library(coxme)
#'  fit <- coxme(Surv(time, status) ~ sex + ph.ecog + ph.karno + (1|inst) +(1|sex), lung)
#'  fit.table <- coxme.display(fit)
#'  lung.label <- mk.lev(lung)
#'  LabeljsMixed(fit.table, ref = lung.label)
#' @rdname LabeljsMixed
#' @export 

LabeljsMixed = function(obj, ref){
  
  variable <- NULL
  
  out <- list()
  out$table <- LabeljsTable(obj$table, ref = ref)
  out$ranef <- LabeljsRanef(obj$ranef, ref = ref)
  out$metric <- LabeljsMetric(obj$metric, ref = ref)
  out$caption <- obj$caption
  if (grep("Mixed effects Cox model", obj$caption) == 1){
    surv.vname <- strsplit(obj$caption, "'")[[1]][c(2,4)]
    for (vn in surv.vname){
      out$caption <- gsub(paste("'", vn, "'", sep = ""), paste("'", ref[variable == vn, var_label][1], "'", sep = ""), out$caption)
    }
    group.vname.comma <- strsplit(obj$caption, "- Group ")[[1]][2]
    group.vname <- strsplit(group.vname.comma, ", ")[[1]]
    group.vname.label <- sapply(group.vname, function(x){ref[variable == x, var_label][1]})
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
#'  library(survival)
#'  fit <- coxph(Surv(time, status) ~ sex + ph.ecog + ph.karno + cluster(inst), 
#'                data = lung, model = TRUE)
#'  fit.table <- cox2.display(fit)
#'  lung.label <- mk.lev(lung)
#'  LabeljsCox(fit.table, ref = lung.label)
#' @rdname LabeljsCox
#' @export 

LabeljsCox = function(obj, ref){
  
  variable <- NULL
  
  out <- list()
  out$table <- LabeljsTable(obj$table, ref = ref)
  if (!is.null(obj$ranef)){
    out$ranef <- LabeljsRanef(obj$ranef, ref = ref)
  }
  out$metric <- obj$metric
  out$caption <- obj$caption
  surv.vname <- strsplit(obj$caption, "'")[[1]][c(2,4)]
  for (vn in surv.vname){
    out$caption <- gsub(paste("'", vn, "'", sep = ""), paste("'", ref[variable == vn, var_label][1], "'", sep = ""), out$caption)
  }
  if (length(grep("- Group", obj$caption)) >= 1){
    group.vname.comma <- strsplit(obj$caption, "- Group ")[[1]][2]
    group.vname <- strsplit(group.vname.comma, ", ")[[1]]
    group.vname.label <- sapply(group.vname, function(x){ref[variable == x, var_label][1]})
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
#'  library(geepack);library(jstable)
#'  data(dietox)
#'  dietox$Cu <- as.factor(dietox$Cu)
#'  gee01 <- geeglm (Weight ~ Time + Cu , id =Pig, data = dietox,
#'                 family=gaussian,corstr="ex")
#'  g1 <- geeglm.display(gee01)
#'  LabeljsGeeglm(g1, ref = mk.lev(dietox))
#' @rdname LabeljsGeeglm
#' @export 

LabeljsGeeglm = function(obj, ref){
  
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
  out$caption <- paste(cap.split[1], "predicting ", ref[variable == y, var_label][1], " by ", paste(sapply(xx, function(vn){ref[variable == vn, var_label][1]}), collapse = ", "), " - Group ", ref[variable == xc, var_label][1], sep="") 
  
  return(out)
}


