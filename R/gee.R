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
    OR <- paste(round(exp(gee.coef[, 1]), dec), " (", round(exp(gee.coef[, 1] - 1.96 * gee.coef[, 2]), dec), ",", round(exp(gee.coef[, 1] + 1.96 * gee.coef[, 2]), dec), ")", sep = "")
    return(cbind(OR, gee.coef[, 3]))
  } else if (family == "gaussian") {
    coeff <- paste(round(gee.coef[, 1], dec), " (", round(gee.coef[, 1] - 1.96 * gee.coef[, 2], dec), ",", round(gee.coef[, 1] + 1.96 * gee.coef[, 2], dec), ")", sep = "")
    return(cbind(coeff, gee.coef[, 3]))
  } else if (family %in% c("poisson", "quasipoisson")) {
    RR <- paste(round(exp(gee.coef[, 1]), dec), " (", round(exp(gee.coef[, 1] - 1.96 * gee.coef[, 2]), dec), ",", round(exp(gee.coef[, 1] + 1.96 * gee.coef[, 2]), dec), ")", sep = "")
    return(cbind(RR, gee.coef[, 3]))
  }
}



#' @title geeglm.display
#' @description Make gee results from "geeglm" object
#' @param geeglm.obj "geeglm" object
#' @param decimal Decimal, Default: 2
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

geeglm.display <- function(geeglm.obj, decimal = 2) {
  family.gee <- geeglm.obj$family[[1]]
  corstr.gee <- geeglm.obj$corstr
  y <- as.character(geeglm.obj$terms[[2]])
  xs <- names(geeglm.obj$model)[-1]
  ## rownames
  geeglm.obj$data <- data.table::data.table(geeglm.obj$data)
  # nomiss <- stats::complete.cases(geeglm.obj$data[, c(y, xs), with = F])
  basemodel <- stats::update(geeglm.obj, stats::formula(paste(c(". ~ .", xs), collapse = " - ")), data = geeglm.obj$data)
  # gee.uni.list <- lapply(xs, function(x){geeUni(y, x, data = geeglm.obj$data[nomiss, ], id.vec = geeglm.obj$id, family = family.gee, cor.type = corstr.gee)})
  gee.uni.list <- lapply(xs, function(x) {
    summary(stats::update(basemodel, stats::formula(paste0(". ~ . +", x)), data = geeglm.obj$data))$coefficients[-1, -3]
  })
  rn.uni <- lapply(gee.uni.list, rownames)
  gee.uni <- Reduce(rbind, gee.uni.list)

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
    gee.multi <- summary(geeglm.obj)$coefficients[-1, -3]
    gee.res <- cbind(geeExp(gee.uni, family = family.gee, dec = decimal), geeExp(gee.multi, family = family.gee, dec = decimal))
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
  info.gee <- as.character(c(NA, round(as.numeric(summary(geeglm.obj)$corr[1]), decimal + 1), length(unique(geeglm.obj$id)), length(geeglm.obj$y)))
  info.df <- cbind(info.gee, matrix(NA, 4, ncol(gee.res) - 1))
  colnames(info.df) <- colnames(out)
  # lapply(seq(2, ncol(gee.res), by =2), function(x){info.df[, x] <<- as.numeric(info.df[, x])})
  rownames(info.df) <- c("", "Estimated correlation parameters", "No. of clusters", "No. of observations")

  ## Caption
  cap.gee <- paste("GEE(", family.gee, ") predicting ", y, " by ", paste(xs, collapse = ", "), " - Group ", as.character(geeglm.obj$call$id)[length(as.character(geeglm.obj$call$id))], sep = "")

  return(list(caption = cap.gee, table = out, metric = info.df))
}
