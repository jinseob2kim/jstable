#' @title svyregress.display: table for svyglm.object
#' @description table for svyglm.object (survey package).
#' @param svyglm.obj svyglm.object
#' @param decimal digit, Default: 2
#' @return table
#' @details DETAILS
#' @examples
#' library(survey)
#' data(api)
#' apistrat$tt <- c(rep(1, 20), rep(0, nrow(apistrat) - 20))
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
#' ds <- svyglm(api00 ~ ell + meals + cname + mobility, design = dstrat)
#' ds2 <- svyglm(tt ~ ell + meals + cname + mobility, design = dstrat, family = quasibinomial())
#' svyregress.display(ds)
#' svyregress.display(ds2)
#' @rdname svyglm.display
#' @export
#' @importFrom survey svyglm
#' @importFrom stats update confint


svyregress.display <- function(svyglm.obj, decimal = 2) {
  model <- svyglm.obj
  design.model <- model$survey.design
  xs <- attr(model$terms, "term.labels")
  y <- names(model$model)[1]
  gaussianT <- ifelse(length(grep("gaussian", model$family)) == 1, T, F)

  if (length(xs) == 0) {
    stop("No independent variable")
  } else if (length(xs) == 1) {
    uni <- cbind(data.frame(coefNA(model))[-1, ], data.frame(stats::confint(model))[-1, ])
    rn.uni <- lapply(list(uni), rownames)
    # uni <- data.frame(summary(survey::svyglm(as.formula(paste(y, " ~ ", xs)), design = design.model, family = model$family))$coefficients)[-1, ]
    if (gaussianT) {
      summ <- paste(round(uni[, 1], decimal), " (", round(uni[, 5], decimal), ",", round(uni[, 6], decimal), ")", sep = "")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("Coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    } else {
      summ <- paste(round(exp(uni[, 1]), decimal), " (", round(exp(uni[, 5]), decimal), ",", round(exp(uni[, 6]), decimal), ")", sep = "")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    }
    rownames(uni.res) <- rownames(uni)
    res <- uni.res
  } else {
    uni <- lapply(xs, function(v) {
      coef.df <- data.frame(coefNA(stats::update(model, formula(paste(paste(c(". ~ .", xs), collapse = " - "), " + ", v)), design = design.model)))[-1, ]
      confint.df <- data.frame(stats::confint(stats::update(model, formula(paste(paste(c(". ~ .", xs), collapse = " - "), " + ", v)), design = design.model)))[-1, ]

      conf.df <- data.frame(matrix(nrow = nrow(coef.df), ncol = 2))
      rownames(conf.df) <- rownames(coef.df)
      for (i in rownames(conf.df)) {
        conf.df[i, ] <- confint.df[i, ]
      }

      cbind(coef.df, conf.df)
    })

    # uni <- lapply(xs, function(v){
    #  summary(survey::svyglm(as.formula(paste(y, " ~ ", v)), design = design.model))$coefficients[-1, ]
    # })
    rn.uni <- lapply(uni, rownames)
    uni <- Reduce(rbind, uni)

    if (gaussianT) {
      summ <- paste(round(uni[, 1], decimal), " (", round(uni[, 5], decimal), ",", round(uni[, 6], decimal), ")", sep = "")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))))
      colnames(uni.res) <- c(paste("crude coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <- rownames(uni)
      # mul <- summary(model)$coefficients[-1, ]
      mul <- cbind(coefNA(model)[-1, ], stats::confint(model)[-1, ])
      mul.summ <- paste(round(mul[, 1], decimal), " (", round(mul[, 5], decimal), ",", round(mul[, 6], decimal), ")", sep = "")
      mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
      colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
    } else {
      summ <- paste(round(exp(uni[, 1]), decimal), " (", round(exp(uni[, 5]), decimal), ",", round(exp(uni[, 6]), decimal), ")", sep = "")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <= 0.001, "< 0.001", as.character(round(uni[, 4], decimal + 1)))))
      colnames(uni.res) <- c(paste("crude OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <- rownames(uni)
      # mul <- summary(model)$coefficients[-1, ]
      mul <- cbind(coefNA(model)[-1, ], stats::confint(model)[-1, ])
      mul.summ <- paste(round(exp(mul[, 1]), decimal), " (", round(exp(mul[, 5]), decimal), ",", round(exp(mul[, 6]), decimal), ")", sep = "")
      mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <= 0.001, "< 0.001", as.character(round(mul[, 4], decimal + 1)))))
      colnames(mul.res) <- c(paste("adj. OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
    }
    res <- cbind(uni.res[rownames(uni.res) %in% rownames(mul.res), ], mul.res)
    rownames(res) <- rownames(mul)
  }

  fix.all <- res
  mdata <- model$model
  ## rownames
  # fix.all.list = lapply(xs, function(x){fix.all[grepl(x, rownames(fix.all)),]})
  fix.all.list <- lapply(1:length(xs), function(x) {
    fix.all[rownames(fix.all) %in% rn.uni[[x]], ]
  })
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x) {
    fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])
  })
  fix.all.unlist <- Reduce(rbind, fix.all.list)

  # rn.list = lapply(xs, function(x){rownames(fix.all)[grepl(x, rownames(fix.all))]})
  rn.list <- lapply(1:length(xs), function(x) {
    rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]
  })
  varnum.2fac <- which(xs %in% names(model$xlevels)[lapply(model$xlevels, length) == 2])
  lapply(varnum.2fac, function(x) {
    rn.list[[x]] <<- paste(xs[x], ": ", model$xlevels[[xs[x]]][2], " vs ", model$xlevels[[xs[x]]][1], sep = "")
  })
  lapply(varnum.mfac, function(x) {
    if (grepl(":", xs[x])) {
      a <- unlist(strsplit(xs[x], ":"))[1]
      b <- unlist(strsplit(xs[x], ":"))[2]

      if (a %in% xs && b %in% xs) {
        ref <- paste0(a, model$xlevels[[a]][1], ":", b, model$xlevels[[b]][1])
        rn.list[[x]] <<- c(paste(xs[x], ": ref.=", ref, sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
      } else {
        rn.list[[x]] <<- c(paste(xs[x], ": ref.=NA", sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
      }
    } else {
      rn.list[[x]] <<- c(paste(xs[x], ": ref.=", model$xlevels[[xs[x]]][1], sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
    }
    # rn.list[[x]] <<- c(paste(xs[x], ": ref.=", model$xlevels[[xs[x]]][1], sep = ""), gsub(xs[x], "   ", rn.list[[x]]))
  })
  if (class(fix.all.unlist)[1] == "character") {
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)

  # pv.colnum = which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  # for (i in pv.colnum){
  #  fix.all.unlist[, i] = ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), decimal + 1))
  # }


  outcome.name <- names(model$model)[1]


  if (gaussianT) {
    first.line <- paste("Linear regression predicting ", outcome.name, sep = "", "- weighted data\n")
    last.lines <- paste("No. of observations = ",
      length(model$y), "\n", "AIC value = ", round(
        model$aic,
        decimal + 2
      ), "\n", "\n",
      sep = ""
    )
  } else {
    first.line <- paste("Logistic regression predicting ", outcome.name, sep = "", "- weighted data\n")
    last.lines <- paste("No. of observations = ", nrow(model$model),
      "\n", "\n",
      sep = ""
    )
  }

  results <- list(
    first.line = first.line, table = fix.all.unlist,
    last.lines = last.lines
  )
  class(results) <- c("display", "list")
  return(results)
}
