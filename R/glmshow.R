
#' @title coefNA: make coefficient table with NA
#' @description Make coefficient table with NA
#' @param model glm object (gaussian or binomial)
#' @return coefficient table with NA
#' @details DETAILS
#' @examples 
#'  coefNA(glm(mpg ~ wt + qsec, data = mtcars))
#' @rdname coefNA
#' @export 
#' @importFrom stats coef

coefNA <- function(model){
  coef.rownames <- merge(coef(summary(model)), model$coefficients, by =0, all= T)
  coef.matrix <- as.matrix(coef.rownames[, -c(1, ncol(coef.rownames))])
  rownames(coef.matrix) <- coef.rownames[, "Row.names"]
  return(coef.matrix[names(model$coefficients), ])
}


#' @title glmshow.display: Show summary table of glm object.
#' @description Show summary table of glm object(regression, logistic).
#' @param glm.object glm.object
#' @param decimal digits, Default: 2
#' @return table
#' @details DETAILS
#' @examples
#'  glmshow.display(glm(mpg ~ wt + qsec, data = mtcars))
#' @seealso
#'  \code{\link[stats]{glm}}
#' @rdname glmshow.display
#' @export
#' @importFrom stats glm cor predict formula

glmshow.display <- function (glm.object, decimal = 2){
  model <- glm.object
  if(!any(class(model) %in% c("lm", "glm"))){stop("Model not from  GLM")}
  
  xs <- attr(model$terms, "term.labels")
  y <- names(model$model)[1]
  gaussianT <- ifelse(length(grep("gaussian", model$family)) == 1, T, F)
  data <- model$data
  
  ## table
  if (length(xs) == 0){
    stop("No independent variable")
  } else if (length(xs) ==1){
    uni <- data.frame(coefNA(glm.object))[-1, ]
    rn.uni <- lapply(list(uni), rownames)
    if (gaussianT){
      summ <- paste(round(uni[,1], decimal), " (", round(uni[, 1] - 1.96*uni[, 2], decimal), "," ,round(uni[, 1] + 1.96*uni[, 2], decimal), ")", sep ="")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <=0.001, "< 0.001", as.character(round(uni[, 4], decimal +1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("Coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    } else{
      summ <- paste(round(exp(uni[,1]), decimal), " (", round(exp(uni[, 1] - 1.96*uni[, 2]), decimal), "," ,round(exp(uni[, 1] + 1.96*uni[, 2]), decimal), ")", sep ="")
      uni.res <- matrix(cbind(summ, ifelse(uni[, 4] <=0.001, "< 0.001", as.character(round(uni[, 4], decimal +1)))), nrow = nrow(uni))
      colnames(uni.res) <- c(paste("OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "P value")
    }
    rownames(uni.res) <-rownames(uni)
    res <- uni.res
    
  } else{
    basemodel <- stats::update(model, formula(paste(c(". ~ .", xs), collapse=' - ')), data = data)
    
    uni <- lapply(xs, function(v){
      data.frame(coefNA(stats::update(basemodel, formula(paste0(". ~ . +", v)), data = data)))
      #data.frame(coefNA(stats::glm(as.formula(paste(y, " ~ ", v)), data = data, family = model$family)))[-1, ]
    })
    rn.uni <- lapply(uni, rownames)
    uni <- Reduce(rbind, uni)
    if (gaussianT){
      summ <- paste(round(uni[,1], decimal), " (", round(uni[, 1] - 1.96*uni[, 2], decimal), "," ,round(uni[, 1] + 1.96*uni[, 2], decimal), ")", sep ="")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <=0.001, "< 0.001", as.character(round(uni[, 4], decimal +1)))))
      colnames(uni.res) <- c(paste("crude coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <-rownames(uni)
      mul <- coefNA(model)[-1, ]
      mul.summ <- paste(round(mul[,1], decimal), " (", round(mul[, 1] - 1.96*mul[, 2], decimal), "," ,round(mul[, 1] + 1.96*mul[, 2], decimal), ")", sep ="")
      mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <=0.001, "< 0.001", as.character(round(mul[, 4], decimal +1)))))
      colnames(mul.res) <- c(paste("adj. coeff.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
    } else{
      summ <- paste(round(exp(uni[,1]), decimal), " (", round(exp(uni[, 1] - 1.96*uni[, 2]), decimal), "," ,round(exp(uni[, 1] + 1.96*uni[, 2]), decimal), ")", sep ="")
      uni.res <- t(rbind(summ, ifelse(uni[, 4] <=0.001, "< 0.001", as.character(round(uni[, 4], decimal +1)))))
      colnames(uni.res) <- c(paste("crude OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "crude P value")
      rownames(uni.res) <-rownames(uni)
      mul <- coefNA(model)[-1, ]
      mul.summ <- paste(round(exp(mul[,1]), decimal), " (", round(exp(mul[, 1] - 1.96*mul[, 2]), decimal), "," ,round(exp(mul[, 1] + 1.96*mul[, 2]), decimal), ")", sep ="")
      mul.res <- t(rbind(mul.summ, ifelse(mul[, 4] <=0.001, "< 0.001", as.character(round(mul[, 4], decimal +1)))))
      colnames(mul.res) <- c(paste("adj. OR.(", 100 - 100 * 0.05, "%CI)", sep = ""), "adj. P value")
    }
    
    res <-cbind(uni.res[rownames(uni.res) %in% rownames(mul.res), ], mul.res)
    rownames(res) <- rownames(mul)
  }
  
  ## label
  fix.all <- res
  
  ## rownames
  fix.all.list <- lapply(1:length(xs), function(x){fix.all[rownames(fix.all) %in% rn.uni[[x]], ]})
  varnum.mfac <- which(lapply(fix.all.list, length) > ncol(fix.all))
  lapply(varnum.mfac, function(x){fix.all.list[[x]] <<- rbind(rep(NA, ncol(fix.all)), fix.all.list[[x]])})
  fix.all.unlist <- Reduce(rbind, fix.all.list)
  
  rn.list <- lapply(1:length(xs), function(x){rownames(fix.all)[rownames(fix.all) %in% rn.uni[[x]]]})
  varnum.2fac <- which(xs %in% names(model$xlevels)[lapply(model$xlevels, length) == 2])
  lapply(varnum.2fac, function(x){rn.list[[x]] <<- paste(xs[x], ": ", model$xlevels[[xs[x]]][2], " vs ", model$xlevels[[xs[x]]][1], sep="")})
  lapply(varnum.mfac, function(x){rn.list[[x]] <<- c(paste(xs[x],": ref.=", model$xlevels[[xs[x]]][1], sep=""), gsub(xs[x],"   ", rn.list[[x]]))})
  if (class(fix.all.unlist)[1] == "character"){
    fix.all.unlist <- t(data.frame(fix.all.unlist))
  }
  rownames(fix.all.unlist) <- unlist(rn.list)
  
  #pv.colnum = which(colnames(fix.all.unlist) %in% c("P value", "crude P value", "adj. P value"))
  #for (i in pv.colnum){
  #  fix.all.unlist[, i] = ifelse(as.numeric(fix.all.unlist[, i]) < 0.001, "< 0.001", round(as.numeric(fix.all.unlist[, i]), decimal + 1))
  #}
  
  
  outcome.name <- y
  
  
  if (gaussianT) {
    first.line <- paste("Linear regression predicting ", outcome.name, sep = "", "\n")
    last.lines <- paste("No. of observations = ",
                        length(model$y), "\n", "R-squared = ", round(cor(model$y, predict(model))^2, decimal + 2), "\n", 
                        "AIC value = ", round(model$aic, decimal + 2), "\n", "\n", sep = "")
  }
  else {
    first.line <- paste("Logistic regression predicting ", outcome.name, sep = "", "\n")
    last.lines <-  paste("No. of observations = ",
                         length(model$y), "\n", 
                         "AIC value = ", round(model$aic, decimal + 2), "\n", "\n", sep = "")
  }
  
  results <- list(first.line = first.line, table = fix.all.unlist,
                  last.lines = last.lines)
  class(results) <- c("display", "list")
  return(results)
  
}

