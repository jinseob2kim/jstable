## svyCreate Table1 : include 2 strata


#' @title svyCreateTableOne2: Modified svyCreateTableOne function in tableone package
#' @description Combine svyCreateTableOne & print function in tableone package
#' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
#' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
#' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
#' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. Do not include factors, unless you need to relevel them by removing empty levels. If omitted, only factors are considered categorical variables. The variables specified here must also be specified in the vars argument.
#' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables., Default: F
#' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed, Default: T
#' @param showAllLevels Whether to show all levels. FALSE by default, i.e., for 2-level categorical variables, only the higher level is shown to avoid redundant information., Default: T
#' @param printToggle Whether to print the output. If FALSE, no output is created, and a matrix is invisibly returned., Default: F
#' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily, Default: F
#' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated, Default: F
#' @param Labels Use Label, Default: F
#' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test)., Default: NULL
#' @param catDigits Number of digits to print for proportions., Default: 1
#' @param contDigits Number of digits to print for continuous variables. Default 2.
#' @param pDigits Number of digits to print for p-values (also used for standardized mean differences), Default: 3
#' @param labeldata labeldata to use, Default: NULL
#' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
#' @param showpm Logical, show normal distributed continuous variables as Mean ± SD. Default: T
#' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via write.csv.
#' @details DETAILS
#' @examples
#' library(survey)
#' data(nhanes)
#' nhanes$SDMVPSU <- as.factor(nhanes$SDMVPSU)
#' nhanesSvy <- svydesign(
#'   ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
#'   nest = TRUE, data = nhanes
#' )
#' svyCreateTableOne2(
#'   vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
#'   strata = "RIAGENDR", data = nhanesSvy,
#'   factorVars = c("HI_CHOL", "race", "RIAGENDR")
#' )
#' @rdname svyCreateTableOne2
#' @importFrom data.table data.table :=
#' @importFrom tableone svyCreateTableOne
#' @importFrom labelled var_label var_label<-
#' @export

svyCreateTableOne2 <- function(data, strata, vars, factorVars, includeNA = F, test = T,
                               showAllLevels = T, printToggle = F, quote = F, smd = F, nonnormal = NULL,
                               catDigits = 1, contDigits = 2, pDigits = 3, Labels = F, labeldata = NULL, minMax = F, showpm = T) {
  setkey <- variable <- level <- . <- val_label <- NULL

  if (length(strata) != 1) {
    stop("Please select only 1 strata")
  }

  vars.ex <- names(which(sapply(vars, function(x) {
    !(class(data$variables[[x]]) %in% c("integer", "numeric", "factor", "character"))
  })))

  if (length(vars.ex) > 0) {
    warning("Variables other than numeric or factor types are excluded.")
    vars <- setdiff(vars, vars.ex)
  }


  res <- tableone::svyCreateTableOne(
    vars = vars, strata = strata, data = data, factorVars = factorVars, includeNA = includeNA, test = test,
    smd = smd
  )

  factor_vars <- res[["MetaData"]][["varFactors"]]

  if (Labels & !is.null(labeldata)) {
    labelled::var_label(data$variables) <- sapply(names(data$variables), function(v) {
      as.character(labeldata[get("variable") == v, "var_label"][1])
    }, simplify = F)
    # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
    data.table::setkey(labeldata, variable, level)
    res0 <- tableone::svyCreateTableOne(vars = vars, data = data, factorVars = factorVars, includeNA = includeNA)
    for (i in seq_along(res0$CatTable)) {
      for (j in factor_vars) {
        lvs <- res0$CatTable[[i]][[j]]$level
        res0$CatTable[[i]][[j]]$level <- labeldata[.(j, lvs), val_label]
      }
    }
    ptb1.res0 <- print(res0,
      showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, varLabels = Labels, nonnormal = nonnormal,
      catDigits = catDigits, contDigits = contDigits, minMax = minMax
    )
    ptb1.rn <- rownames(ptb1.res0)
    ptb1.rn <- gsub("(mean (SD))", "", ptb1.rn, fixed = T)
  }


  ptb1 <- print(res,
    showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, smd = smd, varLabels = Labels, nonnormal = nonnormal,
    catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, minMax = minMax
  )

  if (showpm) {
    ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\(", "\u00B1 ", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ])
    ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\)", "", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ])
  }

  rownames(ptb1) <- gsub("(mean (SD))", "", rownames(ptb1), fixed = T)
  if (Labels & !is.null(labeldata)) {
    rownames(ptb1) <- ptb1.rn
    if (showAllLevels == T) ptb1[, 1] <- ptb1.res0[, 1]
  }
  # cap.tb1 = paste("Table 1: Stratified by ", strata, sep="")

  if (Labels & !is.null(labeldata)) {
    colname.group_var <- unlist(labeldata[get("variable") == strata, "val_label"])
    if (showAllLevels == T) {
      # colname.group_var <- unlist(labeldata[get("variable") == strata, "val_label"])
      colnames(ptb1)[1:(length(colname.group_var) + 1)] <- unlist(c(labeldata[get("variable") == strata, "var_label"][1], colname.group_var))
    } else {
      colnames(ptb1)[1:length(colname.group_var)] <- colname.group_var
    }
  }

  sig <- ifelse(ptb1[, "p"] == "<0.001", "0", ptb1[, "p"])
  sig <- as.numeric(as.vector(sig))
  sig <- ifelse(sig <= 0.05, "**", "")
  ptb1 <- cbind(ptb1, sig)
  return(ptb1)
}



#' @title svyCreateTableOneJS: Modified CreateTableOne function in tableone package
#' @description Combine svyCreateTableOne & print function in tableone package
#' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
#' @param strata Stratifying grouping variable name(s) given as a character vector. If omitted, the overall results are returned.
#' @param strata2 Stratifying 2nd grouping variable name(s) given as a character vector. If omitted, the 1 group results are returned.
#' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
#' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. Do not include factors, unless you need to relevel them by removing empty levels. If omitted, only factors are considered categorical variables. The variables specified here must also be specified in the vars argument.
#' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables., Default: F
#' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed, Default: T
#' @param showAllLevels Whether to show all levels. FALSE by default, i.e., for 2-level categorical variables, only the higher level is shown to avoid redundant information., Default: T
#' @param printToggle Whether to print the output. If FALSE, no output is created, and a matrix is invisibly returned., Default: F
#' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily, Default: F
#' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated, Default: F
#' @param Labels Use Label, Default: F
#' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test)., Default: NULL
#' @param catDigits Number of digits to print for proportions., Default: 1
#' @param contDigits Number of digits to print for continuous variables. Default 2.
#' @param pDigits Number of digits to print for p-values (also used for standardized mean differences), Default: 3
#' @param labeldata labeldata to use, Default: NULL
#' @param psub show sub-group p-values, Default: F
#' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
#' @param showpm Logical, show normal distributed continuous variables as Mean ± SD. Default: T
#' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via write.csv.
#' @details DETAILS
#' @examples
#' library(survey)
#' data(nhanes)
#' nhanes$SDMVPSU <- as.factor(nhanes$SDMVPSU)
#' nhanesSvy <- svydesign(
#'   ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
#'   nest = TRUE, data = nhanes
#' )
#' svyCreateTableOneJS(
#'   vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
#'   strata = "RIAGENDR", data = nhanesSvy,
#'   factorVars = c("HI_CHOL", "race", "RIAGENDR")
#' )
#' @rdname svyCreateTableOneJS
#' @importFrom data.table data.table := CJ
#' @importFrom tableone svyCreateTableOne
#' @importFrom labelled var_label var_label<-
#' @export


svyCreateTableOneJS <- function(vars, strata = NULL, strata2 = NULL, data, factorVars = NULL, includeNA = F, test = T,
                                showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = F, nonnormal = NULL,
                                catDigits = 1, contDigits = 2, pDigits = 3, labeldata = NULL, psub = T, minMax = F, showpm = T) {
  . <- level <- variable <- val_label <- V1 <- V2 <- NULL

  # if (Labels & !is.null(labeldata)){
  #  var_label(data) = sapply(names(data), function(v){as.character(labeldata[get("variable") == v, "var_label"][1])}, simplify = F)
  #  vals.tb1 = c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
  # }
  data <- data

  if (is.null(strata)) {
    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data$variables) <- sapply(names(data$variables), function(v) {
        as.character(labeldata[get("variable") == v, "var_label"][1])
      }, simplify = F)
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
      data.table::setkey(labeldata, variable, level)
    }

    res <- tableone::svyCreateTableOne(vars = vars, data = data, factorVars = factorVars, includeNA = includeNA)
    factor_vars <- res[["MetaData"]][["varFactors"]]

    if (Labels & !is.null(labeldata)) {
      for (i in seq_along(res$CatTable)) {
        for (j in factor_vars) {
          lvs <- res$CatTable[[i]][[j]]$level
          res$CatTable[[i]][[j]]$level <- labeldata[.(j, lvs), val_label]
        }
      }
    }

    ptb1 <- print(res,
      showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, varLabels = Labels, nonnormal = nonnormal,
      catDigits = catDigits, contDigits = contDigits, minMax = minMax
    )

    if (showpm) {
      ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\(", "\u00B1 ", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ])
      ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\)", "", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ])
    }

    rownames(ptb1) <- gsub("(mean (SD))", "", rownames(ptb1), fixed = T)

    cap.tb1 <- "Total - weighted data"
    # if (Labels & !is.null(labeldata)){
    #  ptb1[,1] <- vals.tb1
    # }
    return(list(table = ptb1, caption = cap.tb1))
  } else if (is.null(strata2)) {
    ptb1 <- svyCreateTableOne2(
      strata = strata, vars = vars, data = data, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
      showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal,
      catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, labeldata = labeldata, minMax = minMax, showpm = showpm
    )

    cap.tb1 <- paste("Stratified by ", strata, "- weighted data", sep = "")

    if (Labels & !is.null(labeldata)) {
      cap.tb1 <- paste("Stratified by ", labeldata[get("variable") == strata, "var_label"][1], "- weighted data", sep = "")
      # ptb1[,1] = vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
  } else if (psub == T) {
    data.strata <- lapply(setdiff(unique(data$variable[[strata]]), NA), function(x) {
      subset(data, get(strata) == x)
    })
    ptb1.list <- lapply(data.strata, svyCreateTableOne2,
      vars = vars, strata = strata2, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
      showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = F, nonnormal = nonnormal,
      catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, minMax = minMax, showpm = showpm
    )


    if (showAllLevels == T) {
      ptb1.cbind <- Reduce(cbind, c(list(ptb1.list[[1]]), lapply(2:length(ptb1.list), function(x) {
        ptb1.list[[x]][, -1]
      })))
    } else {
      ptb1.cbind <- Reduce(cbind, ptb1.list)
    }

    # colnum.test = which(colnames(ptb1.cbind) == "test")
    # ptb1.2group = ptb1.cbind[, c(setdiff(1:ncol(ptb1.cbind), colnum.test), colnum.test[1])]
    cap.tb1 <- paste("Stratified by ", strata, "(", paste(levels(data[[strata]]), collapse = ", "), ") & ", strata2, "- weighted data", sep = "")
    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data.strata[[1]]$variables) <- sapply(names(data.strata[[1]]$variables), function(v) {
        as.character(labeldata[get("variable") == v, "var_label"][1])
      }, simplify = F)
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
      data.table::setkey(labeldata, variable, level)

      res <- tableone::svyCreateTableOne(vars = vars, data = data.strata[[1]], factorVars = factorVars, includeNA = includeNA)
      factor_vars <- res[["MetaData"]][["varFactors"]]
      for (i in seq_along(res$CatTable)) {
        for (j in factor_vars) {
          lvs <- res$CatTable[[i]][[j]]$level
          res$CatTable[[i]][[j]]$level <- labeldata[.(j, lvs), val_label]
        }
      }

      ptb1.res <- print(res,
        showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, varLabels = Labels, nonnormal = nonnormal,
        catDigits = catDigits, contDigits = contDigits, minMax = minMax
      )
      ptb1.rn <- rownames(ptb1.res)
      rownames(ptb1.cbind) <- gsub("(mean (SD))", "", ptb1.rn, fixed = T)
      if (showAllLevels == T) {
        ptb1.cbind[, 1] <- ptb1.res[, 1]
      }

      cap.tb1 <- paste("Stratified by ", labeldata[get("variable") == strata, "var_label"][1], "(", paste(unlist(labeldata[get("variable") == strata, "val_label"]), collapse = ", "), ") & ", labeldata[get("variable") == strata2, "var_label"][1], "- weighted data", sep = "")
    }

    return(list(table = ptb1.cbind, caption = cap.tb1))
  } else {
    res <- tableone::svyCreateTableOne(vars = vars, strata = c(strata2, strata), data = data, factorVars = factorVars, includeNA = F, test = T)
    factor_vars <- res[["MetaData"]][["varFactors"]]

    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data$variable) <- sapply(names(data$variable), function(v) {
        as.character(labeldata[get("variable") == v, "var_label"][1])
      }, simplify = F)
      data.table::setkey(labeldata, variable, level)
      res0 <- tableone::svyCreateTableOne(vars = vars, data = data, factorVars = factorVars, includeNA = includeNA)
      for (i in seq_along(res0$CatTable)) {
        for (j in factor_vars) {
          lvs <- res0$CatTable[[i]][[j]]$level
          res0$CatTable[[i]][[j]]$level <- labeldata[.(j, lvs), val_label]
        }
      }

      ptb1.res0 <- print(res0,
        showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, varLabels = Labels, nonnormal = nonnormal,
        catDigits = catDigits, contDigits = contDigits, minMax = minMax
      )
      ptb1.rn <- rownames(ptb1.res0)
      ptb1.rn <- gsub("(mean (SD))", "", ptb1.rn, fixed = T)

      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
    }

    ptb1 <- print(res,
      showAllLevels = showAllLevels,
      printToggle = F, quote = F, smd = smd, varLabels = T, nonnormal = nonnormal,
      catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, minMax = minMax
    )

    if (showpm) {
      ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\(", "\u00B1 ", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ])
      ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\)", "", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ])
    }

    rownames(ptb1) <- gsub("(mean (SD))", "", rownames(ptb1), fixed = T)
    if (Labels & !is.null(labeldata)) {
      rownames(ptb1) <- ptb1.rn
      if (showAllLevels == T) {
        ptb1[, 1] <- ptb1.res0[, 1]
      }
    }

    sig <- ifelse(ptb1[, "p"] == "<0.001", "0", ptb1[, "p"])
    sig <- as.numeric(as.vector(sig))
    sig <- ifelse(sig <= 0.05, "**", "")
    ptb1 <- cbind(ptb1, sig)
    cap.tb1 <- paste("Stratified by ", strata, " and ", strata2, "- weighted data", sep = "")

    if (showpm) {
      ptb1[!grepl("(%)", rownames(ptb1)) & ptb1[, "p"] != "", ] <- gsub("\\(", "\u00B1 ", ptb1[!grepl("(%)", rownames(ptb1)) & ptb1[, "p"] != "", ])
      ptb1[!grepl("(%)", rownames(ptb1)) & ptb1[, "p"] != "", ] <- gsub("\\)", "", ptb1[!grepl("(%)", rownames(ptb1)) & ptb1[, "p"] != "", ])
    }

    # Column name
    if (Labels & !is.null(labeldata)) {
      val_combination <- data.table::CJ(labeldata[variable == strata, val_label], labeldata[variable == strata2, val_label], sorted = F)
      colname.group_var <- val_combination[, paste(V1, ":", V2, sep = "")]
      colname.group_index <- paste(labeldata[variable == strata, var_label][1], ":", labeldata[variable == strata2, var_label][1], sep = "")
      if (showAllLevels == T) {
        colnames(ptb1)[1:(length(colname.group_var) + 1)] <- c(colname.group_index, colname.group_var)
      } else {
        colnames(ptb1)[1:length(colname.group_var)] <- colname.group_var
      }
      # caption
      cap.tb1 <- paste("Stratified by ", labeldata[variable == strata, var_label][1], " and ", labeldata[variable == strata2, var_label][1], "- weighted data", sep = "")
      # val_label
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[variable == v, val_label]})))
      # ptb1[,1] <- vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
  }
}
