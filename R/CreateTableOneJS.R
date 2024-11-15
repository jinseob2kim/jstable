## Create Table1 : include 2 strata


#' @title CreateTableOne2: Modified CreateTableOne function in tableone package
#' @description Combine CreateTableOne & print function in tableone package
#' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
#' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
#' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
#' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. Do not include factors, unless you need to relevel them by removing empty levels. If omitted, only factors are considered categorical variables. The variables specified here must also be specified in the vars argument.
#' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables., Default: F
#' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed, Default: T
#' @param testApprox A function used to perform the large sample approximation based tests. The default is chisq.test. This is not recommended when some of the cell have small counts like fewer than 5, Default: chisq.test
#' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is list(correct = TRUE), which turns on the continuity correction for chisq.test, Default: list(correct = TRUE)
#' @param testExact A function used to perform the exact tests. The default is fisher.test. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice., Default: fisher.test
#' @param argsExact A named list of arguments passed to the function specified in testExact. The default is list(workspace = 2 * 10^5), which specifies the memory space allocated for fisher.test, Default: list(workspace = 2 * 10^5)
#' @param testNormal A function used to perform the normal assumption based tests. The default is oneway.test. This is equivalent of the t-test when there are only two groups, Default: oneway.test
#' @param argsNormal A named list of arguments passed to the function specified in testNormal. The default is list(var.equal = TRUE), which makes it the ordinary ANOVA that assumes equal variance across groups., Default: list(var.equal = F)
#' @param testNonNormal A function used to perform the nonparametric tests. The default is kruskal.test (Kruskal-Wallis Rank Sum Test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups, Default: kruskal.test
#' @param argsNonNormal A named list of arguments passed to the function specified in testNonNormal. The default is list(NULL), which is just a placeholder., Default: list(NULL)
#' @param showAllLevels Whether to show all levels. FALSE by default, i.e., for 2-level categorical variables, only the higher level is shown to avoid redundant information., Default: T
#' @param printToggle Whether to print the output. If FALSE, no output is created, and a matrix is invisibly returned., Default: F
#' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily, Default: F
#' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated, Default: F
#' @param Labels Use Label, Default: F
#' @param exact A character vector to specify the variables for which the p-values should be those of exact tests. By default all p-values are from large sample approximation tests (chisq.test)., Default: NULL
#' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test)., Default: NULL
#' @param catDigits Number of digits to print for proportions., Default: 1
#' @param contDigits Number of digits to print for continuous variables. Default 2.
#' @param pDigits Number of digits to print for p-values (also used for standardized mean differences), Default: 3
#' @param labeldata labeldata to use, Default: NULL
#' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
#' @param showpm Logical, show normal distributed continuous variables as Mean ± SD. Default: T
#' @param addOverall (optional, only used if strata are supplied) Adds an overall column to the table. Smd and p-value calculations are performed using only the stratifed clolumns. Default: F
#' @param pairwise (optional, only used if strata are supplied) When there are three or more strata, it displays the p-values for pairwise comparisons. Default: F
#' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via write.csv.
#' @details DETAILS
#' @examples
#' library(survival)
#' CreateTableOne2(vars = names(lung), strata = "sex", data = lung)
#' @rdname CreateTableOne2
#' @importFrom data.table data.table := setkey
#' @importFrom tableone CreateTableOne
#' @importFrom labelled var_label var_label<-
#' @importFrom stats chisq.test fisher.test kruskal.test oneway.test t.test wilcox.test
#' @importFrom utils combn
#' @importFrom methods is
#' @export

CreateTableOne2 <- function(data, strata, vars, factorVars, includeNA = F, test = T,
                            testApprox = chisq.test, argsApprox = list(correct = TRUE),
                            testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                            testNormal = oneway.test, argsNormal = list(var.equal = F),
                            testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                            showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = F, exact = NULL, nonnormal = NULL,
                            catDigits = 1, contDigits = 2, pDigits = 3, labeldata = NULL, minMax = F, showpm = T, addOverall = F, pairwise = F) {
  setkey <- variable <- level <- . <- val_label <- NULL

  if (length(strata) != 1) {
    stop("Please select only 1 strata")
  }

  vars.ex <- names(which(sapply(vars, function(x) {
    !(class(data[[x]]) %in% c("integer", "numeric", "factor", "character"))
  })))

  if (length(vars.ex) > 0) {
    warning("Variables other than numeric or factor types are excluded.")
    vars <- setdiff(vars, vars.ex)
  }


  res <- tableone::CreateTableOne(
    vars = vars, strata = strata, data = data, factorVars = factorVars, includeNA = includeNA, test = test,
    testApprox = testApprox, argsApprox = argsApprox,
    testExact = testExact, argsExact = argsExact,
    testNormal = testNormal, argsNormal = argsNormal,
    testNonNormal = testNonNormal, argsNonNormal = argsNonNormal, smd = smd, addOverall = addOverall
  )

  # factor_vars <- vars[sapply(vars, function(x){class(data[[x]]) %in% c("factor", "character")})]
  factor_vars <- res[["MetaData"]][["varFactors"]]

  if (Labels & !is.null(labeldata)) {
    labelled::var_label(data) <- sapply(names(data), function(v) {
      as.character(labeldata[get("variable") == v, "var_label"][1])
    }, simplify = F)
    # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
    data.table::setkey(labeldata, variable, level)
    res0 <- tableone::CreateTableOne(vars = vars, data = data, factorVars = factorVars, includeNA = includeNA)
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

  vars.fisher <- sapply(factor_vars, function(x) {
    is(tryCatch(chisq.test(table(data[[strata]], data[[x]])), error = function(e) e, warning = function(w) w), "warning")
  })
  vars.fisher <- factor_vars[unlist(vars.fisher)]

  if (is.null(exact) & length(vars.fisher) > 0) {
    exact <- vars.fisher
  }

  ptb1 <- print(res,
    showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, smd = smd, varLabels = Labels, nonnormal = nonnormal, exact = exact,
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
    colname.group_var <- unlist(labeldata[.(strata, names(res$CatTable)), val_label])
    if (is.na(colname.group_var[1]) & addOverall) {
      colname.group_var[1] <- "Overall"
    }
    if (showAllLevels == T) {
      # colname.group_var <- unlist(labeldata[get("variable") == strata, "val_label"])
      colnames(ptb1)[1:(length(colname.group_var) + 1)] <- unlist(c(labeldata[get("variable") == strata, "var_label"][1], colname.group_var))
    } else {
      colnames(ptb1)[1:length(colname.group_var)] <- colname.group_var
    }
    # ptb1[,1] = vals.tb1
    # cap.tb1 = paste("Table 1: Stratified by ", labeldata[variable == strata, "var_label"][1], sep="")
  }

  if (pairwise && length(unique(data[[strata]])) > 2) {
    # 정규성 테스트 수행
    normality_results <- sapply(vars, function(x) {
      is_continuous <- is.numeric(data[[x]]) || is.integer(data[[x]])
      if (is_continuous && nrow(data) <= 5000) {
        shapiro_test_result <- tryCatch(stats::shapiro.test(data[[x]])$p.value, error = function(e) NA)
        return(!is.na(shapiro_test_result) && shapiro_test_result >= 0.05) # p >= 0.05면 정규성 만족
      } else {
        return(TRUE) # 큰 샘플이거나 정규성 검사가 불가할 때는 정규성 만족으로 처리
      }
    })

    p_position <- which(colnames(ptb1) == "p")
    strata_count <- length(unique(data$variables[[strata]]))
    comparison_columns <- colnames(ptb1)[(p_position - strata_count):(p_position - 1)]
    pairwise_comparisons <- combn(
      comparison_columns, 2,
      simplify = FALSE
    )
    pairwise_pvalues_list <- lapply(vars, function(x) {
      sapply(pairwise_comparisons, function(pair) {
        subset_data <- data[data[[strata]] %in% pair, ]
        is_continuous <- is.numeric(data[[x]]) || is.integer(data[[x]])
        test_result <- if (is_continuous) {
          if (normality_results[x]) {
            tryCatch(
              {
                test <- t.test(subset_data[[x]] ~ subset_data[[strata]], var.equal = FALSE) # Welch's t-test
                list(p_value = test$p.value, test_used = "t-test")
              },
              error = function(e) {
                list(p_value = NA, test_used = NA)
              }
            )
          } else {
            tryCatch(
              {
                test <- wilcox.test(subset_data[[x]] ~ subset_data[[strata]])
                list(p_value = test$p.value, test_used = "wilcox")
              },
              error = function(e) {
                list(p_value = NA, test_used = NA)
              }
            )
          }
        } else {
          # 범주형 데이터에 대해 Chi-square 또는 Fisher's exact test 수행
          tryCatch(
            {
              test <- chisq.test(table(subset_data[[strata]], subset_data[[x]]))
              list(p_value = test$p.value, test_used = "chisq")
            },
            warning = function(w) {
              test <- fisher.test(table(subset_data[[strata]], subset_data[[x]]))
              list(p_value = test$p.value, test_used = "exact")
            },
            error = function(e) {
              list(p_value = NA, test_used = NA)
            }
          )
        }
        return(test_result)
      }, simplify = FALSE)
    })
    names(pairwise_pvalues_list) <- vars

    for (i in seq_along(pairwise_comparisons)) {
      col_name <- paste0("p(", pairwise_comparisons[[i]][1], "vs", pairwise_comparisons[[i]][2], ")")
      test_name <- paste0("test(", pairwise_comparisons[[i]][1], "vs", pairwise_comparisons[[i]][2], ")")
      ptb1 <- cbind(ptb1, col_name = "", test_name = "")
      colnames(ptb1)[ncol(ptb1) - 1] <- col_name
      colnames(ptb1)[ncol(ptb1)] <- test_name
    }

    for (i in seq_along(pairwise_comparisons)) {
      col_name <- paste0("p(", pairwise_comparisons[[i]][1], "vs", pairwise_comparisons[[i]][2], ")")
      test_name <- paste0("test(", pairwise_comparisons[[i]][1], "vs", pairwise_comparisons[[i]][2], ")")
      for (x in vars) {
        p_value <- pairwise_pvalues_list[[x]][[i]]$p_value
        test_used <- pairwise_pvalues_list[[x]][[i]]$test_used
        cleaned_row_names <- gsub("\\s+|\\(\\%\\)", "", rownames(ptb1))
        cleaned_var_name <- gsub("\\s+|\\(\\%\\)", "", x)
        first_row <- which(cleaned_row_names == cleaned_var_name)[1]
        p_value <- ifelse(p_value < 0.001, "<0.001", as.character(round(p_value, 2)))
        ptb1[first_row, col_name] <- p_value
        ptb1[first_row, test_name] <- test_used
      }
    }
    cols_to_remove <- grep("^test\\(", colnames(ptb1))
    ptb1 <- ptb1[, -cols_to_remove]
  }
  sig <- ifelse(ptb1[, "p"] == "<0.001", "0", ptb1[, "p"])
  sig <- as.numeric(as.vector(sig))
  sig <- ifelse(sig <= 0.05, "**", "")
  ptb1 <- cbind(ptb1, sig)
  return(ptb1)
}



#' @title CreateTableOneJS: Modified CreateTableOne function in tableone package
#' @description Combine CreateTableOne & print function in tableone package
#' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
#' @param strata Stratifying grouping variable name(s) given as a character vector. If omitted, the overall results are returned.
#' @param strata2 Stratifying 2nd grouping variable name(s) given as a character vector. If omitted, the 1 group results are returned.
#' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
#' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. Do not include factors, unless you need to relevel them by removing empty levels. If omitted, only factors are considered categorical variables. The variables specified here must also be specified in the vars argument.
#' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables., Default: F
#' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed, Default: T
#' @param testApprox A function used to perform the large sample approximation based tests. The default is chisq.test. This is not recommended when some of the cell have small counts like fewer than 5, Default: chisq.test
#' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is list(correct = TRUE), which turns on the continuity correction for chisq.test, Default: list(correct = TRUE)
#' @param testExact A function used to perform the exact tests. The default is fisher.test. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice., Default: fisher.test
#' @param argsExact A named list of arguments passed to the function specified in testExact. The default is list(workspace = 2 * 10^5), which specifies the memory space allocated for fisher.test, Default: list(workspace = 2 * 10^5)
#' @param testNormal A function used to perform the normal assumption based tests. The default is oneway.test. This is equivalent of the t-test when there are only two groups, Default: oneway.test
#' @param argsNormal A named list of arguments passed to the function specified in testNormal. The default is list(var.equal = TRUE), which makes it the ordinary ANOVA that assumes equal variance across groups., Default: list(var.equal = F)
#' @param testNonNormal A function used to perform the nonparametric tests. The default is kruskal.test (Kruskal-Wallis Rank Sum Test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups, Default: kruskal.test
#' @param argsNonNormal A named list of arguments passed to the function specified in testNonNormal. The default is list(NULL), which is just a placeholder., Default: list(NULL)
#' @param showAllLevels Whether to show all levels. FALSE by default, i.e., for 2-level categorical variables, only the higher level is shown to avoid redundant information., Default: T
#' @param printToggle Whether to print the output. If FALSE, no output is created, and a matrix is invisibly returned., Default: F
#' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily, Default: F
#' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated, Default: F
#' @param Labels Use Label, Default: F
#' @param exact A character vector to specify the variables for which the p-values should be those of exact tests. By default all p-values are from large sample approximation tests (chisq.test)., Default: NULL
#' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test)., Default: NULL
#' @param catDigits Number of digits to print for proportions. Default: 1
#' @param contDigits Number of digits to print for continuous variables. Default 2.
#' @param pDigits Number of digits to print for p-values (also used for standardized mean differences), Default: 3
#' @param labeldata labeldata to use, Default: NULL
#' @param psub show sub-group p-values, Default: F
#' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
#' @param showpm Logical, show normal distributed continuous variables as Mean ± SD. Default: T
#' @param addOverall (optional, only used if strata are supplied) Adds an overall column to the table. Smd and p-value calculations are performed using only the stratifed clolumns. Default: F
#' @param normalityTest Logical, perform the Shapiro test for all variables. Default: F
#' @param pairwise (optional, only used if strata are supplied) When there are three or more strata, it displays the p-values for pairwise comparisons. Default: F#' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via write.csv.
#' @details DETAILS
#' @examples
#' library(survival)
#' CreateTableOneJS(vars = names(lung), strata = "sex", data = lung)
#' @rdname CreateTableOneJS
#' @importFrom data.table data.table := CJ
#' @importFrom tableone CreateTableOne
#' @importFrom labelled var_label var_label<-
#' @importFrom stats chisq.test fisher.test kruskal.test oneway.test shapiro.test
#' @importFrom methods is
#' @export


CreateTableOneJS <- function(vars, strata = NULL, strata2 = NULL, data, factorVars = NULL, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = F, exact = NULL, nonnormal = NULL,
                             catDigits = 1, contDigits = 2, pDigits = 3, labeldata = NULL, psub = T, minMax = F, showpm = T,
                             addOverall = F, normalityTest = F, pairwise = F) {
  . <- level <- variable <- val_label <- V1 <- V2 <- NULL
  # if (Labels & !is.null(labeldata)){
  #  var_label(data) = sapply(names(data), function(v){as.character(labeldata[get("variable") == v, "var_label"][1])}, simplify = F)
  #  vals.tb1 = c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
  # }
  data <- data

  # 모든 변수에 대해 shapiro test 수행
  if (normalityTest == T) {
    if (nrow(data) > 5000) {
      print("Warning: Shapiro test is not possible due to the large sample size.")
    } else {
      if (!is.null(nonnormal)) {
        print("Warning: Nonnormal variables previously entered are ignored.")
      }
      nonnormal <- setdiff(names(data), factorVars)
      nonnormal <- nonnormal[sapply(nonnormal, function(x) {
        ifelse(class(data[[x]]) %in% c("integer", "numeric"), stats::shapiro.test(data[[x]])$p.value < 0.05, F)
      })]
    }
  }

  if (is.null(strata)) {
    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data) <- sapply(names(data), function(v) {
        as.character(labeldata[get("variable") == v, "var_label"][1])
      }, simplify = F)
      data.table::setkey(labeldata, variable, level)
    }

    res <- tableone::CreateTableOne(
      vars = vars, data = data, factorVars = factorVars, includeNA = includeNA, test = test,
      testApprox = testApprox, argsApprox = argsApprox,
      testExact = testExact, argsExact = argsExact,
      testNormal = testNormal, argsNormal = argsNormal,
      testNonNormal = testNonNormal, argsNonNormal = argsNonNormal, smd = smd
    )

    factor_vars <- res[["MetaData"]][["varFactors"]]

    if (Labels & !is.null(labeldata)) {
      for (i in seq_along(res$CatTable)) {
        for (j in factor_vars) {
          lvs <- res$CatTable[[i]][[j]]$level
          res$CatTable[[i]][[j]]$level <- labeldata[.(j, lvs), val_label]
        }
      }
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
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
    cap.tb1 <- "Total"
    # if (Labels & !is.null(labeldata)){
    #  ptb1[,1] <- vals.tb1
    # }
    return(list(table = ptb1, caption = cap.tb1))
  } else if (is.null(strata2)) {
    ptb1 <- CreateTableOne2(
      strata = strata, vars = vars, data = data, factorVars = factorVars, includeNA = includeNA, test = test,
      testApprox = testApprox, argsApprox = argsApprox,
      testExact = testExact, argsExact = argsExact,
      testNormal = testNormal, argsNormal = argsNormal,
      testNonNormal = testNonNormal, argsNonNormal = argsNonNormal, smd = smd,
      showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal, exact = exact,
      catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, labeldata = labeldata, minMax = minMax, showpm = showpm, addOverall = addOverall, pairwise = pairwise
    )

    cap.tb1 <- paste("Stratified by ", strata, sep = "")

    if (Labels & !is.null(labeldata)) {
      cap.tb1 <- paste("Stratified by ", labeldata[get("variable") == strata, "var_label"][1], sep = "")
      # ptb1[,1] = vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
  } else if (psub == T) {
    # data.strata <-  lapply(levels(data[[strata]]), function(x){data[data[[strata]] == x, ]})
    data.strata <- split(data, data[[strata]])
    ptb1.list <- lapply(data.strata, CreateTableOne2,
      vars = vars, strata = strata2, factorVars = factorVars, includeNA = includeNA, test = test,
      testApprox = testApprox, argsApprox = argsApprox,
      testExact = testExact, argsExact = argsExact,
      testNormal = testNormal, argsNormal = argsNormal,
      testNonNormal = testNonNormal, argsNonNormal = argsNonNormal, smd = smd,
      showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = F, nonnormal = nonnormal, exact = exact,
      catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, minMax = minMax, showpm = T, addOverall = F
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
    cap.tb1 <- paste("Stratified by ", strata, "(", paste(levels(data[[strata]]), collapse = ", "), ") & ", strata2, sep = "")
    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data.strata[[1]]) <- sapply(names(data.strata[[1]]), function(v) {
        as.character(labeldata[get("variable") == v, "var_label"][1])
      }, simplify = F)
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
      data.table::setkey(labeldata, variable, level)

      res <- tableone::CreateTableOne(vars = vars, data = data.strata[[1]], factorVars = factorVars, includeNA = includeNA)
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

      cap.tb1 <- paste("Stratified by ", labeldata[get("variable") == strata, "var_label"][1], "(", paste(unlist(labeldata[get("variable") == strata, "val_label"]), collapse = ", "), ") & ", labeldata[get("variable") == strata2, "var_label"][1], sep = "")
    }

    return(list(table = ptb1.cbind, caption = cap.tb1))
  } else {
    res <- tableone::CreateTableOne(
      vars = vars, strata = c(strata2, strata), data = data, factorVars = factorVars, includeNA = F, test = T,
      testApprox = chisq.test, argsApprox = list(correct = TRUE),
      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
      testNormal = oneway.test, argsNormal = list(var.equal = F),
      testNonNormal = kruskal.test, argsNonNormal = list(NULL), addOverall = addOverall
    )

    factor_vars <- res[["MetaData"]][["varFactors"]]
    # factor_vars <- vars[sapply(vars, function(x){class(data[[x]]) %in% c("factor", "character")})]
    var.strata <- paste(data[[strata2]], data[[strata]], sep = "_")

    vars.fisher <- sapply(factor_vars, function(x) {
      is(tryCatch(chisq.test(table(var.strata, data[[x]])), error = function(e) e, warning = function(w) w), "warning")
    })
    vars.fisher <- factor_vars[unlist(vars.fisher)]

    if (is.null(exact) & length(vars.fisher) > 0) {
      exact <- vars.fisher
    }

    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data) <- sapply(names(data), function(v) {
        as.character(labeldata[get("variable") == v, "var_label"][1])
      }, simplify = F)
      data.table::setkey(labeldata, variable, level)
      res0 <- tableone::CreateTableOne(vars = vars, data = data, factorVars = factorVars, includeNA = includeNA)
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
      printToggle = F, quote = F, smd = smd, varLabels = Labels, exact = exact, nonnormal = nonnormal,
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
    cap.tb1 <- paste("Table 1: Stratified by ", strata, " and ", strata2, sep = "")


    # Column name
    if (Labels & !is.null(labeldata)) {
      val_combination <- data.table::CJ(labeldata[variable == strata, val_label], labeldata[variable == strata2, val_label], sorted = F)
      colname.group_var <- val_combination[, paste(V1, ":", V2, sep = "")]
      colname.group_index <- paste(labeldata[variable == strata, var_label][1], ":", labeldata[variable == strata2, var_label][1], sep = "")
      if (showAllLevels == T) {
        if (addOverall) {
          colnames(ptb1)[1:(length(colname.group_var) + 2)] <- c(colname.group_index, "Overall", colname.group_var)
        } else {
          colnames(ptb1)[1:(length(colname.group_var) + 1)] <- c(colname.group_index, colname.group_var)
        }
      } else {
        if (addOverall) {
          colnames(ptb1)[1:length(colname.group_var) + 1] <- colname.group_var
        } else {
          colnames(ptb1)[1:length(colname.group_var)] <- colname.group_var
        }
      }

      # caption
      cap.tb1 <- paste("Stratified by ", labeldata[variable == strata, var_label][1], " and ", labeldata[variable == strata2, var_label][1], sep = "")
      # val_label
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[variable == v, val_label]})))
      # ptb1[,1] <- vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
  }
}
