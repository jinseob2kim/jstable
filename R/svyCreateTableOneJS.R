#' @title ChangeSvyTable: Modify the number n 
#' @description Replace the number of weights taken into account with the number of n in the original data
#' @param svy TableOne object that take weights into account 
#' @param ori TableOne object in the original data 
#' @return A matrix that replaces the n number of weights with the n number of the original data 
#' @details DETAILS
#' @examples
#' # example code
#' library('survey'); library('tableone')
#' data(nhanes)
#' nhanes$SDMVPSU <- as.factor(nhanes$SDMVPSU)
#' nhanesSvy <- svydesign(
#'   ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
#'   nest = TRUE, data = nhanes
#' )
#' 
#' res <- svyCreateTableOne(
#'   vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
#'   strata = "RIAGENDR", data = nhanesSvy,
#'   factorVars = c("HI_CHOL", "race", "RIAGENDR")
#' )
#' 
#' ori <- CreateTableOne(
#'   vars = c("HI_CHOL", "race", "agecat", "RIAGENDR"),
#'   strata = "RIAGENDR", data = nhanes,
#'   factorVars = c("HI_CHOL", "race", "RIAGENDR")
#' )
#' ChangeSvyTable(res, ori)
#' @rdname ChangeSvyTable
#' @export

ChangeSvyTable <- function(svy, ori){
  res <- svy
  
  col_var <- names(svy$CatTable)
  fac_var <- svy$MetaData$varFactors
  cont_var <- setdiff(svy$MetaData$vars, svy$MetaData$varFactors)
  
  for (i in seq_along(res$CatTable)){
    for (j in fac_var){
      res$CatTable[[i]][[j]][,'freq'] <- ori$CatTable[[i]][[j]][,'freq']
    }
  }
  
  if(length(cont_var)==0){
    for (i in seq_along(res$CatTable)){
      res$CatTable[[i]][[1]][ , 'n'] <- ori$CatTable[[i]][[1]][ , 'n']
    }
  }else{
    for (i in seq_along(res$ContTable)){
      res$ContTable[[i]][[1]] <- ori$ContTable[[i]][[1]]
    }
  }
  return(res)
}

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
#' @param addOverall (optional, only used if strata are supplied) Adds an overall column to the table. Smd and p-value calculations are performed using only the stratifed clolumns. Default: F
#' @param pairwise (optional, only used if strata are supplied) When there are three or more strata, it displays the p-values for pairwise comparisons. Default: F
#' @param pairwise.showtest (optional, only used if strata are supplied) When using pairwise comparison, it displays the test used to calculate p-values for pairwise comparisons. Default: F
#' @param n_original Replace the number of weighted n with the n in the original data. Default: T 
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
#' @importFrom survey svychisq svyranktest svyttest
#' @export

svyCreateTableOne2 <- function(data, strata, vars, factorVars, includeNA = F, test = T,
                               showAllLevels = T, printToggle = F, quote = F, smd = F, nonnormal = NULL,
                               catDigits = 1, contDigits = 2, pDigits = 3, Labels = F, labeldata = NULL, minMax = F, showpm = T,
                               addOverall = F, pairwise = F, pairwise.showtest = F, n_original = T) {
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
  
  res2 <- tableone::svyCreateTableOne(
    vars = vars, strata = strata, data = data, factorVars = factorVars, includeNA = includeNA, test = test,
    smd = smd, addOverall = addOverall
  )
  if(n_original){
    ori <- tableone::CreateTableOne(
      vars = vars, strata = strata, data = data$variables, factorVars = factorVars, includeNA = includeNA, test = test,
      smd = smd, addOverall = addOverall
    )
    res <- ChangeSvyTable(res2, ori)
  }else{
    res <- res2
  }
  factor_vars <- res2[["MetaData"]][["varFactors"]]
  
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
                       catDigits = catDigits, contDigits = contDigits, minMax = minMax, addOverall = addOverall
    )
    ptb1.rn <- rownames(ptb1.res0)
    ptb1.rn <- gsub("(mean (SD))", "", ptb1.rn, fixed = T)
  }
  
  #문제
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
    colname.group_var <- unlist(labeldata[get("variable") == strata & get("level") %in% unique(data$variables[[strata]]), "val_label"])
    if (length(colname.group_var) == 0 & addOverall) {
      colname.group_var <- c("Overall")
    }
    if (showAllLevels == T) {
      # colname.group_var <- unlist(labeldata[get("variable") == strata, "val_label"])
      colnames(ptb1)[1:(length(colname.group_var) + 1)] <- unlist(c(labeldata[get("variable") == strata, "var_label"][1], colname.group_var))
    } else {
      colnames(ptb1)[1:length(colname.group_var)] <- colname.group_var
    }
  }
  
  if (pairwise && length(unique(data$variables[[strata]])) > 2) {
    unique_strata <- sort(unique(stats::na.omit(data$variables[[strata]])))
    pairwise_comparisons <- combn(unique_strata, 2, simplify = FALSE)
    pairwise_names <- sapply(pairwise_comparisons, function(pair) {
      paste0("p(", pair[1], " vs ", pair[2], ")")
    })
    pairwise_pvalues <- stats::setNames(
      sapply(pairwise_comparisons, function(pair) {
        subset_data <- subset(data, data$variables[[strata]] %in% pair)
        subset_data$variables[[strata]] <- droplevels(subset_data$variables[[strata]])
        tryCatch(
          {
            table_result <- svyCreateTableOne2(
              data = subset_data, strata = strata, vars = vars, factorVars = factorVars, includeNA = includeNA, test = test,
              showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, smd = smd, nonnormal = nonnormal,
              catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, Labels = Labels, labeldata = labeldata, minMax = minMax, showpm = showpm,
              addOverall = addOverall, pairwise = F, n_original = n_original
            )
            p_values <- table_result[, "p"]
            test_used <- table_result[, "test"]
            list(
              p_value = p_values,
              test_used = test_used
            )
          },
          error = function(e) {
            list(
              p_value = stats::setNames(rep(NA, length(vars)), vars),
              test_used = stats::setNames(rep(NA, length(vars)), vars)
            )
          }
        )
      }, simplify = FALSE),
      nm = pairwise_names
    )
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
      pairwise_key <- paste0("p(", pairwise_comparisons[[i]][1], " vs ", pairwise_comparisons[[i]][2], ")")
      p_value <- pairwise_pvalues[[pairwise_key]]$p_value
      test_used <- pairwise_pvalues[[pairwise_key]]$test_used
      p_value_names <- names(p_value)
      for (x in p_value_names) {
        if (x != "") {
          matched_rows <- match(x, rownames(ptb1))
          if (!is.na(matched_rows)) {
            ptb1[matched_rows, col_name] <- p_value[x]
            ptb1[matched_rows, test_name] <- test_used[x]
          }
        }
      }
    }
    if (!is.null(labeldata) && Labels) {
      pairwise_p_cols <- grep("^p\\(", colnames(ptb1), value = TRUE)
      pairwise_test_cols <- grep("^test\\(", colnames(ptb1), value = TRUE)
      
      strata_labels <- stats::setNames(labeldata[labeldata$variable == strata, val_label], labeldata[labeldata$variable == strata, level])
      updated_p_colnames <- sapply(pairwise_p_cols, function(col_name) {
        match <- regmatches(col_name, regexec("^p\\(([^vs]+)vs([^\\)]+)\\)", col_name))
        if (length(match[[1]]) == 3) {
          group1 <- match[[1]][2]
          group2 <- match[[1]][3]
          label1 <- strata_labels[as.character(group1)]
          label2 <- strata_labels[as.character(group2)]
          if (!is.na(label1) && !is.na(label2)) {
            return(paste0("p(", label1, " vs ", label2, ")"))
          }
        }
        return(col_name)
      })
      updated_test_colnames <- sapply(pairwise_test_cols, function(col_name) {
        match <- regmatches(col_name, regexec("^test\\(([^vs]+)vs([^\\)]+)\\)", col_name))
        if (length(match[[1]]) == 3) {
          group1 <- match[[1]][2]
          group2 <- match[[1]][3]
          label1 <- strata_labels[as.character(group1)]
          label2 <- strata_labels[as.character(group2)]
          if (!is.na(label1) && !is.na(label2)) {
            return(paste0("test(", label1, " vs ", label2, ")"))
          }
        }
        return(col_name)
      })
      
      colnames(ptb1)[colnames(ptb1) %in% pairwise_p_cols] <- updated_p_colnames
      colnames(ptb1)[colnames(ptb1) %in% pairwise_test_cols] <- updated_test_colnames
    }
    if (!pairwise.showtest) {
      cols_to_remove <- grep("^test\\(", colnames(ptb1))
      ptb1 <- ptb1[, -cols_to_remove]
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
#' @param addOverall (optional, only used if strata are supplied) Adds an overall column to the table. Smd and p-value calculations are performed using only the stratifed clolumns. Default: F
#' @param pairwise (optional, only used if strata are supplied) When there are three or more strata, it displays the p-values for pairwise comparisons. Default: F
#' @param pairwise.showtest (optional, only used if strata are supplied) When using pairwise comparison, it displays the test used to calculate p-values for pairwise comparisons. Default: F
#' @param n_original Replace the number of weighted n with the n in the original data. Default: T 
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
                                catDigits = 1, contDigits = 2, pDigits = 3, labeldata = NULL, psub = T, minMax = F, showpm = T,
                                addOverall = F, pairwise = F, pairwise.showtest = F, n_original = T) {
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
    
    res2 <- tableone::svyCreateTableOne(vars = vars, data = data, factorVars = factorVars, includeNA = includeNA)
    factor_vars <- res2[["MetaData"]][["varFactors"]]
    
    if(n_original){
      ori2 <- tableone::CreateTableOne(vars = vars, data = data$variables, factorVars = factorVars, includeNA = includeNA)
      res <- ChangeSvyTable(res2, ori2)
    }else{
      res <- res2
    }
    
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
    if(n_original){
      ptb1 <- svyCreateTableOne2(
        strata = strata, vars = vars, data = data, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
        showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal,
        catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, labeldata = labeldata, minMax = minMax, showpm = showpm,
        addOverall = addOverall, pairwise = pairwise, pairwise.showtest = pairwise.showtest, n_original = T
      )
    }else{
      ptb1 <- svyCreateTableOne2(
        strata = strata, vars = vars, data = data, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
        showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal,
        catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, labeldata = labeldata, minMax = minMax, showpm = showpm,
        addOverall = addOverall, pairwise = pairwise, pairwise.showtest = pairwise.showtest, n_original = F
      )
    }
    
    
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
    
    if(n_original){
      ptb1.list <- lapply(data.strata, svyCreateTableOne2,
                          vars = vars, strata = strata2, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
                          showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal,
                          catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, minMax = minMax, showpm = showpm, addOverall = F, n_original = T, labeldata = labeldata
      )
    }else{
      ptb1.list <- lapply(data.strata, svyCreateTableOne2,
                          vars = vars, strata = strata2, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
                          showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal,
                          catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, minMax = minMax, showpm = showpm, addOverall = F, n_original = F, labeldata = labeldata
      )
    }
    
    
    if (Labels & !is.null(labeldata)) {
      data.table::setkey(labeldata, variable, level)
      strata_labels <- labeldata[.(strata, names(data.strata)), val_label]
      strata2_labels <- unique(labeldata[variable == strata2, val_label])
      
      ptb1.list <- mapply(function(tbl, strata_label) {
        cols_to_rename <- intersect(colnames(tbl), strata2_labels)
        if (length(cols_to_rename) > 0) {
          colnames(tbl)[colnames(tbl) %in% cols_to_rename] <- paste(strata_label, cols_to_rename, sep = ":")
        }
        tbl
      }, tbl = ptb1.list, strata_label = strata_labels, SIMPLIFY = FALSE)
    }
    
    if (showAllLevels == T) {
      ptb1.cbind <- Reduce(cbind, c(list(ptb1.list[[1]]), lapply(2:length(ptb1.list), function(x) {
        ptb1.list[[x]][, -1]
      })))
    } else {
      ptb1.cbind <- Reduce(cbind, ptb1.list)
    }
    
    # colnum.test = which(colnames(ptb1.cbind) == "test")
    # ptb1.2group = ptb1.cbind[, c(setdiff(1:ncol(ptb1.cbind), colnum.test), colnum.test[1])]
    cap.tb1 <- paste("Stratified by ", strata, " and ", strata2, "- weighted data", sep = "")
    if (Labels & !is.null(labeldata)) {
      colname.group_index <- paste(labeldata[variable == strata, var_label][1], ":", labeldata[variable == strata2, var_label][1], sep = "")
      colnames(ptb1.cbind)[1] <- colname.group_index
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
      
      cap.tb1 <- paste("Stratified by ", labeldata[variable == strata, var_label][1], " and ", labeldata[variable == strata2, var_label][1], "- weighted data", sep = "")
    }
    
    return(list(table = ptb1.cbind, caption = cap.tb1))
  } else {
    
    res2 <- tableone::svyCreateTableOne(vars = vars, strata = c(strata2, strata), data = data, factorVars = factorVars, includeNA = F, test = T, addOverall = addOverall)
    factor_vars <- res2[["MetaData"]][["varFactors"]]
    if (n_original){
      ori <- tableone::CreateTableOne(vars = vars, strata = c(strata2, strata), data = data$variables, factorVars = factorVars, includeNA = F, test = T, addOverall = addOverall)
      res <- ChangeSvyTable(res2,ori)
    }else{
      res <- res2
    }
    
    if (Labels & !is.null(labeldata)) {
      labelled::var_label(data$variables) <- sapply(names(data$variable), function(v) {
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
      cap.tb1 <- paste("Stratified by ", labeldata[variable == strata, var_label][1], " and ", labeldata[variable == strata2, var_label][1], "- weighted data", sep = "")
      # val_label
      # vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[variable == v, val_label]})))
      # ptb1[,1] <- vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
  }
}
