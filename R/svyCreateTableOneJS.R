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
#' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via write.csv.
#' @details DETAILS
#' @examples 
#'  library(survey);data(nhanes)
#'  nhanes$SDMVPSU <- as.factor(nhanes$SDMVPSU)
#'  nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR, 
#'                         nest = TRUE, data = nhanes)
#'  svyCreateTableOne2(vars = c("HI_CHOL","race","agecat","RIAGENDR"), 
#'                     strata = "RIAGENDR", data = nhanesSvy)
#' @rdname svyCreateTableOne2
#' @importFrom data.table data.table :=
#' @importFrom tableone svyCreateTableOne 
#' @importFrom labelled var_label var_label<-
#' @export 

svyCreateTableOne2 <- function(data, strata, vars, factorVars, includeNA = F, test = T,
                           showAllLevels = T, printToggle = F, quote = F, smd = F, nonnormal = NULL, 
                           catDigits = 1, contDigits = 2, pDigits = 3, Labels = F, labeldata = NULL){
  

  if (Labels & !is.null(labeldata)){
    labelled::var_label(data$variables) = sapply(names(data$variables), function(v){as.character(labeldata[get("variable") == v, "var_label"][1])}, simplify = F)
    vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
  }
  
  if (length(strata) != 1){
    stop("Please select only 1 strata")
  }

  
  res <- tableone::svyCreateTableOne(vars =vars, strata = strata, data = data, factorVars = factorVars, includeNA = includeNA, test = test, 
                                  smd = smd)
  
  
  ptb1 <- print(res,
                showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, smd = smd, varLabels = Labels, nonnormal = nonnormal,
                catDigits = catDigits, contDigits = contDigits, pDigits = pDigits)
  rownames(ptb1) = gsub("(mean (SD))", "", rownames(ptb1), fixed=T)
  #cap.tb1 = paste("Table 1: Stratified by ", strata, sep="")
  
  if (Labels & !is.null(labeldata)){
    colname.group_var = unlist(labeldata[get("variable") == strata, "val_label"])
    colnames(ptb1)[1:(length(colname.group_var)+1)] = unlist(c(labeldata[get("variable") == strata, "var_label"][1], colname.group_var))
    ptb1[,1] = vals.tb1
    #cap.tb1 = paste("Table 1: Stratified by ", labeldata[variable == strata, "var_label"][1], sep="")
    
  }
  
  sig <- ifelse(ptb1[,"p"] == "<0.001", "0", ptb1[,"p"])
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
#' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via write.csv.
#' @details DETAILS
#' @examples 
#'  library(survey);data(nhanes)
#'  nhanes$SDMVPSU <- as.factor(nhanes$SDMVPSU)
#'  nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR, 
#'                         nest = TRUE, data = nhanes)
#'  svyCreateTableOneJS(vars = c("HI_CHOL","race","agecat","RIAGENDR"), 
#'                      strata = "RIAGENDR", data = nhanesSvy)
#' @rdname svyCreateTableOneJS
#' @importFrom data.table data.table := CJ
#' @importFrom tableone svyCreateTableOne 
#' @importFrom labelled var_label var_label<-
#' @export 


svyCreateTableOneJS <- function(vars, strata = NULL, strata2 = NULL, data, factorVars = NULL, includeNA = F, test = T,
                            showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = F, nonnormal = NULL, 
                            catDigits = 1, contDigits = 2, pDigits = 3, labeldata = NULL, psub = T){
  
  
  variable <- val_label <- V1 <- V2 <- NULL
  
  #if (Labels & !is.null(labeldata)){
  #  var_label(data) = sapply(names(data), function(v){as.character(labeldata[get("variable") == v, "var_label"][1])}, simplify = F)
  #  vals.tb1 = c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
  #}
  data <- data
  
  if (is.null(strata)){
    if (Labels & !is.null(labeldata)){
      labelled::var_label(data$variables) <- sapply(names(data$variables), function(v){as.character(labeldata[get("variable") == v, "var_label"][1])}, simplify = F)
      vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[get("variable") == v, "val_label"]})))
    }
    
    res <- tableone::svyCreateTableOne(vars =vars, data = data, factorVars = factorVars, includeNA = includeNA)
    
    ptb1 <- print(res,
                  showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, varLabels = Labels, nonnormal = nonnormal,
                  catDigits = catDigits, contDigits = contDigits)
    
    rownames(ptb1) <- gsub("(mean (SD))", "", rownames(ptb1), fixed=T)
    cap.tb1 <- "Table 1: Total - weighted data"
    if (Labels & !is.null(labeldata)){
      ptb1[,1] <- vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
  } else if (is.null(strata2)){
    ptb1 <- svyCreateTableOne2(strata = strata, vars =vars, data = data, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
                            showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal,
                            catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, labeldata = labeldata)
    
    cap.tb1 <- paste("Table 1: Stratified by ", strata, "- weighted data", sep="")
    
    if (Labels & !is.null(labeldata)){
      cap.tb1 <- paste("Table 1: Stratified by ", labeldata[get("variable") == strata, "var_label"][1], "- weighted data", sep="")
      #ptb1[,1] = vals.tb1
      
    }
    return(list(table = ptb1, caption = cap.tb1))
  } else if (psub ==T){
    data.strata <-  lapply(levels(data$variable[[strata]]), function(x){subset(data, get(strata) == x)})
    ptb1.list <- lapply(data.strata, svyCreateTableOne2,
                        vars =vars, strata = strata2, factorVars = factorVars, includeNA = includeNA, test = test, smd = smd,
                        showAllLevels = showAllLevels, printToggle = printToggle, quote = quote, Labels = Labels, nonnormal = nonnormal, 
                        catDigits = catDigits, contDigits = contDigits, pDigits = pDigits, labeldata = labeldata
    )
    ptb1.cbind = Reduce(cbind, c(list(ptb1.list[[1]]), lapply(2:length(ptb1.list), function(x){ptb1.list[[x]][,-1]})))
    #colnum.test = which(colnames(ptb1.cbind) == "test")
    #ptb1.2group = ptb1.cbind[, c(setdiff(1:ncol(ptb1.cbind), colnum.test), colnum.test[1])]
    cap.tb1 <- paste("Table 1: Stratified by ", strata, "(", paste(levels(data[[strata]]), collapse=", "), ") & ", strata2, "- weighted data", sep="")
    if (Labels & !is.null(labeldata)){
      cap.tb1 <- paste("Table 1: Stratified by ", labeldata[get("variable") == strata, "var_label"][1], "(", paste(unlist(labeldata[get("variable") == strata, "val_label"]), collapse=", "), ") & ", labeldata[get("variable") == strata2, "var_label"][1], "- weighted data", sep="")
    }
    
    return(list(table = ptb1.cbind, caption = cap.tb1))
  } else{
    res <- tableone::svyCreateTableOne(vars = vars, strata = c(strata2, strata), data = data, factorVars = factorVars, includeNA = F, test = T) 
    
    
    ptb1 <- print(res, 
                  showAllLevels=T,
                  printToggle=F, quote=F, smd = smd, varLabels = T,  nonnormal = nonnormal,
                  catDigits = catDigits, contDigits = contDigits, pDigits = pDigits)
    
    rownames(ptb1) <- gsub("(mean (SD))", "", rownames(ptb1), fixed=T)
    sig <- ifelse(ptb1[,"p"] == "<0.001", "0", ptb1[,"p"])
    sig <- as.numeric(as.vector(sig))
    sig <- ifelse(sig <= 0.05, "**", "")
    ptb1 <- cbind(ptb1, sig)
    cap.tb1 <- paste("Table 1: Stratified by ", strata, " and ",strata2, "- weighted data",  sep="")
    
    # Column name
    if (Labels & !is.null(labeldata)){
      val_combination <- data.table::CJ(labeldata[variable == strata, val_label], labeldata[variable == strata2, val_label])
      colname.group_var <- val_combination[, paste(V1, ":", V2, sep="")] 
      colname.group_index <- paste(labeldata[variable == strata, var_label][1], ":", labeldata[variable == strata2, var_label][1], sep = "")
      colnames(ptb1)[1:(length(colname.group_var)+1)] = c(colname.group_index, colname.group_var)
      # caption
      cap.tb1 = paste("Table 1: Stratified by ", labeldata[variable == strata, var_label][1], " and ", labeldata[variable == strata2, var_label][1], "- weighted data",  sep="")
      # val_label
      vals.tb1 <- c(NA, unlist(sapply(vars, function(v){labeldata[variable == v, val_label]})))
      ptb1[,1] <- vals.tb1
    }
    return(list(table = ptb1, caption = cap.tb1))
    
  }
  
} 
