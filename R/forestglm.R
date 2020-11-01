#' @title TableSubgroupGLM: Sub-group analysis table for GLM.
#' @description Sub-group analysis table for GLM.
#' @param formula formula with survival analysis.
#' @param var_subgroup 1 sub-group variable for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param family family, "gaussian" or "binomial"
#' @param decimal.estimate Decimal for estimate, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @return Sub-group analysis table. 
#' @details This result is used to make forestplot.
#' @examples
#' library(survival);library(dplyr)
#' lung %>% 
#'   mutate(status = as.integer(status == 1),
#'          sex = factor(sex),
#'          kk = factor(as.integer(pat.karno >= 70))) -> lung
#' TableSubgroupGLM(status ~ sex, data = lung, family = "binomial")
#' TableSubgroupGLM(status ~ sex, var_subgroup = "kk", data = lung, family = "binomial")
#' 
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupGLM(status ~ sex, data = data.design, family = "binomial")
#' TableSubgroupGLM(status ~ sex, var_subgroup = "kk", data = data.design, family = "binomial")
#' @seealso 
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[stats]{glm}}
#'  \code{\link[survey]{svyglm}}
#'  \code{\link[stats]{confint}}
#' @rdname TableSubgroupGLM
#' @export 
#' @importFrom purrr possibly map_dbl map map2
#' @importFrom dplyr group_split select filter mutate bind_cols
#' @importFrom magrittr %>%
#' @importFrom survey svyglm
#' @importFrom stats glm confint coefficients anova gaussian quasibinomial
#' @importFrom utils tail

TableSubgroupGLM <- function(formula, var_subgroup = NULL, var_cov = NULL, data, family = "binomial", decimal.estimate = 2, decimal.percent = 1, decimal.pvalue = 3){
  
  . <- NULL
  
  if (length(formula[[3]]) > 1) stop("Formula must contains only 1 independent variable")
  if (any(class(data) == "survey.design" & !is.null(var_subgroup))){
    if (is.numeric(data$variables[[var_subgroup]])) stop("var_subgroup must categorical.")
    if (length(levels(data$variables[[as.character(formula[[3]])]])) != 2) stop("Independent variable must have 2 levels.")
  } else if(any(class(data) == "data.frame" & !is.null(var_subgroup))){
    if (is.numeric(data[[var_subgroup]])) stop("var_subgroup must categorical.")
    if (length(levels(data[[as.character(formula[[3]])]])) != 2) stop("Independent variable must have 2 levels.")
  }
  
  
  ## functions with error
  possible_table <- purrr::possibly(table, NA)
  possible_prop.table <- purrr::possibly(function(x){prop.table(x, 1)[2, ] * 100}, NA) 
  possible_pv <- purrr::possibly(function(x){summary(x)[["coefficients"]][1, ] %>% tail(1)}, NA)
  possible_glm <- purrr::possibly(stats::glm, NA)
  possible_svyglm <- purrr::possibly(survey::svyglm, NA)
  possible_confint <- purrr::possibly(stats::confint, NA)
  possible_modely <- purrr::possibly(function(x){purrr::map_dbl(x, .[["y"]], 1)}, NA)
  possible_rowone <- purrr::possibly(function(x){x[-1, ]}, NA)
  

  var_cov <- setdiff(var_cov, c(as.character(formula[[3]]), var_subgroup))
  family.svyglm <- gaussian()
  if (family  == "binomial") family.svyglm <- quasibinomial()
  
  if (is.null(var_subgroup)){
    if (!is.null(var_cov)){
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }
    
    if (any(class(data) == "survey.design")){
      model <- survey::svyglm(formula, design = data, x= T, family = family.svyglm)
      if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
    } else{
      model <- stats::glm(formula, data = data, x= TRUE, family = family)
      if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
    }
    
    
    Point.Estimate <- round(coef(model), decimal.estimate)[-1]
    CI <- round(confint(model)[-1, ], decimal.estimate)
    if(family == "binomial"){
      Point.Estimate <- round(exp(coef(model)), decimal.estimate)[-1]
      CI <- round(exp(confint(model)[-1, ]), decimal.estimate)
    }
    
    
    
    #if (length(Point.Estimate) > 1){
    #  stop("Formula must contain 1 independent variable only.")
    #}
    
    
    
    #event <- model$y
    #prop <- round(prop.table(table(event, model$x[, 1]), 2)[2, ] * 100, decimal.percent)
    pv <- round(tail(summary(model)$coefficients[-1, ], 1), decimal.pvalue)
    
    data.frame(Variable = "Overall", Count = length(model$y), Percent = 100, `Point Estimate` = Point.Estimate, Lower = CI[1], Upper = CI[2]) %>% 
    dplyr::mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA) -> out
    
    if (family == "binomial"){
      names(out)[4] <- "OR"
    }
    
    return(out)
  } else if (length(var_subgroup) > 1 | any(grepl(var_subgroup, formula))){
    stop("Please input correct subgroup variable.")
  } else{
    if (!is.null(var_cov)){
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }
    if (any(class(data) == "survey.design")){
      data$variables[[var_subgroup]] %>% table %>% names -> label_val
      label_val %>% purrr::map(~possible_svyglm(formula, design = subset(data, get(var_subgroup) == .), x = TRUE, family = family.svyglm)) -> model
      xlev <- survey::svyglm(formula, design = data)$xlevels
      xlabel <- names(attr(model[[which(!is.na(model))[1]]]$x, "contrast"))[1]
      pvs_int <- possible_svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep=""), deparse(formula))), design = data, family = family.svyglm) %>% summary %>% coefficients
      pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
      if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      
      
      if (length(label_val) > 2){
        model.int <- survey::svyglm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep=""), deparse(formula))), design = data, family = family.svyglm)
        pv_anova <- anova(model.int, method = "Wald")
        pv_int <- pv_anova[[length(pv_anova)]][[7]]
      }
      Count <- as.vector(table(data$variables[[var_subgroup]]))
      
    } else{
      data %>% filter(!is.na(get(var_subgroup))) %>% group_split(get(var_subgroup)) %>% purrr::map(~possible_glm(formula, data = ., x= T, family = family)) -> model
      data %>% filter(!is.na(get(var_subgroup))) %>% select(var_subgroup) %>% table %>% names -> label_val
      xlev <- stats::glm(formula, data = data)$xlevels
      xlabel <- names(attr(model[[which(!is.na(model))[1]]]$x, "contrast"))[1]
      model.int <- possible_glm(as.formula(gsub(xlabel, paste(xlabel, "*", var_subgroup, sep=""), deparse(formula))), data = data, family = family)  
      pvs_int <- model.int %>% summary %>% coefficients
      pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
      if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      
      if (length(label_val) > 2){
        pv_anova <- anova(model.int, test = "Chisq")
        pv_int <- round(pv_anova[nrow(pv_anova), 5], decimal.pvalue)
      }
      
      Count <- as.vector(table(data[[var_subgroup]]))
    }
    
    
    
    Estimate <- model %>% purrr::map("coefficients", .default = NA) %>% purrr::map_dbl(2, .default = NA)
    CI0 <- model %>% purrr::map(possible_confint) %>% purrr::map(possible_rowone) %>% Reduce(rbind, .)
    Point.Estimate <- round(Estimate, decimal.estimate)
    CI <- round(CI0, decimal.estimate)
    if (family == "binomial"){
      Point.Estimate <- round(exp(Estimate), decimal.estimate)
      CI <- round(exp(CI0), decimal.estimate)
    }
    
    model %>% purrr::map(possible_pv) %>% purrr::map_dbl(~round(., decimal.pvalue)) -> pv
    
    
    data.frame(Variable = paste("  ", label_val) , Count = Count, Percent = round(Count/sum(Count) * 100, decimal.percent), "Point Estimate" = Point.Estimate, Lower = CI[, 1], Upper = CI[, 2]) %>%
      dplyr::mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA) -> out
    if (family == "binomial"){
      names(out)[4] <- "OR"
    }
    
    return(rbind(c(var_subgroup, rep(NA, ncol(out) - 2), ifelse(pv_int >= 0.001, pv_int, "<0.001")), out))
  }
}



#' @title TableSubgroupMultiGLM: Multiple sub-group analysis table for GLM.
#' @description Multiple sub-group analysis table for GLM.
#' @param formula formula with survival analysis.
#' @param var_subgroups Multiple sub-group variables for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param family family, "gaussian" or "binomial"
#' @param decimal.estimate Decimal for estimate, Default: 2
#' @param decimal.percent Decimal for percent, Default: 1
#' @param decimal.pvalue Decimal for pvalue, Default: 3
#' @param line Include new-line between sub-group variables, Default: F
#' @return Multiple sub-group analysis table. 
#' @details This result is used to make forestplot.
#' @examples 
#' library(survival);library(dplyr)
#' lung %>% 
#'   mutate(status = as.integer(status == 1),
#'          sex = factor(sex),
#'          kk = factor(as.integer(pat.karno >= 70)),
#'          kk1 = factor(as.integer(pat.karno >= 60))) -> lung
#' TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), 
#'                       data=lung, line = TRUE, family = "binomial")
#' 
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupMultiGLM(status ~ sex, var_subgroups = c("kk", "kk1"), 
#'                       data = data.design, family = "binomial")
#' @seealso 
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}}
#' @rdname TableSubgroupMultiGLM
#' @export 
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows


TableSubgroupMultiGLM <- function(formula, var_subgroups = NULL, var_cov = NULL, data, family = "binomial", decimal.estimate = 2, decimal.percent = 1, decimal.pvalue = 3, line = F){
  
  . <- NULL
  out.all <- TableSubgroupGLM(formula, var_subgroup = NULL, var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue)
  
  if (is.null(var_subgroups)){
    return(out.all)
  } else {
    out.list <- purrr::map(var_subgroups, ~TableSubgroupGLM(formula, var_subgroup = ., var_cov = var_cov, data = data, family = family, decimal.estimate = decimal.estimate, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue))
    if (line){
      out.newline <- out.list %>% purrr::map(~rbind(NA, .))
      return(rbind(out.all, out.newline %>% dplyr::bind_rows()))
    } else{
      return(rbind(out.all, out.list %>% dplyr::bind_rows()))
    }
  } 
}



