#' @title TableSubgroupCox: Sub-group analysis table for Cox/svycox model.
#' @description Sub-group analysis table for Cox/svycox model.
#' @param formula formula with survival analysis.
#' @param var_subgroup 1 sub-group variable for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param time_eventrate Time for kaplan-meier based event rate calculation, Default = 365 * 3
#' @param decimal.hr Decimal for hazard ratio, Default: 2
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
#' TableSubgroupCox(Surv(time, status) ~ sex, data = lung, time_eventrate = 100)
#' TableSubgroupCox(Surv(time, status) ~ sex, var_subgroup = "kk", data = lung, 
#'                  time_eventrate = 100)
#' 
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupCox(Surv(time, status) ~ sex, data = data.design, time_eventrate = 100)
#' TableSubgroupCox(Surv(time, status) ~ sex, var_subgroup = "kk", data = data.design,
#'                  time_eventrate = 100)
#' @seealso 
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[survival]{coxph}}
#'  \code{\link[survey]{svycoxph}}
#'  \code{\link[stats]{confint}}
#' @rdname TableSubgroupCox
#' @export 
#' @importFrom purrr possibly map_dbl map map2
#' @importFrom dplyr select filter mutate bind_cols
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom survival coxph
#' @importFrom survey svycoxph regTermTest
#' @importFrom stats confint coefficients
#' @importFrom utils tail

TableSubgroupCox <- function(formula, var_subgroup = NULL, var_cov = NULL, data, time_eventrate = 3 * 365, decimal.hr = 2, decimal.percent = 1, decimal.pvalue = 3){
  
  . <- NULL
  
  if (any(class(data) == "survey.design" & !is.null(var_subgroup))){
    if (is.numeric(data$variables[[var_subgroup]])) stop("var_subgroup must categorical.")
    #if (length(levels(data$variables[[as.character(formula[[3]])]])) != 2) stop("Independent variable must have 2 levels.")
  } else if(any(class(data) == "data.frame" & !is.null(var_subgroup))){
    if (is.numeric(data[[var_subgroup]])) stop("var_subgroup must categorical.")
    #if (length(levels(data[[as.character(formula[[3]])]])) != 2) stop("Independent variable must have 2 levels.")
  }
  
  
  ## functions with error
  possible_table <- purrr::possibly(table, NA)
  possible_prop.table <- purrr::possibly(function(x){prop.table(x, 1)[2, ] * 100}, NA) 
  possible_pv <- purrr::possibly(function(x){summary(x)[["coefficients"]][1, ] %>% tail(1)}, NA)
  possible_coxph <- purrr::possibly(survival::coxph, NA)
  possible_svycoxph <- purrr::possibly(survey::svycoxph, NA)
  possible_confint <- purrr::possibly(stats::confint, NA)
  possible_modely <- purrr::possibly(function(x){purrr::map_dbl(x, .[["y"]], 1)}, NA)
  possible_rowone <- purrr::possibly(function(x){x[1, ]}, NA)
  
  formula.km <- formula
  var_cov <- setdiff(var_cov, c(as.character(formula[[3]]), var_subgroup))
  xlabel <- setdiff(as.character(formula)[[3]], "+")[1]
  
  ncoef <- ifelse(any(class(data) == "survey.design"), ifelse(length(levels(data$variables[[xlabel]])) <= 2, 1,  length(levels(data$variables[[xlabel]])) - 1),
    ifelse(length(levels(data[[xlabel]])) <= 2, 1,  length(levels(data[[xlabel]])) - 1))
  
  if (is.null(var_subgroup)){
    if (!is.null(var_cov)){
      formula <- as.formula(paste0(deparse(formula), " + ", paste(var_cov, collapse = "+")))
    }
    if (any(class(data) == "survey.design")){
      model <- survey::svycoxph(formula, design = data, x= T)
      #if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      res.kap <- survey::svykm(formula.km, design = data)
      prop <- round(100 * sapply(res.kap, function(x){1 - x[["surv"]][which.min(abs(x[["time"]] - time_eventrate))]}), decimal.percent)
      names(prop) <- model$xlevels[[1]]
    } else{
      model <- survival::coxph(formula, data = data, x= TRUE)
      #if (!is.null(model$xlevels) & length(model$xlevels[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      res.kap <- survival::survfit(formula.km, data = data)
      res.kap.times <- summary(res.kap, times = time_eventrate, extend = T)
      prop <- round(100 * (1 - res.kap.times[["surv"]]), decimal.percent)
      names(prop) <- model$xlevels[[1]]
      #out.kap <- paste(res.kap.times[["n.event"]], " (", round(100 * (1 - res.kap.times[["surv"]]), decimal.percent), ")", sep = "")
    }
    
    
    
    Point.Estimate <- round(exp(coef(model)), decimal.hr)[1:ncoef]
    
    #if (length(Point.Estimate) > 1){
    #  stop("Formula must contain 1 independent variable only.")
    #}
    
    
    CI <- round(exp(confint(model)[1:ncoef, ]), decimal.hr)
    event <- purrr::map_dbl(model$y, 1) %>% tail(model$n)
    #prop <- round(prop.table(table(event, model$x[, 1]), 2)[2, ] * 100, decimal.percent)
    pv <- round(summary(model)$coefficients[1:ncoef, "Pr(>|z|)"], decimal.pvalue)
    
    if (ncoef <= 2){
      out <- data.frame(Variable = "Overall", Count = model$n, Percent = 100, `Point Estimate` = Point.Estimate, Lower = CI[1], Upper = CI[2], check.names = F) %>% 
        mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA)
      
      if (!is.null(names(prop))){
        out <- data.frame(Variable = "Overall", Count = model$n, Percent = 100, `Point Estimate` = Point.Estimate, Lower = CI[1], Upper = CI[2], check.names = F) %>% 
          cbind(t(prop)) %>% 
          mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA) 
      }     
    } else{
      out <- data.frame(Variable = c("Overall", rep("", length(Point.Estimate))), Subgroup = rep("", length(Point.Estimate) + 1), Levels =  levels(data[[xlabel]]), `Point Estimate` = c("Reference", Point.Estimate), Lower = c("", CI[, 1]), Upper = c("", CI[, 2]), check.names = F) %>% 
        mutate(`P value` = c("", ifelse(pv >= 0.001, pv, "<0.001")), `P for interaction` = NA)
      rownames(out) <- NULL
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
      label_val %>% purrr::map(~possible_svycoxph(formula, design = subset(data, get(var_subgroup) == .), x = TRUE)) -> model
      xlev <- survey::svycoxph(formula, design = data)$xlevels
      pvs_int <- possible_svycoxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), design = data) %>% summary %>% coefficients
      pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
      #if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
      model.int <- survey::svycoxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), design = data)
      
      if (sum(grepl(":", names(coef(model.int)))) > 1){
        model.int$call$formula <- as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula)))
        pv_anova <- survey::regTermTest(model.int, as.formula(paste0("~", xlabel, ":", var_subgroup)))
        pv_int <- round(pv_anova$p[1], decimal.pvalue)
      }
       
      if (!is.numeric(data$variables[[xlabel]])){
        res.kap <- purrr::map(label_val, ~survey::svykm(formula.km, design = subset(data, get(var_subgroup) == . )))
        mkz <- function(reskap){
          round(100 * sapply(reskap, function(x){1 - x[["surv"]][which.min(abs(x[["time"]] - time_eventrate))]}), decimal.percent)
        } 
        prop <- purrr::map(res.kap, mkz)  %>% dplyr::bind_cols() %>% t
        #prop <- purrr::map(res.kap, ~round(100 * sapply(., function(x){1 - x[["surv"]][which.min(abs(x[["time"]] - time_eventrate))]}), decimal.percent))
        colnames(prop) <- xlev[[1]]
      } else{
        prop <- NULL
      }
      
      
    } else{
      data %>% filter(!is.na(get(var_subgroup))) %>% split(.[[var_subgroup]]) %>% purrr::map(~possible_coxph(formula, data = ., x= T)) -> model
      data %>% filter(!is.na(get(var_subgroup))) %>% select(var_subgroup) %>% table %>% names -> label_val
      xlev <- survival::coxph(formula, data = data)$xlevels
      model.int <- possible_coxph(as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula))), data = data)  
      if (sum(grepl(":", names(coef(model.int)))) == 1){
        pvs_int <- model.int %>% summary %>% coefficients
        pv_int <- round(pvs_int[nrow(pvs_int), ncol(pvs_int)], decimal.pvalue)
        #if (!is.null(xlev) & length(xlev[[1]]) != 2) stop("Categorical independent variable must have 2 levels.")
        if (!is.numeric(data[[xlabel]])){
          res.kap.times <- data %>% filter(!is.na(get(var_subgroup))) %>% split(.[[var_subgroup]]) %>% purrr::map(~survival::survfit(formula.km, data = .)) %>% purrr::map(~summary(., times = time_eventrate, extend = T))
          prop <- res.kap.times %>% purrr::map(~round(100 * (1 - .[["surv"]]), decimal.percent)) %>% dplyr::bind_cols() %>% t
          colnames(prop) <- xlev[[1]]
        } else{
          prop <- NULL
        }
      } else{
        model.int$call$formula <- as.formula(gsub(xlabel, paste0(xlabel, "*", var_subgroup), deparse(formula)))
        model.int$call$data <- as.name("data")
        pv_anova <- anova(model.int)
        pv_int <- round(pv_anova[nrow(pv_anova), 4], decimal.pvalue)
        prop <- NULL
      }
 
      
    }
    
    model %>% purrr::map_dbl("n", .default = NA) -> Count
    model %>% purrr::map("coefficients", .default = NA) %>% lapply(function(x){round(exp(x[1:ncoef]), decimal.hr)}) %>% unlist -> Point.Estimate
    #model %>% purrr::map("coefficients", .default = NA) %>% purrr::map_dbl(1) %>% exp %>% round(decimal.hr) -> Point.Estimate
    model %>% purrr::map(possible_confint)  %>% Reduce(rbind, .) %>% exp %>% round(decimal.hr) -> CI
    #model %>% purrr::map(possible_confint) %>% purrr::map(possible_rowone) %>% Reduce(rbind, .) %>% exp %>% round(decimal.hr) -> CI
    #model %>% purrr::map("y") %>% purrr::map(~purrr::map_dbl(., 1)) %>% purrr::map(~tail(., length(.)/2)) -> event
    #purrr::map2(event, model, ~possible_table(.x, .y[["x"]][, 1])) %>% purrr::map(possible_prop.table) %>% purrr::map(~round(., decimal.percent)) %>% Reduce(rbind, .) -> prop
    model %>% purrr::map(possible_pv) %>% purrr::map_dbl(~round(., decimal.pvalue)) -> pv
    
    out <- data.frame(Variable = paste("  ", label_val) , Count = Count, Percent = round(Count/sum(Count) * 100, decimal.percent), `Point Estimate` = Point.Estimate, Lower = CI[, 1], Upper = CI[, 2], check.names = F) %>%
      mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA)
    
    if (!is.null(prop)){
      out <- data.frame(Variable = paste("  ", label_val) , Count = Count, Percent = round(Count/sum(Count) * 100, decimal.percent), `Point Estimate` = Point.Estimate, Lower = CI[, 1], Upper = CI[, 2], check.names = F) %>%
        cbind(prop) %>% 
        mutate(`P value` = ifelse(pv >= 0.001, pv, "<0.001"), `P for interaction` = NA)
    }  
    
    
    return(rbind(c(var_subgroup, rep(NA, ncol(out) - 2), ifelse(pv_int >= 0.001, pv_int, "<0.001")), out))
  }
}




#' @title TableSubgroupMultiCox: Multiple sub-group analysis table for Cox/svycox model.
#' @description Multiple sub-group analysis table for Cox/svycox model.
#' @param formula formula with survival analysis.
#' @param var_subgroups Multiple sub-group variables for analysis, Default: NULL
#' @param var_cov Variables for additional adjust, Default: NULL
#' @param data Data or svydesign in survey package.
#' @param time_eventrate Time for kaplan-meier based event rate calculation, Default = 365 * 3
#' @param decimal.hr Decimal for hazard ratio, Default: 2
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
#' TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), 
#'                       data=lung, time_eventrate = 100, line = TRUE)
#' 
#' ## survey design
#' library(survey)
#' data.design <- svydesign(id = ~1, data = lung)
#' TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), 
#'                       data = data.design, time_eventrate = 100)
#' @seealso 
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}}
#' @rdname TableSubgroupMultiCox
#' @export 
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows


TableSubgroupMultiCox <- function(formula, var_subgroups = NULL, var_cov = NULL, data, time_eventrate = 3 * 365, decimal.hr = 2, decimal.percent = 1, decimal.pvalue = 3, line = F){
  
  . <- NULL
  out.all <- TableSubgroupCox(formula, var_subgroup = NULL, var_cov = var_cov, data = data, time_eventrate = time_eventrate, decimal.hr = decimal.hr, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue)
  
  if (is.null(var_subgroups)){
    return(out.all)
  } else {
    out.list <- purrr::map(var_subgroups, ~TableSubgroupCox(formula, var_subgroup = ., var_cov = var_cov,  data = data, time_eventrate = time_eventrate, decimal.hr = decimal.hr, decimal.percent = decimal.percent, decimal.pvalue = decimal.pvalue))
    if (line){
      out.newline <- out.list %>% purrr::map(~rbind(NA, .))
      return(rbind(out.all, out.newline %>% dplyr::bind_rows()))
    } else{
      return(rbind(out.all, out.list %>% dplyr::bind_rows()))
      }
    } 
  }



