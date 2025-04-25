#' @title count_event_by: funciton to count event, subgroup number inside TableSubgroupCox, TableSubgroupMultiCox
#' @description Function to count event, subgroup number
#' @param formula formula with survival analysis
#' @param data same data as in formula
#' @param count_by_var variables to count subgroup for
#' @param var_subgroup 1 sub-group variable for analysis,
#' @param decimal.percent decimals to show percent of, Default: 1
#' @return Table with event, subgroup number
#' @details This function is used inside TableSubgroupCox, TableSubgroupMultiCox for calculation
#' @examples
#' \dontrun{
#' if (interactive()) {
#'
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{arrange}}
#' @rdname count_event_by
#' @export
#' @importFrom dplyr group_by summarize mutate bind_rows arrange
#' @importFrom rlang sym
#'
count_event_by <- function(formula, data, count_by_var = NULL, var_subgroup = NULL, decimal.percent = 1) {
  if (inherits(data, "survey.design")) {
    data <- data$variables
  } else {
    data <- data
  }
  Count <- Event_Count <- NULL
  event_col <- as.character(formula[[2]][[3]])
  total_count <- nrow(data)
  total_event_count <- sum(data[[event_col]] == 1, na.rm = TRUE)
  total_event_rate <- paste0(total_event_count, "/", total_count, " (", round(total_event_count / total_count * 100, decimal.percent), "%)")
  
  if (!is.null(count_by_var) && !is.null(var_subgroup)) {
    counts <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(var_subgroup))) %>%
      dplyr::group_by(!!rlang::sym(count_by_var), !!rlang::sym(var_subgroup)) %>%
      dplyr::summarize(Count = dplyr::n(), Event_Count = sum(!!rlang::sym(event_col) == 1, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Event_Rate = paste0(Event_Count, "/", Count, " (", round(Event_Count / Count * 100, decimal.percent), "%)"))
    
    overall_counts <- data %>%
      dplyr::group_by(!!rlang::sym(count_by_var)) %>%
      dplyr::summarize(Count = dplyr::n(), Event_Count = sum(!!rlang::sym(event_col) == 1, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        Event_Rate = paste0(Event_Count, "/", Count, " (", round(Event_Count / Count * 100, decimal.percent), "%)"),
        !!rlang::sym(var_subgroup) := "Overall"
      )
    
    counts <- counts %>%
      dplyr::bind_rows(overall_counts) %>%
      dplyr::arrange(!!rlang::sym(count_by_var), !!rlang::sym(var_subgroup))
  } else if (is.null(count_by_var) && !is.null(var_subgroup)) {
    counts <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(var_subgroup))) %>%
      dplyr::group_by(!!rlang::sym(var_subgroup)) %>%
      dplyr::summarize(Count = dplyr::n(), Event_Count = sum(!!rlang::sym(event_col) == 1, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Event_Rate = paste0(Event_Count, "/", Count, " (", round(Event_Count / Count * 100, decimal.percent), "%)"))
  } else if (!is.null(count_by_var) && is.null(var_subgroup)) {
    counts <- data %>%
      dplyr::group_by(!!rlang::sym(count_by_var)) %>%
      dplyr::summarize(Count = dplyr::n(), Event_Count = sum(!!rlang::sym(event_col) == 1, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Event_Rate = paste0(Event_Count, "/", Count, " (", round(Event_Count / Count * 100, decimal.percent), "%)"))
  } else {
    counts <- tibble::tibble(
      Total = "Total",
      Count = total_count,
      Event_Count = total_event_count,
      Event_Rate = total_event_rate
    )
    return(counts)
  }

  total_row <- tibble::tibble(
    Count = total_count,
    Event_Count = total_event_count,
    Event_Rate = total_event_rate
  )
  
  counts <- dplyr::bind_rows(counts, total_row)
  
  return(counts)
}




#' @title collapse_counts
#' @description #' When a grouping variable  count_by has more than two levels, this utility collapses all of the level-specific columns
#' @param df A data.frame produced by one of the TableSubgroup functions
#' @param count_by A string giving the name of the variable whose per-level
#'   count columns should be collapsed. If NULL, df is returned unmodified.
#' @return A data.frame identical to `f except that:
#' All Count(count_by=…) columns are removed.
#' A single column Count(by count_by) remains, containing the first
#'      non-missing, non-empty value from the original per-level columns.
#' @details
#' This helper is intended for internal use by
#' TableSubgroupCox(), TableSubgroupMultiCox(), and their GLM equivalents.
#' It simplifies reporting when you request counts stratified by a factor with
#' more than two levels.
#'
#' @export

collapse_counts <- function(df, count_by) {
  pattern <- paste0("^Count\\(", count_by, "=[^\\)]+\\)$")
  drop_cols <- grep(pattern, names(df), value = TRUE)
  if (length(drop_cols)<3) return(df)
  newcol <- paste0("Count(by ", count_by, ")")
  df[[newcol]] <- NA_character_
  key <- "Levels"
  for (col in drop_cols) {
    lvl <- sub(paste0("^Count\\(", count_by, "=(.+)\\)$"), "\\1", col)
    idx_val <- which(!is.na(df[[col]]))
    rows_lvl <- which(df[[key]] == paste0(count_by, "=", lvl))
    for (j in seq_along(rows_lvl)) {
      i <- rows_lvl[j]
      if (j <= length(idx_val)) df[[newcol]][i] <- as.character(df[[col]][idx_val[j]])
    }
  }
  df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
  cols <- names(df)
  pos <- match("Count", cols)
  if (!is.na(pos)) {
    cols <- append(cols[cols != newcol], newcol, after = pos)
    df <- df[, cols, drop = FALSE]
  }
  df
}




#' @title count_event_by_glm: function to count event rates or summary metrics for GLM analysis
#' @description Count event and subgroup summary, supports automatic parsing of fixed-effect syntax `(1|subgroup)` and multiple families.
#' @param formula formula with response (0/1 or count or continuous) and optional fixed-effect `(1|subgroup)` term.
#' @param data data.frame or survey.design
#' @param count_by_var variable name to stratify by (string), default NULL
#' @param var_subgroup subgroup variable name (string); parsed from formula if not provided, default NULL
#' @param decimal.percent decimals for percent or mean/sd, default 1
#' @param family family type: "gaussian", "binomial", "poisson", or "quasipoisson"
#' @return tibble with grouping columns and Metric column (rate or mean sd)
#' @importFrom dplyr group_by summarise filter
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
count_event_by_glm <- function(
    formula,
    data,
    count_by_var    = NULL,
    var_subgroup    = NULL,
    decimal.percent = 1,
    family          = "binomial"
) {
  # raw data
  df_raw <- if (inherits(data, "survey.design")) data$variables else data
  
  # determine needed variables (includes random-effect groups)
  required_vars <- all.vars(formula)
  if (!is.null(count_by_var))   required_vars <- c(required_vars, count_by_var)
  if (!is.null(var_subgroup))   required_vars <- c(required_vars, var_subgroup)
  
  # subset to complete cases
  df <- df_raw[stats::complete.cases(df_raw[, required_vars, drop = FALSE]), , drop = FALSE]
  
  # response name
  response_col <- as.character(formula[[2]])
  
  # metric function by family
  if (family == "binomial") {
    metric_fn <- function(x) {
      n <- length(x)
      e <- sum(x == 1, na.rm = TRUE)
      paste0(e, "/", n, " (", round(e / n * 100, decimal.percent), "%)")
    }
  } else if (family == "gaussian") {
    metric_fn <- function(x) {
      n <- length(x)
      m <- mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      paste0(n, " (", round(m, decimal.percent), " +- ", round(s, decimal.percent), ")")
    }
  } else if (family %in% c("poisson", "quasipoisson")) {
    metric_fn <- function(x) {
      n <- length(x)
      m <- mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      paste0(n, " (", round(m, decimal.percent), " +- ", round(s, decimal.percent), ")")
    }
  } else {
    stop("Unsupported family: ", family)
  }
  
  # no stratification
  if (is.null(count_by_var) && is.null(var_subgroup)) {
    return(tibble::tibble(
      Total  = "Total",
      Count  = nrow(df),
      Metric = metric_fn(df[[response_col]])
    ))
  }
  
  # both count_by and subgroup
  if (!is.null(count_by_var) && !is.null(var_subgroup)) {
    # subgroup breakdown
    res <- df %>%
      dplyr::filter(!is.na(.data[[var_subgroup]])) %>%
      dplyr::group_by(.data[[count_by_var]], .data[[var_subgroup]]) %>%
      dplyr::summarise(
        Count  = dplyr::n(),
        Metric = metric_fn(.data[[response_col]]),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        !!count_by_var := as.character(.data[[count_by_var]]),
        !!var_subgroup := as.character(.data[[var_subgroup]])
      )
    # overall per count_by
    overall <- df %>%
      dplyr::group_by(.data[[count_by_var]]) %>%
      dplyr::summarise(
        Count  = dplyr::n(),
        Metric = metric_fn(.data[[response_col]]),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        !!count_by_var := as.character(.data[[count_by_var]]),
        !!var_subgroup := "Overall"
      )
    # total across all
    total <- tibble::tibble(
      !!count_by_var := NA_character_,
      !!var_subgroup := NA_character_,
      Count  = nrow(df),
      Metric = metric_fn(df[[response_col]])
    )
    return(dplyr::bind_rows(res, overall, total))
  }
  
  # only subgroup
  if (is.null(count_by_var) && !is.null(var_subgroup)) {
    res <- df %>%
      dplyr::filter(!is.na(.data[[var_subgroup]])) %>%
      dplyr::group_by(.data[[var_subgroup]]) %>%
      dplyr::summarise(
        Count  = dplyr::n(),
        Metric = metric_fn(.data[[response_col]]),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        !!var_subgroup := as.character(.data[[var_subgroup]])
      )
    # append total row
    total <- tibble::tibble(
      !!var_subgroup := "Total",
      Count  = nrow(df),
      Metric = metric_fn(df[[response_col]])
    )
    return(dplyr::bind_rows(res, total))
  }
  
  # only count_by
  if (!is.null(count_by_var) && is.null(var_subgroup)) {
    res <- df %>%
      dplyr::group_by(.data[[count_by_var]]) %>%
      dplyr::summarise(
        Count  = dplyr::n(),
        Metric = metric_fn(.data[[response_col]]),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        !!count_by_var := as.character(.data[[count_by_var]])
      )
    # append total
    total <- tibble::tibble(
      !!count_by_var := "Total",
      Count  = nrow(df),
      Metric = metric_fn(df[[response_col]])
    )
    return(dplyr::bind_rows(res, total))
  }
}






