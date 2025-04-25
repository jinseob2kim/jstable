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
    # count_by_var와 var_subgroup이 모두 있을 때
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
    # var_subgroup만 있을 때
    counts <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(var_subgroup))) %>%
      dplyr::group_by(!!rlang::sym(var_subgroup)) %>%
      dplyr::summarize(Count = dplyr::n(), Event_Count = sum(!!rlang::sym(event_col) == 1, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Event_Rate = paste0(Event_Count, "/", Count, " (", round(Event_Count / Count * 100, decimal.percent), "%)"))
  } else if (!is.null(count_by_var) && is.null(var_subgroup)) {
    # count_by_var만 있을 때
    counts <- data %>%
      dplyr::group_by(!!rlang::sym(count_by_var)) %>%
      dplyr::summarize(Count = dplyr::n(), Event_Count = sum(!!rlang::sym(event_col) == 1, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Event_Rate = paste0(Event_Count, "/", Count, " (", round(Event_Count / Count * 100, decimal.percent), "%)"))
  } else {
    # count_by_var와 var_subgroup이 NULL일 때는 전체 데이터만 Total로 계산
    counts <- tibble::tibble(
      Total = "Total",
      Count = total_count,
      Event_Count = total_event_count,
      Event_Rate = total_event_rate
    )
    return(counts)
  }
  
  # Total 행을 추가
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
#'   1. All Count(count_by=…) columns are removed.
#'   2. A single column Count(by count_by) remains, containing the first
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
  if (!length(drop_cols)) return(df)
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





#' @title count_event_by_glm: function to count event rates for GLM analysis
#' @description Count event and subgroup numbers, supports automatic parsing of fixed-effect syntax (1|subgroup).
#' @param formula formula with binary response (0/1) and optional fixed-effect `(1|subgroup)` term.
#' @param data data.frame or survey.design
#' @param count_by_var variable name to stratify counts by (string)
#' @param var_subgroup subgroup variable name (string); automatically parsed from `(1|subgroup)` if not provided
#' @param decimal.percent decimals for percent display, Default: 1
#' @return tibble with Count, Event_Count, Event_Rate, and grouping columns
#' @export
#' @importFrom dplyr group_by summarise mutate arrange filter
#' @importFrom rlang .data
count_event_by_glm <- function(formula, data, count_by_var = NULL,
                               var_subgroup = NULL, decimal.percent = 1) {
  # handle survey designs
  if (inherits(data, "survey.design")) data <- data$variables
  # parse fixed-effect syntax (1|subgroup) in formula
  ftxt <- deparse(formula)
  if (is.null(var_subgroup)) {
    m <- regexec("\\(1\\|([^\\)]+)\\)", ftxt)
    vs <- regmatches(ftxt, m)[[1]]
    if (length(vs) == 2) {
      var_subgroup <- vs[2]
      ftxt <- gsub("\\s*\\(1\\|[^\\)]+\\)", "", ftxt)
      formula <- as.formula(ftxt)
    }
  }
  event_col <- as.character(formula[[2]])
  # total only
  if (is.null(count_by_var) && is.null(var_subgroup)) {
    total_n <- nrow(data)
    total_e <- sum(data[[event_col]] == 1, na.rm = TRUE)
    total_rate <- paste0(total_e, "/", total_n,
                         " (", round(total_e/total_n*100,
                                     decimal.percent), "%)")
    return(tibble::tibble(
      Total = "Total",
      Count = total_n,
      Event_Count = total_e,
      Event_Rate = total_rate
    ))
  }
  # both count_by and subgroup
  if (!is.null(count_by_var) && !is.null(var_subgroup)) {
    df <- dplyr::filter(data, !is.na(.data[[var_subgroup]]))
    base <- df %>%
      dplyr::group_by(.data[[count_by_var]], .data[[var_subgroup]]) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Event_Count = sum(.data[[event_col]] == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Event_Rate = paste0(Event_Count, "/", Count,
                            " (", round(Event_Count/Count*100,
                                        decimal.percent), "%)")
      )
    overall <- df %>%
      dplyr::group_by(.data[[count_by_var]]) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Event_Count = sum(.data[[event_col]] == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Event_Rate = paste0(Event_Count, "/", Count,
                            " (", round(Event_Count/Count*100,
                                        decimal.percent), "%)"),
        !!var_subgroup := "Overall"
      )
    return(dplyr::bind_rows(base, overall) %>%
             dplyr::arrange(.data[[count_by_var]], .data[[var_subgroup]]))
  }
  # only subgroup
  if (is.null(count_by_var) && !is.null(var_subgroup)) {
    df <- dplyr::filter(data, !is.na(.data[[var_subgroup]]))
    return(df %>%
             dplyr::group_by(.data[[var_subgroup]]) %>%
             dplyr::summarise(
               Count = dplyr::n(),
               Event_Count = sum(.data[[event_col]] == 1, na.rm = TRUE),
               .groups = "drop"
             ) %>%
             dplyr::mutate(
               Event_Rate = paste0(Event_Count, "/", Count,
                                   " (", round(Event_Count/Count*100,
                                               decimal.percent), "%)")
             ))
  }
  # only count_by
  if (!is.null(count_by_var) && is.null(var_subgroup)) {
    return(data %>%
             dplyr::group_by(.data[[count_by_var]]) %>%
             dplyr::summarise(
               Count = dplyr::n(),
               Event_Count = sum(.data[[event_col]] == 1, na.rm = TRUE),
               .groups = "drop"
             ) %>%
             dplyr::mutate(
               Event_Rate = paste0(Event_Count, "/", Count,
                                   " (", round(Event_Count/Count*100,
                                               decimal.percent), "%)")
             ))
  }
}



