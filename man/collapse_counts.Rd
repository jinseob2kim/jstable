% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{collapse_counts}
\alias{collapse_counts}
\title{collapse_counts}
\usage{
collapse_counts(df, count_by)
}
\arguments{
\item{df}{A data.frame produced by one of the TableSubgroup functions}

\item{count_by}{A string giving the name of the variable whose per-level
count columns should be collapsed. If NULL, df is returned unmodified.}
}
\value{
A data.frame identical to `f except that:
All Count(count_by=…) columns are removed.
A single column Count(by count_by) remains, containing the first
     non-missing, non-empty value from the original per-level columns.
}
\description{
#' When a grouping variable  count_by has more than two levels, this utility collapses all of the level-specific columns
}
\details{
This helper is intended for internal use by
TableSubgroupCox(), TableSubgroupMultiCox(), and their GLM equivalents.
It simplifies reporting when you request counts stratified by a factor with
more than two levels.
}
