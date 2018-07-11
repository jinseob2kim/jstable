## Label data 


#' @title Export label and level: one variable
#' @description Export label and level: one variable
#' @param data data
#' @param vname variable to export label and level
#' @return if continuous variable - (label, NA), categorical variable - (label, level)
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  out.list = lapply(names(data), function(x){mk.lev.var(data, x)})
#'  }
#' }
#' @rdname mk.lev.var
#' @export 

mk.lev.var = function(data , vname){
  v.vec = data[[vname]]
  out = ""
  if (is.numeric(v.vec)){
    out = c(vname, class(v.vec), NA)
  } else{
    v.level = levels(v.vec)
    nr = length(v.level)
    out = cbind(rep(vname, nr), rep(class(v.vec), nr), v.level)
  }
  return(out)
}



#' @title Export label and level: multiple variable
#' @description Export label and level: multiple variable
#' @param data data
#' @return default label and level data
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data.label = mk.lev(data)
#'  }
#' }
#' @rdname mk.lev
#' @export 
#' @importFrom data.table data.table :=

mk.lev = function(data){
  out.list = lapply(names(data), function(x){mk.lev.var(data, x)})
  out.dt = data.table(Reduce(rbind, out.list))
  names(out.dt) = c("variable", "class","level")
  out.dt[, var_label := variable]
  out.dt[, val_label := level]
  return(out.dt[])
}
