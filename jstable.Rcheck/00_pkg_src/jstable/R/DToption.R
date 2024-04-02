#' @title datable option for data(DT package)
#' @description DT::datatable option for data
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples
#' opt.data("mtcars")
#' @rdname opt.data
#' @export

opt.data <- function(fname) {
  return(
    list(
      dom = "<lf<rt>Bip>", lengthMenu = list(c(10, 25, -1), c("10", "25", "All")), pageLength = 10,
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = fname),
            list(extend = "excel", filename = fname),
            list(extend = "pdf", filename = fname)
          ),
          text = "Download"
        )
      )
    )
  )
}


#' @title datable option for table 1(DT package)
#' @description DT::datatable option for table 1
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples
#' options <- opt.tb1("mtcars")
#' @rdname opt.tb1
#' @export

opt.tb1 <- function(fname) {
  return(
    list(
      dom = "<lf<rt>Bip>", lengthMenu = list(c(10, 25, -1), c("10", "25", "All")), pageLength = 25, ordering = F,
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = fname),
            list(extend = "excel", filename = fname),
            list(extend = "pdf", filename = fname)
          ),
          text = "Download"
        )
      )
    )
  )
}



#' @title datable option for regression table(DT package)
#' @description DT::datatable option for glm, gee(geepack package), lmer/glmer(lme4 package)
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples
#' options <- opt.tbreg("mtcars")
#' @rdname opt.tbreg
#' @export

opt.tbreg <- function(fname) {
  return(
    list(
      dom = "<lf<rt>Bip>", lengthMenu = list(c(10, 25, -1), c("10", "25", "All")), pageLength = -1, ordering = F,
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = fname),
            list(extend = "excel", filename = fname),
            list(extend = "pdf", filename = fname)
          ),
          text = "Download"
        )
      )
    )
  )
}

#' @title datable option for ROC result(DT package)
#' @description DT::datatable option for ROC result
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples
#' options <- opt.roc("mtcars")
#' @rdname opt.roc
#' @export

opt.roc <- function(fname) {
  return(
    list(
      dom = "<<rt>Bip>", ordering = F,
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = fname),
            list(extend = "excel", filename = fname),
            list(extend = "pdf", filename = fname)
          ),
          text = "Download"
        )
      )
    )
  )
}



#' @title datable option for simple download(DT package)
#' @description Simple download DT::datatable option - No filter, No page
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples
#' options <- opt.simpledown("mtcars")
#' @rdname opt.simpledown
#' @export

opt.simpledown <- function(fname) {
  return(
    list(
      dom = "<<rt>B>", ordering = F, pageLength = -1,
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = fname),
            list(extend = "excel", filename = fname),
            list(extend = "pdf", filename = fname)
          ),
          text = "Download"
        )
      )
    )
  )
}
