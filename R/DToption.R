
#' @title datable option for data(DT package)
#' @description DT::datatable option for data
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  datatable(a.label, rownames=F, extension= "Buttons", caption = "Labels of data",
#'  options = opt.data("data")
#'  )
#'  }
#' }
#' @rdname opt.data
#' @export 

opt.data = function(fname){
  return(
    list(dom = '<lf<rt>Bip>', lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), pageLength = 10,
         buttons = list('copy', 
                        'print', 
                        list(extend = 'collection', 
                             buttons = list(list(extend = 'csv', filename= fname),
                                            list(extend = 'excel', filename= fname), 
                                            list(extend = 'pdf', filename= fname)
                             ), 
                             text = 'Download')
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
#' \dontrun{
#' if(interactive()){
#'  datatable(ptb1, rownames=T, extension= "Buttons", caption = cap.tb1,
#'  options = c(opt.tb1("tb1"), list(columnDefs = list(list(visible=FALSE, targets=ncol(ptb1)))
#'                                  )
#'             )
#'  }
#' }
#' @rdname opt.tb1
#' @export 

opt.tb1 = function(fname){
  return(
    list(dom = '<lf<rt>Bip>', lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), pageLength = 10, ordering = F,
         buttons = list('copy', 
                        'print', 
                        list(extend = 'collection', 
                             buttons = list(list(extend = 'csv', filename= fname),
                                            list(extend = 'excel', filename= fname), 
                                            list(extend = 'pdf', filename= fname)
                             ), 
                             text = 'Download')
         )
    )
  )
}



#' @title datable option for regression table(DT package)
#' @description DT::datatable option for glm, gee(geepack packge), lmer/glmer(lme4 package)
#' @param fname File name to download
#' @return datatable option object
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  datatable(out.gee, rownames=T, extension= "Buttons", caption = cap.gee,
#'           options = c(opt.tbreg(paste("gee_gaussian", y , paste(xs, collapse = "_"), sep="_"))
#'                      )
#'           )
#'  }
#' }
#' @rdname opt.tbreg
#' @export 

opt.tbreg = function(fname){
  return(
    list(dom = '<lf<rt>Bip>', lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), pageLength = -1, ordering=F,
         buttons = list('copy', 
                        'print', 
                        list(extend = 'collection', 
                             buttons = list(list(extend = 'csv', filename= fname),
                                            list(extend = 'excel', filename= fname), 
                                            list(extend = 'pdf', filename= fname)
                             ), 
                             text = 'Download')
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
#' \dontrun{
#' if(interactive()){
#'  datatable(roc.table, rownames=F, extension= "Buttons", caption = "Best cut-off", 
#'            options = opt.roc(paste("roc", input$dep_vars_roc , 
#'                                    paste(xs = input$indep_vars_roc, collapse = "_"), sep="_"))
#'            )
#'  }
#' }
#' @rdname opt.roc
#' @export 

opt.roc = function(fname){
  return(
    list(dom = '<<rt>Bip>', ordering=F,
         buttons = list('copy', 
                        'print', 
                        list(extend = 'collection', 
                             buttons = list(list(extend = 'csv', filename= fname),
                                            list(extend = 'excel', filename= fname), 
                                            list(extend = 'pdf', filename= fname)
                             ), 
                             text = 'Download')
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
#' \dontrun{
#' if(interactive()){
#'  datatable(NIInput()[["stat"]], rownames=T, extension= "Buttons", caption = "Outcome: statistics",
#'  options = c(opt.simpledown("Outcome_absent"), 
#'  list(scrollX = TRUE)
#'  ))
#'  }
#' }
#' @rdname opt.simpledown
#' @export 

opt.simpledown = function(fname){
  return(
    list(dom = '<<rt>B>', ordering=F, pageLength = -1,
         buttons = list('copy', 
                        'print', 
                        list(extend = 'collection', 
                             buttons = list(list(extend = 'csv', filename= fname),
                                            list(extend = 'excel', filename= fname), 
                                            list(extend = 'pdf', filename= fname)
                             ), 
                             text = 'Download')
         )
    )
  )
}
