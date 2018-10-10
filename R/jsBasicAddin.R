
#' @title jsBasicGadget: Shiny Gadget of Basic Statistics in Medical Research.
#' @description Shiny Gadget including Data, Label info, Table 1, Regression(liear, logistic), Basic plot
#' @param data data
#' @return Shiny Gadget including Data, Label info, Table 1, Regression(liear, logistic), Basic plot
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(survival)
#'  jsBasicGadget(lung) 
#'  }
#' }
#' @rdname jsBasicGadget
#' @export 
#' @importFrom GGally ggpairs
#' @importFrom epiDisplay regress.display logistic.display
#' @importFrom stats as.formula binomial glm
#' @importFrom data.table data.table := .SD
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @import ggplot2 
#' @import shiny



jsBasicGadget <- function(data) {
  
  
  data <- data.table(data)
  
  factor_vars <- names(data)[sapply(data, class) %in% c("factor", "character")]

  conti_vars <- names(data)[sapply(data, class) %in% c("numeric", "integer")]
  nclass = data[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars]
  except_var <- names(nclass)[nclass == 1]
  factor2_vars <- names(nclass)[nclass == 2]

  
  
  ui <- navbarPage("Description",
                   tabPanel("Data",
                            tabsetPanel(type = "pills",
                                        tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                        tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                            )
                   ),
                   tabPanel("Table 1",
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput("group_vars", "Stratified by", 
                                               choices =  c("None", factor_vars), multiple = F, 
                                               selected = "None"
                                ),
                                selectizeInput("nonnormal_vars", "Non-normal variable (continuous)", 
                                               choices = conti_vars, multiple = T, 
                                               selected = NULL
                                ),
                                sliderInput("decimal_tb1_con", "Digits (continuous)",
                                            min = 1, max = 3, value = 1
                                ),
                                sliderInput("decimal_tb1_cat", "Digits (categorical, %)",
                                            min = 1, max = 3, value = 1
                                ),
                                conditionalPanel(
                                  condition = "input.group_vars != 'None'",
                                  sliderInput("decimal_tb1_p", "Digits (p)",
                                              min = 3, max = 5, value = 3
                                  ),
                                  selectizeInput("exact_vars", "Fisher's test (categorical)", 
                                                 choices = factor_vars, multiple = T, 
                                                 selected = NULL
                                  ),
                                  checkboxInput("smd", "Show SMD", F),
                                  selectizeInput("group2_vars", "2nd group (optional)", 
                                                 choices = c("None", factor_vars), multiple = F, 
                                                 selected = "None"
                                  )
                                ),
                                conditionalPanel(
                                  condition = "(input.group_vars != 'None') && (input.group2_vars != 'None') && (input.group2_vars != input.group_vars)",
                                  checkboxInput("psub", "Subgroup p-values", F)
                                )
                              ),
                              
                              mainPanel(
                                withLoader(DTOutput("table1"), type="html", loader="loader6"),
                                h5("ACS: acute coronary syndrome; SA: stable angina, LVEF: left ventricular ejection fraction; LAD: left anterior descending artery; QCA: quantitative coronary angiography; IVUS: intravascular ultrasound; MLA: minimal lumen area; FFR: fractional flow reserve"),
                                wellPanel(
                                  h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                  h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                  h5("Categorical variables  are summarized with table"),
                                  uiOutput("text_tb1_chisq"),
                                  uiOutput("text_tb1_fisher")
                                )
                              )
                            )
                   ),
                   navbarMenu("Regression",
                              tabPanel("Linear",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectizeInput("dep_vars_linear", "Dependent variable", 
                                                          choices = conti_vars, multiple = F, 
                                                          selected = conti_vars[1]
                                           ),
                                           selectizeInput("indep_vars_linear", "Independent variables", 
                                                          choices = names(data), multiple = T, 
                                                          selected = names(data)[1]
                                           ),
                                           sliderInput("decimal_linear", "Digits",
                                                       min = 1, max = 4, value = 2
                                           )
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Logistic",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectizeInput("dep_vars_logistic", "Dependent variable", 
                                                          choices = factor2_vars, multiple = F, 
                                                          selected = "center_involve"
                                           ),
                                           selectizeInput("indep_vars_logistic", "Independent variables", 
                                                          choices = names(data), multiple = T, 
                                                          selected = names(data)[1]
                                           ),
                                           sliderInput("decimal_logistic", "Digits",
                                                       min = 1, max = 4, value = 2
                                           )
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                         )
                                       )
                              )
                   ),
                   navbarMenu("Plot",
                              tabPanel("Basic plot",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectizeInput("var_scatter", "Variables", 
                                                          choices = names(data), multiple = T, 
                                                          selected = names(data)[1:2]
                                           ),
                                           selectizeInput("strata_scatter", "Strata", 
                                                          choices = c("None", factor_vars), multiple = F, 
                                                          selected = "None"
                                           ),
                                           selectizeInput("theme_scatter", "Theme",
                                                          c("default", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"), multiple = F, 
                                                          selected = "default"
                                           )
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("scatter"), type="html", loader="loader6"),
                                           h3("Graph option"),
                                           wellPanel(
                                             uiOutput("gtype")
                                           ),
                                           h3("Download options"),
                                           wellPanel(
                                             uiOutput("downloadControls_scatter"),
                                             downloadButton("downloadButton_scatter", label = "Download the plot") 
                                           )
                                           
                                         )
                                       )
                              )
                              
                   )
  )
  
  server <- function(input, output, session) {
    
    output$data <- renderDT({
      datatable(data, rownames=F, editable = F, caption = "Data",
                options = list(scrollX = TRUE)
      )
    })
    
    
    data.label <- reactive(mk.lev(data))
    
    data.val <- reactive({
      out.val = data
      for (i in factor_vars){
        levels(out.val[[i]]) = data.label()[variable == i, val_label]
      }
      return(out.val[])
    })
      
    
    
    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = T, extensions= "Buttons", caption = "Labels of data",
                options = opt.data("data")
      )
    })
    
    output$table1 <- renderDT({
      
      validate(need(!(input$group_vars %in% except_var), 'This variable has only 1 category'))
      var_label(data) = sapply(names(data), function(v){data.label()[variable == v, var_label][1]}, simplify = F)
      vars = setdiff(names(data),  c(input$group_vars))
      
      if (input$group_vars == "None"){
        res = CreateTableOneJS(data = data, 
                               vars = vars, includeNA = F, test = T,
                               testApprox = chisq.test, argsApprox = list(correct = TRUE),
                               testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                               testNormal = oneway.test, argsNormal = list(var.equal = F),
                               testNonNormal = kruskal.test, argsNonNormal = list(NULL), 
                               showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars, 
                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, labeldata = data.label())
        
        datatable(res$table, rownames=T, extensions= "Buttons", caption = res$caption,
                  options = c(opt.tb1("tb1"), 
                              list(columnDefs = list(list(visible=FALSE, targets= which(colnames(res$table) %in% c("test","sig"))))
                              ),
                              list(scrollX = TRUE)
                  )
        ) 
      } else if ((input$group2_vars == "None") | (input$group2_vars == input$group_vars)){
        vars.tb1 = setdiff(vars, input$group_vars)
        res = CreateTableOneJS(data = data, 
                               vars = vars.tb1, strata = input$group_vars, includeNA = F, test = T,
                               testApprox = chisq.test, argsApprox = list(correct = TRUE),
                               testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                               testNormal = oneway.test, argsNormal = list(var.equal = F),
                               testNonNormal = kruskal.test, argsNonNormal = list(NULL), 
                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = input$exact_vars, nonnormal = input$nonnormal_vars, 
                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data.label())
        
        datatable(res$table, rownames=T, extensions= "Buttons", caption = res$caption,
                  options = c(opt.tb1("tb1"), 
                              list(columnDefs = list(list(visible=FALSE, targets= which(colnames(res$table) %in% c("test","sig"))))
                              ),
                              list(scrollX = TRUE)
                  )
        ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
      } else if (input$psub == T){
        vars.tb1 = setdiff(vars, c(input$group2_vars, input$group_vars))
        res = CreateTableOneJS(data = data, 
                               vars = vars.tb1, strata = input$group_vars, strata2 = input$group2_vars, includeNA = F, test = T,
                               testApprox = chisq.test, argsApprox = list(correct = TRUE),
                               testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                               testNormal = oneway.test, argsNormal = list(var.equal = F),
                               testNonNormal = kruskal.test, argsNonNormal = list(NULL), 
                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = input$exact_vars, nonnormal = input$nonnormal_vars, 
                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data.label(), psub = input$psub)
        
        sigs = which(colnames(res$table) =="sig")
        datatable(res$table, rownames=T, extensions= "Buttons", caption = res$caption,
                  options = c(opt.tb1("tb1"), 
                              list(columnDefs = list(list(visible=FALSE, targets= which(colnames(res$table) %in% c("test","sig"))))
                              ),
                              list(scrollX = TRUE)
                  )
        ) %>% formatStyle(sigs, target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      } else {
        vars.tb1 = setdiff(vars, c(input$group2_vars, input$group_vars))
        res = CreateTableOneJS(data = data, 
                               vars = vars.tb1, strata = input$group_vars, strata2 = input$group2_vars, includeNA = F, test = T,
                               testApprox = chisq.test, argsApprox = list(correct = TRUE),
                               testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                               testNormal = oneway.test, argsNormal = list(var.equal = F),
                               testNonNormal = kruskal.test, argsNonNormal = list(NULL), 
                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = input$exact_vars, nonnormal = input$nonnormal_vars, 
                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data.label(), psub = input$psub)
        
        datatable(res$table, rownames=T, extensions= "Buttons", caption = res$caption,
                  options = c(opt.tb1("tb1"), 
                              list(columnDefs = list(list(visible=FALSE, targets= which(colnames(res$table) %in% c("test","sig"))))
                              ),
                              list(scrollX = TRUE)
                  )
        ) %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      
    })
    
    output$lineartable <- renderDT({

      y = input$dep_vars_linear
      xs = input$indep_vars_linear
      
      validate(
        need(!is.null(xs) , "Please select at least 1 variable")
      )
      
      validate(
        need(!(y %in% xs), "Dependent variable can't be a independent variable. Please remove it.")
      )
      
      form = as.formula(paste(y,"~",paste(xs,collapse=" + "), sep=" "))
      res.linear = glm(form, data = data) 
      tb.linear = regress.display(res.linear, crude = T, crude.p.value = T, decimal = input$decimal_logistic)
      cap.lv = data.label()[variable ==  y, val_label]
      cap.linear = paste("Linear regression predicting ", data.label()[variable == y, var_label][1], sep="")

      out.linear = LabelepiDisplay(tb.linear, label = T, ref = data.label())
      hide = which(colnames(out.linear) == c("P(LR-test)",  "sig"))
      datatable(out.linear, rownames=T, extensions= "Buttons", caption = cap.linear,
                options = c(opt.tbreg(paste("linear", y , paste(xs, collapse = "_"), sep="_")), 
                            list(columnDefs = list(list(visible=FALSE, targets= hide ))
                            )
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
      
      
    })
    
    output$logistictable <- renderDT({
      y = input$dep_vars_logistic
      validate(
        need(nrow(data.label()[variable == y]) ==2, "Dependent variable must have 2 categories")
      )
      xs = input$indep_vars_logistic
      
      validate(
        need(!is.null(xs) , "Please select at least 1 variable")
      )
      
      validate(
        need(!(y %in% xs), "Dependent variable can't be a independent variable. Please remove it.")
      )
      
      form = as.formula(paste(y,"~",paste(xs,collapse=" + "), sep=" "))
      res.logistic = glm(form, data = data, family= binomial) 
      tb.logistic = logistic.display(res.logistic, crude = T, crude.p.value = T, decimal = input$decimal_logistic)
     
      cap.lv = data.label()[variable ==  y, val_label]
      cap.logistic = paste("Logistic regression predicting ", data.label()[variable ==y, var_label][1], " : ", cap.lv[1]," vs ", cap.lv[2], sep="")
      #out.logistic = LabelTable(tb.logistic, xs, ref = new.label)
      out.logistic = LabelepiDisplay(tb.logistic, label = T, ref = data.label())
      hide = which(colnames(out.logistic) == c("P(LR-test)",  "sig"))
      datatable(out.logistic, rownames=T, extensions= "Buttons", caption = cap.logistic,
                options = c(opt.tbreg(paste("logistic", y , paste(xs, collapse = "_"), sep="_")), 
                            list(columnDefs = list(list(visible=FALSE, targets= hide ))
                            )
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
      
      
    })
    
    
    
    # Plot
    ## Basic plot
    
    scatterInput <- reactive({
      if (is.null(input$var_scatter)){
        return(NULL)
      } else if (input$strata_scatter == "None"){
        p = ggpairs(data.val(), columns = input$var_scatter, columnLabels = sapply(input$var_scatter, function(x){data.label()[variable == x, var_label][1]}), 
                    upper = list(continuous = input$gytpe_upper_conti, combo = input$gytpe_upper_combo, discrete = input$gytpe_upper_discrete, na = "na"),
                    lower = list(continuous = input$gytpe_lower_conti, combo = input$gytpe_lower_combo, discrete = input$gytpe_lower_discrete, na = "na"),
                    diag = list(continuous = input$gytpe_diag_conti, discrete = input$gytpe_diag_discrete,  na = "na"),
                    axisLabels='show') 
      } else {
        p = ggpairs(data.val(), columns = input$var_scatter, columnLabels = sapply(input$var_scatter, function(x){data.label()[variable == x, var_label][1]}), 
                    mapping = aes_string(color = input$strata_scatter),
                    upper = list(continuous = input$gytpe_upper_conti, combo = input$gytpe_upper_combo, discrete = input$gytpe_upper_discrete, na = "na"),
                    lower = list(continuous = input$gytpe_lower_conti, combo = input$gytpe_lower_combo, discrete = input$gytpe_lower_discrete, na = "na"),
                    diag = list(continuous = input$gytpe_diag_conti, discrete = input$gytpe_diag_discrete,  na = "na"),
                    axisLabels='show')
      }
      
      if (input$theme_scatter == "default"){
        return(p)
      } else if(input$theme_scatter == "bw"){
        return(p + theme_bw())
      } else if(input$theme_scatter == "linedraw"){
        return(p + theme_linedraw())
      } else if(input$theme_scatter == "light"){
        return(p + theme_light())
      } else if(input$theme_scatter == "dark"){
        return(p + theme_dark())
      } else if(input$theme_scatter == "minimal"){
        return(p + theme_minimal())
      } else if(input$theme_scatter == "classic"){
        return(p + theme_classic())
      } else if(input$theme_scatter == "void"){
        return(p + theme_void())
      } 
      
    })
    
    output$scatter <- renderPlot({
      print(scatterInput())
    })
    
    
    output$gtype <- renderUI({
      fluidRow(
        h4("Upper"),
        column(4, 
               selectizeInput("gytpe_upper_conti", "conti", 
                              choices = c("points", "smooth", "smooth_loess", "density", "cor", "blank"), multiple = F, 
                              selected = "cor"
               )
        ),
        
        column(4,
               selectizeInput("gytpe_upper_combo", "combo", 
                              choices = c('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'), multiple = F, 
                              selected = "box_no_facet"
               )
        ),
        column(4,
               selectizeInput("gytpe_upper_discrete", "discrete", 
                              choices = c('facetbar', 'ratio', 'blank'), multiple = F, 
                              selected = "cor"
               )
        ),
        h4("Lower"),
        column(4, 
               selectizeInput("gytpe_lower_conti", "conti", 
                              choices = c("points", "smooth", "smooth_loess", "density", "cor", "blank"), multiple = F, 
                              selected = "smooth_loess"
               )
        ),
        
        column(4,
               selectizeInput("gytpe_lower_combo", "combo", 
                              choices = c('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'), multiple = F, 
                              selected = "facethist"
               )
        ),
        column(4,
               selectizeInput("gytpe_lower_discrete", "discrete", 
                              choices = c('facetbar', 'ratio', 'blank'), multiple = F, 
                              selected = "facetbar"
               )
        ),
        h4("Diag"),
        column(4, 
               selectizeInput("gytpe_diag_conti", "conti", 
                              choices = c('densityDiag', 'barDiag', 'blankDiag'), multiple = F, 
                              selected = "densityDiag"
               )
        ),
        
        column(4,
               selectizeInput("gytpe_diag_discrete", "discrete", 
                              choices = c('barDiag', 'blankDiag'), multiple = F, 
                              selected = "barDiag"
               )
        )
      )
      
      
      
    }) 
    
    
    
    
    
    output$downloadControls_scatter <- renderUI({
      fluidRow(
        column(4,
               selectizeInput("file_ext_scatter", "File extension (dpi = 300)", 
                              choices = c("jpg","pdf", "tiff", "svg"), multiple = F, 
                              selected = "jpg"
               )
        ),
        column(4,
               sliderInput("fig_width_scatter", "Width (in):",
                           min = 5, max = 15, value = 8
               )
        ),
        column(4,
               sliderInput("fig_height_scatter", "Height (in):",
                           min = 5, max = 15, value = 6
               )
        )
      )
      
      
    })
    
    output$downloadButton_scatter <- downloadHandler(
      filename =  function() {
        paste(input$var_scatter,"_by_", input$strata_scatter, "_ggpairs.", input$file_ext_scatter ,sep="")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file,scatterInput(), dpi = 300, units = "in", width = input$fig_width_scatter, height =input$fig_height_scatter)
        
      } 
    )
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }
  
  
  viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  runGadget(ui, server, viewer = viewer)
}



#' @title jsBasicAddin: Rstudio addin of jsBasicGadget 
#' @description Rstudio addin of jsBasicGadget 
#' @return Rstudio addin of jsBasicGadget 
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsBasicAddin
#' @export 
#' @importFrom rstudioapi getActiveDocumentContext


jsBasicAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  #viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsBasicGadget(data)
}


