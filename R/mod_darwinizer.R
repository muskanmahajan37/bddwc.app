# Module UI

#' @title   mod_darwinizer_ui and mod_darwinizer_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_darwinizer
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_darwinizer_ui <- function(id) {
  ns <- NS(id)
  tagList(column(
    12,
    
    #------- INPUT COLUMNS --------------
    
    div(
      id = ns("IDE"),
      column(2,  id = "origin", div(
        h4("Original Names", class = "control-header"),
        div(class = "control-header stats-red", textOutput(ns("origin_count"))),
        
        DT::dataTableOutput(ns("original"))
      )),
      column(2, div(
        h4("Darwinizer Dictionary", class = "control-header"),
        DT::dataTableOutput(ns("dictionary"))
      )),
      
      #------- CONTROLS --------------
      
      column(1, div(
        
        div(id = "controls", 
            
            fluidRow(actionButton(ns("darwinize"), "Darwinize", icon("award"), class = "activeButton shadow")),
            br(),
            fluidRow(actionButton(ns("manual"), "", icon("arrow-circle-right"), class = "readyButton shadow")),
            fluidRow(actionButton(ns("remove"), "", icon("backspace"), class = "readyButton shadow")),
            fluidRow(actionButton(ns("removeall"), "", icon("times-circle"), class = "readyButton shadow")),
            br(),
            fluidRow(downloadButton(ns("download"), "", class = "readyButton shadow")))
        
      )),
      
      #------- OUTPUT COLUMNS --------------
      
      column(7, id = "fixed",
             column(5, div(
               h4("Darwinized Names", class = "control-header"),
               div(class = "control-header stats-green", textOutput(ns("darwin_count"))),
               DT::dataTableOutput(ns("darwinized"))
             )),
             column(4, div(
               h4("Manual Renames", class = "control-header"),
               div(class = "control-header stats-green", textOutput(ns("manual_count"))),
               DT::dataTableOutput(ns("manual"))
             )),
             column(3, div(
               h4("Identical Matches", class = "control-header"),
               div(class = "control-header stats-green", textOutput(ns("identitical_count"))),
               DT::dataTableOutput(ns("identical"))
             )))
    )
    #------- END OF OUTPUT COLUMNS --------------
  ))
}

# Module Server

#' @rdname mod_darwinizer
#' @export
#' @keywords internal

mod_darwinizer_server <- function(input, output, session, data_original, dictionary) {
  ns <- session$ns
  
  identical <- data.frame()
  darwinized <- data.frame()
  manual <- data.frame()
  names_left <-  reactive(names(data_original))
  
  
  output$origin_count <-
    renderText({
      input$darwinize
      input$manual
      input$remove
      input$removeall
      ifelse(any(class(names_left) == 'reactive'), return(nrow(data_original)), return(length(names_left)))
    })
  
  output$darwin_count <-
    renderText({
      input$darwinize
      input$manual
      input$remove
      input$removeall
      nrow(darwinized)
    })
  
  output$manual_count <-
    renderText({
      input$darwinize
      input$manual
      input$remove
      input$removeall
      nrow(manual)
    })
  
  output$identitical_count <-
    renderText({
      input$darwinize
      input$manual
      input$remove
      input$removeall
      nrow(identical)
    })
  
  
  #---------- BUTTON --------------
  
  observeEvent(input$darwinize, {
    results <- bdDwC::darwinize_names(
      as.data.frame(data_original),
      as.data.frame(dictionary)
    )
    
    identical <<- results[results$match_type == "Identical", ]
    darwinized <<- results[results$match_type == "Darwinized", ]
    
    pre_names <- names(data_original)
    fixed_names <- c(identical$name_old, darwinized$name_old)
    
    names_left <<- pre_names[!(pre_names %in% fixed_names)]
    
    
    shinyjs::runjs(code = paste('$("#', ns("darwinize"), '").addClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("darwinize"), '").removeClass("activeButton");', sep = ""))
    
    shinyjs::runjs(code = paste('$("#', ns("manual"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("manual"), '").removeClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("remove"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("remove"), '").removeClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("removeall"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("removeall"), '").removeClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("download"), '").addClass("completedButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("download"), '").removeClass("readyButton");', sep = ""))
  })
  
  
  observeEvent(input$manual, {
    from <- input$original_rows_selected
    to <- input$dictionary_rows_selected
    
    from_name <- names_left[from]
    to_name <- dictionary[to, 2]
    
    manual <<- rbind(manual, data.frame(name_old = from_name, name_new = to_name))
    
    pre_names <- names_left
    names_left <<- pre_names[!(pre_names %in% from_name)]
  })
  
  observeEvent(input$download, {
    result <- data.frame(name_old = identical[,1], name_new = identical[,1])
    result <- rbind(result, darwinized)
    result <- rbind(result, manual)
    
  })
  
  output$download <- shiny::downloadHandler(
    filename = format(Sys.time(), "darwinizedData_%Y_%b_%d.csv"),
    content = function(file) {
      data.table::fwrite(
        rbind(manual, rbind(darwinized, data.frame(name_old = identical[,1], name_new = identical[,1]))),
        file
      )
    }
  )
  
  
  observeEvent(input$remove, {
    darwin_rem <- input$darwinized_rows_selected
    manual_rem <- input$manual_rows_selected
    identic_rem <- input$identical_rows_selected
    
    if(length(darwin_rem) > 0){
      names <- darwinized[darwin_rem, 1]
      darwinized <<- darwinized[!darwin_rem, ]
      names_left <<- c(names, names_left)
    }
    
    if(length(manual_rem) > 0){
      names <- as.character(manual[manual_rem, 1])
      manual <<- manual[c(-1 * manual_rem), ]
      names_left <<- c(names, names_left)
    }
    
    if(length(identic_rem) > 0){
    }
  })
  
  
  observeEvent(input$removeall, {
    identical <<- data.frame()
    darwinized <<- data.frame()
    manual <<- data.frame()
    
    names_left <<-  names(data_original)
  })
  
  
  
  #----------- TABLES -------------
  
  output$original <- DT::renderDataTable(DT::datatable({
    input$darwinize
    input$manual
    input$remove
    input$removeall
    
   if(any(class(names_left) == 'reactive')){
     as.data.frame(names_left())
   } else {
     as.data.frame(names_left)
   }
  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE
  ),
  rownames = TRUE,
  selection = 'single'))
  
  
  
  output$dictionary <- DT::renderDataTable(DT::datatable(
    data.frame(standardNames = unique(dictionary[,2:2])),
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE
    ),
    rownames = FALSE,
    selection = 'single'
  ))
  
  
  
  output$darwinized <- DT::renderDataTable(DT::datatable({
    input$darwinize
    input$manual
    input$remove
    input$removeall
    
    if(length(as.data.frame(identical)) > 0){
      as.data.frame(darwinized[, 1:2])
    } else {
      as.data.frame(darwinized)
    }
  },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE
    ),
    rownames = FALSE
  ))
  
  
  
  output$manual <- DT::renderDataTable(DT::datatable({
    input$darwinize
    input$manual
    input$remove
    input$removeall
    
    as.data.frame(manual)
  },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE
    ),
    rownames = FALSE
  ))
  
  
  
  output$identical <- DT::renderDataTable(DT::datatable({
    input$darwinize
    input$manual
    input$remove
    input$removeall
    
    if(length(as.data.frame(identical)) > 0){
      data.frame(name_same = identical[, 2:2])
    } else {
      as.data.frame(identical)
    }
  },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE
    ),
    rownames = FALSE
  ))
}





## To be copied in the UI
# mod_darwinizer_ui("darwinizer_ui_1")

## To be copied in the server
# callModule(mod_darwinizer_server, "darwinizer_ui_1")
