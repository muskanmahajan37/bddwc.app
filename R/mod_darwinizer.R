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
            fluidRow(downloadButton(ns("downloadData"), "", class = "readyButton shadow")),
            
            br(),
            fluidRow(downloadButton(ns("downloadDictionary"), "", class = "readyButton shadow")))
        
        
        
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
    #------- END OF COLUMNS --------------
  ))
}

# Module Server

#' @rdname mod_darwinizer
#' @export
#' @keywords internal
#' @importFrom utils write.csv
#' @import bdutilities
mod_darwinizer_server <- function(input, output, session, data_original, darwin_dictionary) {
  ns <- session$ns
  
  # data_original <- shiny::observe(data_original_temp)
  identical <- data.frame()
  darwinized <- data.frame()
  manual <- data.frame()
  names_left <-  reactive(names(data_original()))
  darwin_dictionary_unique <- reactive(unique(darwin_dictionary()$standard))
  is_darwinized <- FALSE
  
  #------- BIG FAT NUMBERS --------------
  
  output$origin_count <-
    renderText({
      input$darwinize
      input$manual
      input$remove
      input$removeall
      ifelse(any(class(names_left) == 'reactive'), return(length(data_original())), return(length(names_left)))
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
  
  #------- BIG FAT NUMBERS --------------
  
  #---------- BUTTON --------------
  
  observeEvent(input$darwinize, {
    results <- bdDwC::darwinize_names(
      as.data.frame(data_original()),
      as.data.frame(darwin_dictionary())
    )
    
    identical <<- results[results$match_type == "Identical", ]
    darwinized <<- results[results$match_type == "Darwinized", ]
    
    pre_names <- names(data_original())
    fixed_names <- c(identical$name_old, darwinized$name_old)
    fixed_standards <- c(identical$name_old, darwinized$name_new)
    
    names_left <<- pre_names[!(pre_names %in% fixed_names)]
    darwin_dictionary_unique <<- bdutilities::return_core(darwin_dictionary_unique)
    darwin_dictionary_unique <<- darwin_dictionary_unique[!(darwin_dictionary_unique %in% fixed_standards)]
    
    
    shinyjs::runjs(code = paste('$("#', ns("darwinize"), '").addClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("darwinize"), '").removeClass("activeButton");', sep = ""))
    
    shinyjs::runjs(code = paste('$("#', ns("manual"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("manual"), '").removeClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("remove"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("remove"), '").removeClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("removeall"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("removeall"), '").removeClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("downloadData"), '").addClass("completedButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("downloadData"), '").removeClass("readyButton");', sep = ""))
    
    is_darwinized <<- TRUE
  })
  
  
  observeEvent(input$manual, {
    
    if(!is_darwinized){
      showNotification("Please Darwinize First",
                       duration = 6) 
      return()
    }
    
    from <- input$original_rows_selected
    to <- input$dictionary_rows_selected
    
    if(is.null(from) || is.null(to)){
      showNotification("Select a row each from two tables on the left to manually rename.",
                       duration = 6) 
    } else {
      from_name <- names_left[from]
      to_name <- darwin_dictionary_unique[to]
      
      manual <<- rbind(manual, data.frame(name_old = from_name, name_new = to_name))
      
      pre_names <- names_left
      names_left <<- pre_names[!(pre_names %in% from_name)]
      darwin_dictionary_unique <<- darwin_dictionary_unique[!(darwin_dictionary_unique %in% to_name)]
    }
  })
  
  observeEvent(input$remove, {
    if(!is_darwinized){
      showNotification("Please Darwinize First",
                       duration = 6) 
      return()
    }
    
    darwin_rem <- input$darwinized_rows_selected
    manual_rem <- input$manual_rows_selected
    identic_rem <- input$identical_rows_selected
    
    if(length(darwin_rem) > 0){
      names <- darwinized[darwin_rem, 1]
      
      names_darwin <- darwinized[darwin_rem, 2]
      darwin_dictionary_unique <<- c(names_darwin, darwin_dictionary_unique)
      
      darwinized <<- darwinized[c(-1 * darwin_rem), ]
      names_left <<- c(names, names_left)
    }
    
    if(length(manual_rem) > 0){
      names <- as.character(manual[manual_rem, 1])
      
      names_darwin <- as.character(manual[manual_rem, 2])
      darwin_dictionary_unique <<- c(names_darwin, darwin_dictionary_unique)
      
      manual <<- manual[c(-1 * manual_rem), ]
      names_left <<- c(names, names_left)
    }
    
    if(length(identic_rem) > 0){
      names <- as.character(identical[identic_rem, 1])
      
      names_darwin <- as.character(identical[identic_rem, 2])
      darwin_dictionary_unique <<- c(names_darwin, darwin_dictionary_unique)
      
      
      identical <<- identical[c(-1 * identic_rem), ]
      names_left <<- c(names, names_left)
    }
  })
  
  
  observeEvent(input$removeall, {
    if(!is_darwinized){
      showNotification("Please Darwinize First",
                       duration = 6) 
      return()
    }
    
    identical <<- data.frame()
    darwinized <<- data.frame()
    manual <<- data.frame()
    
    names_left <<-  names(data_original())
    darwin_dictionary_unique <<- unique(darwin_dictionary()$standard)
    
  })
  
  #---------- BUTTON --------------
  
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
  
  
  output$dictionary <- DT::renderDataTable(DT::datatable({
    input$darwinize
    input$manual
    input$remove
    input$removeall
    data.frame(standardNames = bdutilities::return_core(darwin_dictionary_unique))
  },
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
  
  
  #----------- TABLES -------------
  
  #----------- DOWNLOAD HANDLERS -------------
  
  
  output$downloadDictionary <- shiny::downloadHandler(
    filename = format(Sys.time(), "manualDictionary_%Y_%b_%d.csv"),
    content = function(file) {
      if(!is_darwinized){
        showNotification("Please Darwinize First",
                         duration = 6) 
        return()
      }
      
      df <- data.frame(name_old = "dummy", name_new = "dummy")
      
      if (nrow(identical) != 0)
        df <- rbind(df, data.frame(name_old = identical[, 1], name_new = identical[, 1]))
      
      if (nrow(manual) != 0)
        df <- rbind(df, manual)
      
      if (nrow(darwinized) != 0)
        df <- rbind(df, darwinized)
      
      # removing dummy row
      df <- df[-c(1),]
      
      data.table::fwrite(df, file)
    }
  )
  
  output$downloadData <- shiny::downloadHandler(
    filename = format(Sys.time(), "darwinizedData_%Y_%b_%d.csv"),
    content = function(file) {
      if(!is_darwinized){
        showNotification("Please Darwinize First",
                         duration = 6) 
        return()
      }
      
      manual[, ] <- lapply(manual, function(x) {
        as.character(x)
      })
      
      dat <- data.frame(
        name_old = c(identical[, 1], darwinized$name_old, manual$name_old),
        name_new = c(identical[, 1], darwinized$name_new, manual$name_new)
      )
      dat[, ] <- lapply(dat, function(x) {
        as.character(x)
      })
      
      write.csv(bdDwC::rename_user_data(as.data.frame(data_original()), dat),
                file)
    }
  )
  
  #----------- DOWNLOAD HANDLERS -------------
  
}
