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
        p("124", class = "control-header stats-red"),
        
        DT::dataTableOutput(ns("original"))
      )),
      column(2, div(
        h4("Darwinizer Dictionary", class = "control-header"),
        DT::dataTableOutput(ns("dictionary"))
      )),
      
      #------- CONTROLS --------------
      
      column(2, div(
        
        div(id = "controls", 
            
            fluidRow(actionButton(ns("queryDatabase"), "", icon("award"), class = "activeButton shadow ")),
            br(),
            fluidRow(actionButton(ns("queryDatabase"), "", icon("arrow-circle-right"), class = "readyButton shadow ")),
            fluidRow(actionButton(ns("queryDatabase"), "", icon("backspace"), class = "readyButton shadow ")),
            fluidRow(actionButton(ns("queryDatabase"), "", icon("times-circle"), class = "readyButton shadow ")),
            br(),
            fluidRow(actionButton(ns("queryDatabase"), "", icon("file-download"), class = "completedButton shadow ")))
        
      )),
      
      #------- OUTPUT COLUMNS --------------
      
      column(6, id = "fixed",
             column(4, div(
               h4("Darwinized Names", class = "control-header"),
               p("6", class = "control-header stats-green"),
               DT::dataTableOutput(ns("darwinized"))
             )),
             column(4, div(
               h4("Manual Renames", class = "control-header"),
               p("12", class = "control-header stats-green"),
               DT::dataTableOutput(ns("manual"))
             )),
             column(4, div(
               h4("Identical Matches", class = "control-header"),
               p("74", class = "control-header stats-green"),
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

mod_darwinizer_server <- function(input, output, session) {
  ns <- session$ns
  
  output$original <- DT::renderDataTable(
    DT::datatable(
      as.data.frame(bdDwC:::data_darwin_cloud$data$fieldname[1:110])
      ,
      options = list(
        paging = FALSE,
        autoWidth = TRUE,
        scrollY = TRUE,
        searching = FALSE,
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  $(thead).remove();",
          "}"
        )
      ),
      filter = 'top',
      rownames = FALSE,
    )
  )
  
  
  output$dictionary <- DT::renderDataTable(DT::datatable(
    as.data.frame(bdDwC:::data_darwin_cloud$data$standard)
    ,
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      searching = FALSE,
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  $(thead).remove();",
        "}"
      )
    ),
    rownames = FALSE,
    filter = 'top'
  ))
  
  output$darwinized <- DT::renderDataTable(DT::datatable(
    as.data.frame(bdDwC:::data_darwin_cloud$data$standard[1:10])
    ,
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      searching = FALSE,
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  $(thead).remove();",
        "}"
      )
    ),
    rownames = FALSE,
    filter = 'top'
  ))
  
  output$manual <- DT::renderDataTable(DT::datatable(
    as.data.frame(bdDwC:::data_darwin_cloud$data$standard[1:3])
    ,
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      searching = FALSE,
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  $(thead).remove();",
        "}"
      )
    ),
    rownames = FALSE,
    filter = 'top'
  ))
  
  output$identical <- DT::renderDataTable(DT::datatable(
    as.data.frame(bdDwC:::data_darwin_cloud$data$standard[1:70])
    ,
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      searching = FALSE,
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  $(thead).remove();",
        "}"
      )
    ),
    rownames = FALSE,
    filter = 'top'
  ))
}





## To be copied in the UI
# mod_darwinizer_ui("darwinizer_ui_1")

## To be copied in the server
# callModule(mod_darwinizer_server, "darwinizer_ui_1")
