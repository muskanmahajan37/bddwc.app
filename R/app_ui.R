#' @import shiny shinydashboard bdutilities.app
app_ui <- function() {
  tagList(# List the first level UI elements here
    dashboardPage(
      dashboardHeader(title = "bddwc"),
      
      dashboardSidebar(sidebarMenu(
        id = "sideBar",
        menuItem(
          "Add Data",
          tabName = "add",
          icon = icon("plus-circle")
        ),
        menuItem(
          "Configure Dictionary",
          tabName = "configure",
          icon = icon("wrench")
        ),
        menuItem("Darwinize",
                 tabName = "darwinize",
                 icon = icon("file-csv")),
        menuItem("Cite Us",
                 tabName = "citation",
                 icon = icon("copyright"))
      )),
      
      dashboardBody(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        
        tabItems(
          # ------------- Add Data Module -------------------
          tabItem("add",
                  fluidRow(
                    div(mod_add_data_ui("bdFileInput")),
                    
                    column(12,
                           div(
                             id = "dataToDictionaryDiv",
                             tags$br(),
                             actionButton("dataToDictionary", "Next: Configure Dictionary")
                           ))
                  )),
          
          tabItem("configure",
                  fluidRow(div(
                    mod_add_dictionary_ui("bdDictionaryInput")                  ),
                    
                    column(12,
                           div(
                             id = "dictionaryToDarwinDiv",
                             tags$br(),
                             actionButton("dictionaryToDarwin", "Next: Darwinize")
                           ))
                    
                    )),
          
          tabItem("darwinize",
                  fluidRow(div(
                    mod_darwinizer_ui("bdDarwinizer")
                  ))),
          
          tabItem("citation",
                  fluidRow(div(
                    mod_citation_ui("citation_ui_1")
                  )))
        )
      )
    ))
  
}

#' @import shiny shinyjs
golem_add_external_resources <- function() {
  addResourcePath('www', system.file('app/www', package = 'bddwc.app'))
  
  tags$head(
    # golem::js(),
    golem::favicon(),
    shinyjs::useShinyjs(),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "www/input.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
  )
}
