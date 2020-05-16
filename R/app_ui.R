#' @import shiny shinydashboard bdutilities.app
#' @export
app_ui <- function() {
  tagList(
    dashboardPage(
      dashboardHeader(title = "bddwc"),
      
      dashboardSidebar(
        sidebarMenu(
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
          menuItem(
            "Darwinize",
            tabName = "darwinize",
            icon = icon("table")
          ),
          menuItem("Cite Us",
                   tabName = "citation",
                   icon = icon("copyright"))
        )
      ),
      
      dashboardBody(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        
        tabItems(
          tabItem("add",
                  fluidRow(
                    div(bdutilities.app::mod_add_data_ui("bdFileInput")),
                    
                    column(12,
                           div(
                             id = "dataToDictionaryDiv",
                             tags$br(),
                             actionButton("dataToDictionary", "Next: Configure Dictionary")
                           ))
                  )),
          
          tabItem("configure",
                  fluidRow(
                    div(mod_add_dictionary_ui("bdDictionaryInput")),
                    
                    column(
                      12,
                      div(
                        id = "dictionaryToDarwinDiv",
                        tags$br(),
                        actionButton("dictionaryToDarwin", "Next: Darwinize")
                      )
                    )
                    
                  )),
          
          tabItem("darwinize",
                  fluidRow(div(
                    mod_darwinizer_ui("bdDarwinizer")
                  ))),
          
          tabItem("citation",
                  fluidRow(
                    div(
                      bdutilities.app::mod_citation_ui("citation_ui_1", "bddwc.app")
                    )
                  ))
        )
      )
    ))
  
}

#' @import shiny shinyjs golem
golem_add_external_resources <- function() {
  addResourcePath('www', system.file('app/www', package = 'bddwc.app'))
  
  tags$head(
    # golem::js(),
    golem::favicon(ico = "www/bddwc_favicon_4.png"),
    shinyjs::useShinyjs(),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "www/input.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
  )
}
