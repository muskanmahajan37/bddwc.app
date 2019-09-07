#' @import shiny
app_server <- function(input, output, session) {
  
  #------------- Data --------------
  data_store <-
    shiny::reactiveValues(
      data_user = data.frame(),
      dictionary = data.frame(),
      data_darwinized = data.frame()
    )
  #------------- Data --------------
  
  
  #------------- Modules --------------
  data_store$data_user <-
    callModule(mod_add_data_server, "bdFileInput", "dataToDictionaryDiv")
  
  data_store$dictionary <- callModule(mod_add_dictionary_server, "bdDictionaryInput", "dictionaryToDarwinDiv")
  
  callModule(mod_darwinizer_server,
             "bdDarwinizer",
             data_store$data_user(), data_store$dictionary)
  
  callModule(mod_citation_server, "citation_ui_1", "bddwc.app")
  #------------- Modules --------------
  
  
  #------------- Events --------------
  observeEvent(input$dataToDictionary, {
    dat <- data_store$data_user
    
    if (length(dat()) == 0) {
      showNotification("Please add data",
                       duration = 6)
    } else {
      updateTabItems(session, "sideBar", "configure")
    }
  })
  
  observeEvent(input$dictionaryToDarwin, {
    dat <- data_store$dictionary
    
    if (length(dat()) == 0) {
      showNotification("Please add dictionary",
                       duration = 6)
    } else {
      updateTabItems(session, "sideBar", "darwinize")
    }
  })
  #------------- Events --------------
}
