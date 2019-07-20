#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
    
    inputData <- callModule(mod_add_data_server, "bdFileInput")
    
    callModule(mod_add_dictionary_server, "bdDictionaryInput")
    
    callModule(mod_darwinizer_server, "bdDarwinizer")
    
    
}
