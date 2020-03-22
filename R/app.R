library(bddwc.app)

shiny::shinyApp(ui = bddwc.app::app_ui(), server = bddwc.app:::app_server)