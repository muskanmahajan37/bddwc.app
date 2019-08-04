# Module UI

#' @title   mod_add_dictionary_ui and mod_add_dictionary_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_add_dictionary
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_add_dictionary_ui <- function(id) {
  ns <- NS(id)
  tagList(column(
    12,
    h1("Add Standardizing Dictionary"),
    column(
      4,
      # class = "upload_side",
      tabsetPanel(
        type = "tabs",
        
        # 1
        tabPanel(
          "Cached",
          div(class = "secondaryHeaders", h3(
            "Option 01: From bddwc - Cached Darwin Cloud"
          )),
          p("A version of Kurator project is stored and used."),
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("cacheButton"), "Use Cached Dictionary", icon("download"))
          )
        ),
        
        # 2
        tabPanel(
          "Cloud",
          div(class = "secondaryHeaders", h3("Option 02: From Darwin Cloud Server")),
          p("Get update of the Darwin cloud and use.") ,
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("updateCache"), "Update and Use", icon("download"))
          )
        ),
        
        #3
        tabPanel(
          "Edit",
          div(class = "secondaryHeaders", h3("Option 03: Edit Darwin Cloud Dictionary")),
          p(
            "Manually Edit Darwin Cloud Dictionary and save before using. 
            Use the pane on right to edit and click below to continue."
          )          ,
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("edit_cloud"), "Use Edited Dictionary", icon("download"))
          )
        ),
        
        # 4
        tabPanel(
          "New",
          div(class = "secondaryHeaders", h3("Option 04: Create a New Dictionary")),
          p(
            "Create a New Dictionary from Scratch. You have all the control. Create a simple dictionary for your need."
          ),
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("queryDatabase"), "Use New Dictionary", icon("download"))
          )
        ),
        # ------------- End of DB Module -------------------
        
        # ------------- Local Disk Module -------------------
        tabPanel(
          "Upload",
          div(class = "secondaryHeaders", h3(
            "Option 05: Upload Dictionary From Local Disk"
          )),
          div(
            id = ns("inputFileDiv"),
            class = "activeButton",
            fileInput(
              ns("inputFile"),
              label = h3("CSV / DWCA ZIP file input"),
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv",
                ".zip",
                "application/zip"
              )
            )
          )
        )
      )
    ),
    
    # ------------- Map / Table Module -------------------
    column(8,
           tabsetPanel(
             id = ns("editViewTab"),
             type = "tabs",
             tabPanel(
               title = "Dictionary View",
               value = "table",
               h3("Viewing Darwinizing Dictionary"),
               DT::dataTableOutput(ns("dictionaryView"))
             ),
             tabPanel(
               title = "Edit View",
               value = "edit",
               div(
                 class = ns("text"),
                 h3("Editing Darwinizing Dictionary"),
                 actionButton(ns("queryDatabase"), "", icon("save"), class = "activeButton"),
                 actionButton(ns("queryDatabase"), "", icon("download"), class = "activeButton"),
                 actionButton(ns("queryDatabase"), "", icon("refresh"), class = "activeButton"),
                 textAreaInput(ns("edit_text"),
                               label = "",
                               value = "")
               )
             )
           ))
    # ------------- End of Map/Table Module -------------------
  ))
}

# Module Server

#' @rdname mod_add_dictionary
#' @export
#' @keywords internal
#' @import bdDwC
mod_add_dictionary_server <-
  function(input, output, session, next_button_id = "dataToConfigureDiv") {
    ns <- session$ns
    returnData <- data.frame()
    
    observeEvent(input$cacheButton, {
      returnData <<- bdDwC:::data_darwin_cloud$data
      updateTextInput(session,
                      "edit_text",
                      value = bddwc.app::get_edit_string(returnData))
      
      shinyjs::runjs(code = paste(
        '$("#',
        next_button_id,
        '").addClass("completedButton");',
        sep = ""
      ))
      shinyjs::runjs(code = paste(
        '$("#',
        ns("queryDatabaseDiv"),
        '").removeClass("activeButton");',
        sep = ""
      ))
    })
    
    observeEvent(input$updateCache, {
      returnData <<- bdDwC::download_cloud_data()
      updateTextInput(session,
                      "edit_text",
                      value = bddwc.app::get_edit_string(returnData))
      
      shinyjs::runjs(code = paste(
        '$("#',
        next_button_id,
        '").addClass("completedButton");',
        sep = ""
      ))
      shinyjs::runjs(code = paste(
        '$("#',
        ns("queryDatabaseDiv"),
        '").removeClass("activeButton");',
        sep = ""
      ))
    })
    
    observeEvent(input$edit_cloud, {
      updateTabsetPanel(session, "editViewTab", selected = "edit")
      updateTextInput(session,
                      "edit_text",
                      value = bddwc.app::get_edit_string(returnData))
    })
    
    output$dictionaryView <- DT::renderDataTable({
      input$cacheButton
      input$updateCache
      
      return(returnData)
    }, options = list(scrollX = TRUE))

    
    returnDataReact <- reactive({
      # Input actions that need to trigger new dataframe return
      input$inputFile
      input$queryDatabase
      returnData
    })
    return(returnDataReact)
  }



## To be copied in the UI
# mod_add_dictionary_ui("add_dictionary_ui_1")

## To be copied in the server
# callModule(mod_add_dictionary_server, "add_dictionary_ui_1")
