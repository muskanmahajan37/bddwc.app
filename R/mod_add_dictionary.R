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
          div(class = "secondaryHeaders", h3("Option 01: From bddwc - Cached Darwin Cloud")),
         
         p("A version of Kurator project is stored and used.")
          ,
          
          
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("queryDatabase"), "Use Cached Dictionary", icon("download"))
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
            actionButton(ns("queryDatabase"), "Update and Use", icon("download"))
          )
        ),
        
        #3
        tabPanel(
          "Edit",
          div(class = "secondaryHeaders", h3("Option 03: Edit Darwin Cloud Dictionary")),
          
          p("Manually Edit Darwin Cloud Dictionary and save before using. Use the pane on right to edit and click below to continue.")          ,
          
          
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("queryDatabase"), "Use Edited Dictionary", icon("download"))
          )
        ),
        
        # 4
        tabPanel(
          "New",
          div(class = "secondaryHeaders", h3("Option 04: Create a New Dictionary")),
          
          p("Create a New Dictionary from Scratch. You have all the control. Create a simple dictionary for your need.")
          ,
          
          
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
          div(class = "secondaryHeaders", h3("Option 05: Upload Dictionary From Local Disk")),
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
        
        # ------------- End of Local Disk Module -------------------
        
        
      )
      
    ),
    
    # ------------- Map / Table Module -------------------
    column(8,
           # class = "upload_main",
           tabsetPanel(
             type = "tabs",
             tabPanel("Dictionary View",
                      h3("Viewing Darwinizing Dictionary"),
                      DT::dataTableOutput(ns("dictionaryView"))),
             tabPanel("Edit View",
                      
                      
                      div(class = ns("text"), 
                          h3("Editing Darwinizing Dictionary"),
                          actionButton(ns("queryDatabase"), "", icon("save"), class = "activeButton"),
                          actionButton(ns("queryDatabase"), "", icon("download"), class = "activeButton"),
                          actionButton(ns("queryDatabase"), "", icon("refresh"), class = "activeButton"),
                          # HTML(
                          #   paste(
                          #     '<textarea id="widgEditor" class="widgEditor">',
                          #     paste(capture.output(
                          #       write.table(
                          #         bdDwC:::data_darwin_cloud$data,
                          #         sep = '\t',
                          #         quote = F,
                          #         row.names = F
                          #       )
                          #     ), collapse = "\n "),
                          #     '</textarea>'
                          #   )
                          # )
                          textAreaInput(
                            "text",
                            label = "",
                            value = paste(capture.output(
                              write.table(
                                bdDwC:::data_darwin_cloud$data,
                                sep = '\t',
                                quote = F,
                                row.names = F
                              )
                            ), collapse = "\n ")
                          )
                          ))
           ))
    
    # ------------- End of Map/Table Module -------------------
  ))
}

# Module Server

#' @rdname mod_add_dictionary
#' @export
#' @keywords internal
#' @import bdDwC
mod_add_dictionary_server <- function(input, output, session) {
  ns <- session$ns
  
  output$dictionaryView <- DT::renderDataTable(DT::datatable({
    bdDwC:::data_darwin_cloud$data
  }, options = list(scrollX = TRUE)))
}

## To be copied in the UI
# mod_add_dictionary_ui("add_dictionary_ui_1")

## To be copied in the server
# callModule(mod_add_dictionary_server, "add_dictionary_ui_1")
