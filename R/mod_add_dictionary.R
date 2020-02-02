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
        id = ns("addDictPanel"),
        type = "tabs",
        
        # 1
        tabPanel(
          "Darwin Cloud",
          div(class = "secondaryHeaders", h3("Option 01: From Darwin Cloud")),
          p("A version of Kurator project is stored and used."),
          div(
            id = ns("queryDatabaseDiv"),
            class = "activeButton",
            actionButton(ns("cacheButton"), "Use Cached Dictionary", icon("history")),
            actionButton(ns("updateCache"), "Download New Version", icon("download"))
          )
        ),
        
        #2
        tabPanel(
          "Edit",
          div(class = "secondaryHeaders", h3("Option 02: Edit Darwin Cloud Dictionary")),
          p(
            "Manually Edit Darwin Cloud Dictionary and save before using.
            Use the pane on right to edit and click below to continue."
          )
        ),
        
        #3
        tabPanel(
          "Upload",
          div(class = "secondaryHeaders", h3(
            "Option 03: Upload Dictionary From Local Disk"
          )),
          div(
            id = ns("inputFileDiv"),
            class = "activeButton",
            fileInput(
              ns("inputFile"),
              label = h3("CSV Dictionary input"),
              accept = c(".csv")
            ),
            helpText("The file should have headers 'fieldname' and 'standard'.")
          )
        )
      )
    ),
    
    # ------------- Map / Table Module -------------------
    column(8,
           tabsetPanel(
             type = "tabs",
             id = "viewPanels",
             
             # 1
             tabPanel(
               "Dictionary",
               
               br(),
               
               conditionalPanel(
                 paste0("input['", ns("addDictPanel"), "'] != 'Edit'"),
                 tabPanel(
                   title = "Dictionary View",
                   value = "table",
                   h3("Viewing Darwinizing Dictionary"),
                   DT::dataTableOutput(ns("dictionaryView"))
                 )
               ),
               
               conditionalPanel(
                 paste0("input['", ns("addDictPanel"), "'] == 'Edit'"),
                 tabPanel(
                   title = "Edit View",
                   value = "edit",
                   div(
                     class = ns("text"),
                     
                     actionButton(ns("save"), "", icon("save"), class = "activeButton"),
                     actionButton(ns("reset"), "", icon("refresh"), class = "activeButton"),
                     h3("Editing in Table"),
                     
                     DT::dataTableOutput(ns("dictionaryEditView")),
                     
                     h3("Editing in Text"),
                     textAreaInput(ns("edit_text"),
                                   label = "",
                                   value = "")
                   )
                 )
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
#' @import bdDwC DT
#' @importFrom utils read.csv
mod_add_dictionary_server <-
  function(input, output, session, next_button_id = "dataToConfigureDiv") {
    ns <- session$ns
    returnData <- data.frame()
    editingData <- data.frame()
    
    observeEvent(input$cacheButton, {
      returnData <<- bdDwC:::data_darwin_cloud$data
      
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
      
      showNotification(
        paste(
          "Downloaded Darwin Cloud from
          https://raw.githubusercontent.com/kurator-org/kurator-validation/master/packages/kurator_dwca/data/vocabularies/darwin_cloud.txt",
          collapse = " "
        ),
        duration = 6
      )
      
    })
    
    observeEvent(input$addDictPanel, {
      if (input$addDictPanel == "Edit") {
        data_temp <- data.frame()
        
        if (length(editingData) == 1) {
          data_temp <- bdDwC::download_cloud_data()
        } else {
          data_temp <- editingData
        }
        
        updateTextInput(session,
                        "edit_text",
                        value = bddwc.app::get_edit_string(data_temp))
      }
    })
    
    observeEvent(input$save, {
      returnData <<- editingData
      
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
    
    observeEvent(input$reset, {
      editingData <<- bdDwC::download_cloud_data()
      
      updateTextInput(session,
                      "edit_text",
                      value = bddwc.app::get_edit_string(editingData))
    })
    
    
    
    observeEvent(input$inputFile, {
      if (input$inputFile$type == "text/csv"){
        returnData <<- read.csv(input$inputFile$datapath)
      } else if (input$inputFile$type == "text/plain"){
        returnData <<- data.table::fread(input$inputFile$datapath, header = T, sep = "\t", data.table = F)
      } else {
        showNotification("Incorrect Format")
      }
    })
    
    
    
    output$dictionaryView <- DT::renderDataTable({
      input$cacheButton
      input$updateCache
      input$inputFile
      
      return(returnData)
    }, options = list(scrollX = TRUE))
    
    output$dictionaryEditView <- DT::renderDataTable({
      input$addDictPanel
      input$edit_text
      return(editingData)
    }, options = list(scrollX = TRUE), editable = TRUE)
    
    
    proxy <- dataTableProxy('dictionaryEditView')
    observeEvent(input$dictionaryEditView_cell_edit, {
      info <- input$dictionaryEditView_cell_edit
      i <- info$row
      j <- info$col
      v <- info$value
      editingData[i, j] <<- DT::coerceValue(v, editingData[i, j])
      replaceData(proxy, editingData, resetPaging = FALSE)
      updateTextInput(session,
                      "edit_text",
                      value = bddwc.app::get_edit_string(editingData))
    })
    
    observeEvent(input$edit_text, {
      rows <- strsplit(input$edit_text, "\n ", fixed = T)[[1]]
      lists <- strsplit(rows[2:length(rows)], "\t", fixed = T)
      if (length(lists) != 0) {
        df <-
          data.frame(matrix(
            unlist(lists),
            nrow = length(lists),
            byrow = T
          ),
          stringsAsFactors = FALSE)
        colnames(df) <- strsplit(rows[1], "\t", fixed = T)[[1]]
        editingData <<- df
      }
    })
    
    
    returnDataReact <- reactive({
      # Input actions that need to trigger new dataframe return
      input$cacheButton
      input$updateCache
      input$inputFile
      input$save
      
      returnData
    })
    
    
    return(returnDataReact)
  }
