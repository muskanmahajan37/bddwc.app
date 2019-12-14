# bddwc Utility Functions

#' @title   get representation of bddwc dictionary that can be edited by hand
#' @description  Get text version of dictionary to show in edit tab.
#'
#' @param dataframe bddwc dictionary
#' 
#' @importFrom utils capture.output write.table
#'
#' @keywords internal
#' @export
get_edit_string <- function(dataframe){
    return(paste(capture.output(
        write.table(
            dataframe,
            sep = '\t',
            quote = F,
            row.names = F
        )
    ), collapse = "\n "))
}