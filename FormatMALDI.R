#' Importing Excel Document Function
#'
#' This Function converts multi sheet excel doc to single data-frame with m/z,intensity, 
#' @param .xls file
#' @keywords import
#' @export
#' @examples
#' importWorksheets() 

#load XLConnect package to read in Excel files
importWorksheets <- function(file) {
  # filename: name of Excel file
  workbook <- loadWorkbook(file)
  sheet_names <- getSheets(workbook)
  names(sheet_names) <- sheet_names
  sheet_list <- lapply(sheet_names, function(.sheet){
    readWorksheet(object=workbook, .sheet)})
}