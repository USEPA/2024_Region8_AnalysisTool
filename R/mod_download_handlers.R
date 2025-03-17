# R/mod_download_handlers.R

#' Data Download Handler Module
#'
#' @description A module for download handler
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

downloadHandlersUI <- function(id) {
  ns <- NS(id)

  box(
    title = "Save Data",
    status = "warning", solidHeader = TRUE,
    width = NULL, collapsible = TRUE,
    downloadButton(ns("data_download"), "Save data"),
    downloadButton(ns("data_download_QC"), "Save QC data"),
    actionButton(ns("reset"), "Reset")  # Changed from Reset to reset for consistency
  )
}

#' Data download handler module
#'
#' @noRd

downloadHandlersServer <- function(id, processed_data, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    # Download handler for raw data
    output$data_download <- downloadHandler(
      filename = function() {
        paste0("WQP_data_", Sys.Date(), ".csv")
      },
      content = function(con) {
        showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
        fwrite(processed_data$WQP_dat2, file = con, na = "", bom = TRUE)
        removeModal()
      }
    )

    # Download handler for QC data
    output$data_download_QC <- downloadHandler(
      filename = function() {
        paste0("WQP_data_", Sys.Date(), "_QC.csv")
      },
      content = function(con) {
        showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
        fwrite(processed_data$WQP_QC, file = con, na = "", bom = TRUE)
        removeModal()
      }
    )

    # Reset button triggers reset event
    observeEvent(input$reset, {
      # Add debug message
      message("Reset button clicked, current value: ", reset_trigger())

      # Update the reset trigger
      reset_trigger(reset_trigger() + 1)

      # Add confirmation message
      showNotification("Application reset complete", type = "message")

      # Add debug message after update
      message("Reset trigger updated to: ", reset_trigger())
    })
  })
}
