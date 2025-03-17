#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Reset trigger
  reset_trigger <- reactiveVal(0)

  # Initialize modules
  area_selection <- areaSelectionServer(
    "area",
    Region8_simple,
    Region8_out_simple,
    HUC8_label,
    HUC8_out_label,
    reset_trigger
  )

  param_selection <- parameterSelectionServer(
    "params",
    parameter_names,
    reset_trigger
  )

  # Initialize data download module
  download_results <- dataDownloadServer(
    "download",
    area_selection$info,
    area_selection$valid,
    area_selection$HUC8_data,
    param_selection$info,
    param_selection$valid,
    parameter_reference_table,
    get_par,
    WQP_summary_template,
    TADA_download_temp
  )

  # Process data with TADA when available
  processed_data <- tadaProcessingServer("tada_process", download_results)

  # Conditionally show data summary UI when data is available
  observe({
    req(processed_data$WQP_dat2)

    output$summary_section <- renderUI({
      dataSummaryUI("data_summary")
    })

    # Initialize data summary module
    dataSummaryServer("data_summary", processed_data, count_fun)
  })

  # Initialize download handlers
  downloadHandlersServer("download_handlers", processed_data, reset_trigger)

  # Show/hide download buttons based on data availability
  observe({
    if (!is.null(processed_data$WQP_dat2)) {
      shinyjs::show(id = "download_handlers-data_download")
    } else {
      shinyjs::hide(id = "download_handlers-data_download")
    }

    if (!is.null(processed_data$WQP_QC)) {
      shinyjs::show(id = "download_handlers-data_download_QC")
    } else {
      shinyjs::hide(id = "download_handlers-data_download_QC")
    }
  })

  # Add an observer to reset processed data when reset_trigger changes
  observeEvent(reset_trigger(), {
    # Skip the initial trigger (when the app starts)
    if (reset_trigger() > 0) {
      message("Reset triggered in main server. Reset value: ", reset_trigger())

      # Clear processed data
      processed_data$WQP_dat2 <- NULL
      processed_data$WQP_QC <- NULL

      # Hide the summary section
      output$summary_section <- renderUI({ NULL })

      # Hide download buttons
      shinyjs::hide(id = "download_handlers-data_download")
      shinyjs::hide(id = "download_handlers-data_download_QC")
    }
  })

  # Ensure sink is reset when app stops
  session$onSessionEnded(function() {
    while (sink.number() > 0) {
      sink(NULL)
    }
  })
}
