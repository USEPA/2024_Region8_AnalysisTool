# R/mod_data_summary.R

#' Data summary Module
#'
#' @description A module to summarize the data
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

dataSummaryUI <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      title = "Data Summary",
      status = "warning", solidHeader = TRUE,
      width = NULL, collapsible = TRUE,
      fluidRow(
        column(
          width = 6,
          verbatimTextOutput(ns("Date_Info"), placeholder = TRUE)
        )
      ),
      h2("Clean Data Summary"),
      br(),
      fluidRow(
        column(
          width = 3,
          selectInput(ns("clean_summary"), "Data attribute to summarize",
                      choices = c("Standard Name" = "TADA.CharacteristicName",
                                  "Standard Fraction" = "TADA.ResultSampleFractionText",
                                  "Censored Data" = "TADA.CensoredData.Flag"),
                      multiple = TRUE)
        ),
        column(
          width = 9,
          DTOutput(ns("clean_summary_table"))
        )
      ),
      fluidRow(
        column(
          width = 6,
          verbatimTextOutput(ns("Date_Info2"), placeholder = TRUE)
        )
      )
    )
  )
}

#' Data Summary Server Function
#'
#' @noRd

dataSummaryServer <- function(id, processed_data, count_fun) {
  moduleServer(id, function(input, output, session) {
    # Display basic data information
    output$Date_Info <- renderText({
      req(processed_data$WQP_dat2)

      if (is.null(processed_data$sample_size) || processed_data$sample_size == 0) {
        paste0("Data are downloaded at ", processed_data$download_date, ".\n",
               "There are ", processed_data$sample_size, " records from ", processed_data$site_number, " sites.\n")
      } else {
        paste0("Data are downloaded at ", processed_data$download_date, ".\n",
               "The date range is from ", processed_data$min_date, " to ", processed_data$max_date, ".\n",
               "There are ", processed_data$sample_size, " records from ", processed_data$site_number, " sites.\n")
      }
    })

    # Display data info after cleaning
    output$Date_Info2 <- renderText({
      req(processed_data$WQP_dat3, nrow(processed_data$WQP_dat3) > 0)

      paste0("There are ", processed_data$sample_size2, " records from ",
             processed_data$site_number2, " sites after data cleaning.")
    })

    # Generate summary table
    output$clean_summary_table <- renderDT({
      req(processed_data$WQP_dat2, input$clean_summary)

      tt <- count_fun(processed_data$WQP_dat2, input$clean_summary)
      datatable(tt, options = list(scrollX = TRUE)) %>%
        formatPercentage(columns = "Percent (%)", digits = 2)
    })
  })
}
