# R/mod_parameter_selection.R

#' Parameter Selection Module
#'
#' @description A module to select parameters
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

parameterSelectionUI <- function(id, parameter_names = parameter_names) {
  ns <- NS(id)

  box(
    title = "Parameter Input",
    status = "primary", solidHeader = TRUE,
    width = NULL,
    column(
      width = 8,
      selectInput(ns("download_se"),
                  label = "Choose the data type to download",
                  choices = c("Characteristic names",
                              "Organic characteristic group",
                              "All data"),
                  selected = "Characteristic names"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Characteristic names'", ns("download_se")),
        awesomeCheckboxGroup(ns("par_group_se"), "Choose the parameter group",
                             choices = c("Metal", "Nutrient", "Pestcide", "Other",
                                         "Oil & Gas", "All"),
                             inline = TRUE,
                             status = "primary"),
        multiInput(ns("par_se"), "Select the parameter to download",
                   choiceNames = parameter_names$Group_Name,
                   choiceValues = parameter_names$Standard_Name)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Organic characteristic group'", ns("download_se")),
        selectizeInput(ns("CharGroup_se"), "Select the organic group",
                       choices = c("Organics, BDEs", "Organics, Other",
                                   "Organics, PCBs", "Organics, Pesticide",
                                   "PFAS,Perfluorinated Alkyl Substance",
                                   "PFOA, Perfluorooctanoic Acid"),
                       multiple = TRUE)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'All data'", ns("download_se")),
        selectizeInput(ns("sitetype_se"), "Select site type",
                       choices = c("Lake, Reservoir, Impoundment", "Stream"),
                       multiple = TRUE,
                       selected = "Stream")
      ),
      dateRangeInput(ns("par_date"), "Select the date range",
                     start = "2000-01-01",
                     min = "1900-01-01",
                     end = Sys.Date(),
                     max = Sys.Date(),
                     width = "100%")
    )
  )
}

#' Data download handler module
#'
#' @noRd

parameterSelectionServer <- function(id, parameter_names, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    # Update parameter list based on group selection
    par_value <- reactiveValues(variable = NULL)

    observe({
      if ("All" %in% input$par_group_se) {
        variable <- parameter_names$Standard_Name
      } else if (length(input$par_group_se) > 0) {
        group_par <- parameter_names %>%
          filter(Group %in% input$par_group_se)
        variable <- group_par$Standard_Name
      } else {
        variable <- character(0)
      }
      par_value$variable <- variable
    })

    observe({
      updateMultiInput(session, "par_se",
                       selected = par_value$variable)
    })

    # Parameter validity check
    param_valid <- reactive({
      if (input$download_se == "Characteristic names") {
        return(length(input$par_se) > 0)
      } else if (input$download_se == "Organic characteristic group") {
        return(length(input$CharGroup_se) > 0)
      }
      return(TRUE) # All data option is always valid
    })

    # Handle reset
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        updateSelectInput(session, "download_se", selected = "Characteristic names")
        updateCheckboxGroupInput(session, "par_group_se", selected = character(0))
        updateMultiInput(session, "par_se", selected = character(0))
        updateSelectizeInput(session, "CharGroup_se", selected = character(0))
        updateSelectizeInput(session, "sitetype_se", selected = "Stream")
        updateDateRangeInput(session, "par_date",
                             start = "2000-01-01",
                             end = Sys.Date())
      })
    }

    # Return parameter information and validity
    return(list(
      info = reactive({
        list(
          download_type = input$download_se,
          parameters = if (input$download_se == "Characteristic names") input$par_se else NULL,
          char_group = if (input$download_se == "Organic characteristic group") input$CharGroup_se else NULL,
          site_type = if (input$download_se != "All data") input$sitetype_se else NULL,
          date_range = input$par_date
        )
      }),
      valid = param_valid
    ))
  })
}
