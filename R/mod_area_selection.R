# R/mod_area_selection.R
#' Area Selection Module
#'
#' @description A module to handle area selection
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_areaSelectionUI <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      title = "Geographic Selection",
      status = "primary", solidHeader = TRUE,
      width = NULL,
      radioGroupButtons(
        inputId = ns("area_se"),
        label = "Choose the method to select geographic area",
        choices = c("State", "HUC8", "BBox"),
        selected = "State"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'HUC8' || input['%s'] == 'BBox'", ns("area_se"), ns("area_se")),
        checkboxInput(ns("state_out_bound"),
                      label = "Including sites outside the state boundary but within HUC8 intersecting with the state boundary")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'State'", ns("area_se")),
        fluidRow(
          column(
            width = 8,
            selectizeInput(ns("State_se"), "Select state",
                           choices = c("Colorado", "Montana",
                                       "North Dakota", "South Dakota",
                                       "Utah", "Wyoming"),
                           multiple = FALSE)
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'HUC8'", ns("area_se")),
        fluidRow(
          column(
            width = 8,
            strong("Select the HUC8 based on the map or the dropdown menu"),
            mapUI(ns("HUC8map"))
          ),
          column(
            width = 8,
            selectizeInput(ns("HUC_se"), "HUC8 dropdown menu",
                           choices = NULL, multiple = TRUE,
                           width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'BBox'", ns("area_se")),
        fluidRow(
          column(
            width = 6,
            strong("Provide the coordinates by drawing a rectangle on the map or type in the values"),
            mapUI(ns("BBox_map"))
          ),
          column(
            width = 6,
            fluidRow(
              column(
                width = 6,
                numericInput(ns("bb_W"), "West bound", value = -117.65217,
                             min = -117.65217, max = -102.11928)
              ),
              column(
                width = 6,
                numericInput(ns("bb_E"), "East bound", value = -102.11928,
                             min = -117.65217, max = -102.11928)
              )
            ),
            fluidRow(
              column(
                width = 6,
                numericInput(ns("bb_N"), "North bound", value = 50.58677,
                             min = 40.37306, max = 50.58677)
              ),
              column(
                width = 6,
                numericInput(ns("bb_S"), "South bound", value = 40.37306,
                             min = 40.37306, max = 50.58677)
              )
            )
          )
        )
      )
    )
  )
}

#' Area Selection Server Function
#'
#' @noRd

mod_areaSelectionServer <- function(id, Region8_simple, Region8_out_simple, HUC8_label, HUC8_out_label, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store selection
    area_info <- reactiveValues(
      type = "State",
      value = NULL,
      bounds = NULL
    )

    # Update area type on selection change
    observe({
      area_info$type <- input$area_se
    })

    # Toggle input states based on selection
    observeEvent(input$area_se, {
      toggleState(id = ns("State_se"), condition = input$area_se == "State")
      toggleState(id = ns("HUC_se"), condition = input$area_se == "HUC8")
      toggleState(id = ns("bb_N"), condition = input$area_se == "BBox")
      toggleState(id = ns("bb_S"), condition = input$area_se == "BBox")
      toggleState(id = ns("bb_W"), condition = input$area_se == "BBox")
      toggleState(id = ns("bb_E"), condition = input$area_se == "BBox")
    })

    # Reset state_out_bound when state is selected
    observe({
      req(input$area_se)
      if (input$area_se == "State") {
        updateCheckboxInput(session, "state_out_bound", value = FALSE)
      }
    })

    # HUC8 data selection based on state boundary option
    HUC8_dat <- reactive({
      if (!input$state_out_bound) {
        return(Region8_simple)
      } else {
        return(Region8_out_simple)
      }
    })

    # Update HUC8 list based on state boundary option
    observe({
      if (!input$state_out_bound) {
        updateSelectizeInput(session, "HUC_se", choices = HUC8_label)
      } else {
        updateSelectizeInput(session, "HUC_se", choices = HUC8_out_label)
      }
    })

    # Initialize map modules
    huc_map_events <- mapServer("HUC8map", "HUC8", data = HUC8_dat)
    bbox_map_events <- mapServer("BBox_map", "BBox")

    # Update HUC selection based on map clicks
    observe({
      req(huc_map_events$selected)
      updateSelectizeInput(session, "HUC_se", selected = huc_map_events$selected)
    })

    # Update bounding box inputs based on map drawing
    observe({
      req(bbox_map_events$bounds)
      updateNumericInput(session, "bb_W", value = bbox_map_events$bounds$west)
      updateNumericInput(session, "bb_S", value = bbox_map_events$bounds$south)
      updateNumericInput(session, "bb_E", value = bbox_map_events$bounds$east)
      updateNumericInput(session, "bb_N", value = bbox_map_events$bounds$north)
    })

    # Debounce bounding box inputs to reduce validation frequency
    bb_W_debounce <- reactive(input$bb_W) %>% debounce(2000)
    bb_S_debounce <- reactive(input$bb_S) %>% debounce(2000)
    bb_E_debounce <- reactive(input$bb_E) %>% debounce(2000)
    bb_N_debounce <- reactive(input$bb_N) %>% debounce(2000)

    # Validate bounding box coordinates
    observe({
      req(input$bb_N, input$bb_S, input$bb_W, input$bb_E)

      # Check bounds
      if (input$bb_W < -117.65217 || input$bb_W > -102.11928) {
        shinyalert(text = "The west bound is out of range. Please adjust the coordinates.",
                   type = "error")
      }
      if (input$bb_E < -117.65217 || input$bb_E > -102.11928) {
        shinyalert(text = "The east bound is out of range. Please adjust the coordinates.",
                   type = "error")
      }
      if (input$bb_N < 40.37306 || input$bb_N > 50.58677) {
        shinyalert(text = "The north bound is out of range. Please adjust the coordinates.",
                   type = "error")
      }
      if (input$bb_S < 40.37306 || input$bb_S > 50.58677) {
        shinyalert(text = "The south bound is out of range. Please adjust the coordinates.",
                   type = "error")
      }

      # Check relationships
      if (input$bb_W >= input$bb_E) {
        shinyalert(text = "The longitude of west bound should not be larger or equal to the east bound.
                 Please provide a valid longitude entry.",
                   type = "error")
      }
      if (input$bb_S >= input$bb_N) {
        shinyalert(text = "The latitude of south bound should not be larger or equal to the north bound.
                 Please provide a valid latitude entry.",
                   type = "error")
      }
    }) %>% bindEvent(bb_W_debounce(), bb_S_debounce(), bb_E_debounce(), bb_N_debounce())

    # Update area_info based on selection type
    observe({
      if (input$area_se == "State") {
        area_info$value <- input$State_se
      } else if (input$area_se == "HUC8") {
        area_info$value <- input$HUC_se
      } else if (input$area_se == "BBox") {
        area_info$bounds <- list(
          west = input$bb_W,
          south = input$bb_S,
          east = input$bb_E,
          north = input$bb_N
        )
      }
    })

    # Location validity indicator
    location_valid <- reactive({
      if (input$area_se == "State") {
        return(!is.null(input$State_se))
      } else if (input$area_se == "HUC8") {
        return(length(input$HUC_se) > 0)
      } else if (input$area_se == "BBox") {
        req(input$bb_W, input$bb_S, input$bb_E, input$bb_N)
        return(
          input$bb_W < input$bb_E &&
            input$bb_S < input$bb_N &&
            input$bb_W >= -117.65217 && input$bb_W <= -102.11928 &&
            input$bb_E >= -117.65217 && input$bb_E <= -102.11928 &&
            input$bb_N >= 40.37306 && input$bb_N <= 50.58677 &&
            input$bb_S >= 40.37306 && input$bb_S <= 50.58677
        )
      }
      return(FALSE)
    })

    # Handle reset
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        updateRadioGroupButtons(session, "area_se", selected = "State")
        updateCheckboxInput(session, "state_out_bound", value = FALSE)
        updateSelectizeInput(session, "State_se", selected = character(0))
        updateSelectizeInput(session, "HUC_se", selected = character(0))
        updateNumericInput(session, "bb_W", value = -117.65217)
        updateNumericInput(session, "bb_E", value = -102.11928)
        updateNumericInput(session, "bb_N", value = 50.58677)
        updateNumericInput(session, "bb_S", value = 40.37306)
      })
    }

    # Return the reactive area_info and validity status
    return(list(
      info = reactive({
        list(
          type = area_info$type,
          value = if (area_info$type == "BBox") area_info$bounds else area_info$value,
          out_bounds = input$state_out_bound
        )
      }),
      valid = location_valid,
      HUC8_data = HUC8_dat
    ))
  })
}
