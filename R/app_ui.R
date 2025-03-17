#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# ui.R
library(tidyverse)
library(lubridate)
library(collapse)
library(data.table)
library(openxlsx)
library(dataRetrieval)
library(shiny)
library(shinybusy)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(DT)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(zip)
library(EPATADA)
library(tigris)
library(tidycensus)
library(spsComps)
library(scales)

# Set the upload size to 5MB
mb_limit <- 10000
options(shiny.maxRequestSize = mb_limit * 1024^2)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      # The header panel
      header = dashboardHeader(title = "WQP Data Tool"),
      # The sidebar panel
      sidebar = dashboardSidebar(
        # Use Shiny Busy
        add_busy_spinner(spin = "circle", position = "top-right"),
        useSweetAlert(),

        # Sidebar menu
        sidebarMenu(
          id = "tabs",
          menuItem(
            text = "Data Download",
            tabName = "Download",
            icon = icon("pen")
          )
        )
      ),
      body = dashboardBody(
        # Call shinyCatch
        spsDepend("shinyCatch"),
        # Use shinyJS
        useShinyjs(),

        # Main content tabs
        tabItems(
          tabItem(
            tabName = "Download",
            h2("Data Download"),
            fluidRow(
              column(width = 12, areaSelectionUI("area"))
            ),
            fluidRow(
              column(width = 12, parameterSelectionUI("params", parameter_names))
            ),
            fluidRow(
              column(width = 12, dataDownloadUI("download"))
            ),
            uiOutput("summary_section"),
            fluidRow(
              column(width = 12, downloadHandlersUI("download_handlers"))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Region8WQP"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
