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

# Source UI-related module files
source("modules/map_module.R")  # For UI functions
source("modules/area_selection_module.R")  # For UI functions
source("modules/parameter_selection_module.R")  # For UI functions
source("modules/data_download_module.R")  # For UI functions
source("modules/data_summary_module.R")  # For UI functions
source("modules/download_handlers_module.R")  # For UI functions

# Set the upload size to 5MB
mb_limit <- 10000
options(shiny.maxRequestSize = mb_limit * 1024^2)

# UI Definition
ui <- function(request) {
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
}