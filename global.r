# global.R - This file is loaded before both ui.R and server.R

# Load libraries that are needed by both UI and server
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

# Source module files (both UI and server components)
source("modules/map_module.R")
source("modules/area_selection_module.R")
source("modules/parameter_selection_module.R")
source("modules/data_download_module.R")
source("modules/tada_processing_module.R")
source("modules/data_summary_module.R")
source("modules/download_handlers_module.R")

# Load the data (needed by the server)
load("DataInput/InputData38.RData")

# Load the functions
source("scripts/Main_helper_functions.R")
source("scripts/Map_functions.R")

# Set the upload size to 5MB
mb_limit <- 10000
options(shiny.maxRequestSize = mb_limit * 1024^2)