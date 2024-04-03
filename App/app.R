### This is a draft version of a tool to download data from the water quality portal

# Load packages
library(tidyverse)
library(lubridate)
library(collapse)
library(data.table)
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
library(openxlsx2)
library(zip)
library(TADA)
library(tigris)
library(tidycensus)

# Load the data
load("DataInput/InputData19.RData")
load("DataInput/SANDS_input15.RData")

# Load the functions
source("Helper_Fun.R")
source("SANDS_fun_App.R")

# Set the upload size to 1000MB
options(shiny.maxRequestSize = 1000 * 1024^2)

### Helper function to add USGS topo map

# Stored USGS map element names
grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo", "USGS Shaded Relief", "Hydrography")

att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")

# Get the base map
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

add_USGS_base <- function(x){
  x <- leaflet::addWMSTiles(x, GetURL("USGSTopo"),
                            group = grp[1], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSImageryOnly"),
                            group = grp[2], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSImageryTopo"),
                            group = grp[3], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSShadedReliefOnly"),
                            group = grp[4], attribution = att, layers = "0")
  
  # Add the tiled overlay for the National Hydrography Dataset to the map widget:
  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
  x <- leaflet::addWMSTiles(x, GetURL("USGSHydroCached"),
                            group = grp[5], options = opt, layers = "0")
  x <- leaflet::hideGroup(x, grp[5])
  
  # Add layer control
  # Add layer controls
  opt2 <- leaflet::layersControlOptions(collapsed = FALSE)
  x <- leaflet::addLayersControl(x, baseGroups = grp[1:4],
                                 overlayGroups = grp[5], options = opt2)
  
  return(x)
}

### UI
ui <- function(request){
  dashboardPage(
    # The header panel
    header = dashboardHeader(title = "WQP Data Tool"),
    # The sidebar panel
    sidebar = dashboardSidebar(
      
      # Use Shiny Busy
      add_busy_spinner(spin = "circle",
                       position = "top-right"
      ),
      useSweetAlert(),
      
      # The sidebar manual
      sidebarMenu(
        id = "tabs",
        # Tab 1: Introduction Page
        menuItem(
          text = "Introduction Page",
          tabName = "Intro",
          icon = icon("map")
        ),
        # Tab 2: Download Page
        menuItem(
          text = "Download Page",
          tabName = "Download",
          icon = icon("pen")
        ),
        # Tab 3: Introduction
        menuItemOutput("Analysis_page")
      )
    ),
    body = dashboardBody(
      
      # A call to the use shinyJS package
      useShinyjs(),
      
      # Set up the contents in the main panel as tabs
      tabItems(
        # Tab 1: Introduction Page
        tabItem(
          # The tab name
          tabName = "Intro",
          # The header
          h2("Water Quality Portal Data Tool"),
          # The first row in Tab 1
          fluidRow(
            # Box 1 in Tab 1
            box(
              title = "", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
              h2("Introduction"),
              p("The Water Quality Portal Data Tool is a tool to download the data from the Water Quality Portal and analyze the data for exceedance, designed for the State of Montana and Wyoming."),
              h3("Data Download"),
              p("To download the data, go to the 'Download Page' by clicking the 'Download Page' button on the side-bar panel'. 
                 The tool offers two formats to organize the downloaded data:"),
              tags$li("Downloading the data as selected individual parameters and save the data as a CSV file with long format."),
              tags$li("Downloading the data as a group of parameters and save the data as an Excel file following the SANDS format."),
              p("Select one of the formats in 'Select the data format'"),
              p("If 'Individual Parameter' is selected, select the parameter names and site types in the following menu on the left.
                If 'SANDS Format' is selected, Select the filter groups in the following menu on the right."),
              p("In the 'Geographic and Date Input' box, select the date range. Choose one of the three methods to select geographic area, then seelct the area. 
                Click the 'Download the data' button to download the data to work space.
                When downloading the data, the tool displays a progress bar to show the progress. 
                When the download is completed, the tool shows a dialog box. Click 'OK' to continue."),
              fluidRow(
                column(width = 12,
                       img(src = "Download_completed.PNG", height = "30%", width = "30%", align = "left"))
              ),
              p("Once the data download is completed, the tool shows summary information in the 'Data Summary' box."),
              p("Click the 'Save Data' button to download the data as a CSV file or an Excel file with 'Individual Parameter' or 'SANDS Format', respectively."), 
              p("Click 'Reset' to reset the inputs."),
              h3("Data Analysis"),
              p("If data are downloaded with the format as the 'Individual Parameter', 
                the 'Analysis Page' provides options to visualize the exceedance information. 
                The analysis can use the data from the 'Download Page' or upload by the users with the same format. 
                Once data are available, select the parameter name, the fraction, 
                the state criteria, the use of criteria, and the details of criteria. 
                The tool would search for the criteria information and compare the measurement with the criteria."),
              p("A table and a map will display the sample size, number of exceedance and percentage of exceedance. 
                Select the site by clicking each row in the table or dot on the map, 
                the measurement and the criteria will be plotted as a time-series plot and a boxplot."),
              p("For hardness-dependent parameter, hardness data are required to calculate the criteria. 
                The criteria are plotted as orange points, while the measurements are plotted as blue points in the time-series plot.
                For parameters with criteria not depending on hardness, the criteria are plotted as an orange line.")
            )
          )
        ),
        # Tab 2: Download Page
        tabItem(
          # The tab name
          tabName = "Download",
          h2("Data Download"),
          # The first row in Tab 2
          fluidRow(
            column(
              width = 12,
              box(
                title = "Parameter Input", 
                status = "primary", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                # Select the data format
                radioGroupButtons(
                  inputId = "out_se", label = "Select the data format",
                  choices = c("Individual Parameter", "SANDS Format"), 
                  selected = "Individual Parameter",
                  justified = TRUE,
                  width = "40%"),
                column(
                  width = 6,
                  sidebarPanel(
                    id = "Ind_input",
                    width = 12,
                    h4("Individual Parameter"),
                    awesomeCheckboxGroup(inputId = "par_group_se", label = "Choose the parameter group",
                                         choices = c("Metal", "Nutrient", "Pestcide", "Other", 
                                                     "Oil & Gas", "All"),
                                         inline = TRUE,
                                         status = "primary"),
                    uiOutput("multi_label"),
                    multiInput(inputId = "par_se", label = "Select the parameter to download",
                               choiceNames = parameter_reference_table$Group_Name,
                               choiceValues = parameter_reference_table$Standard_Name),
                    selectizeInput(inputId = "sitetype_se", label = "Select site type",
                                   choices = c("Lake, Reservoir, Impoundment", "Stream"), 
                                   multiple = TRUE,
                                   selected = "Stream")
                  )
                ),
                column(
                  width = 6,
                  sidebarPanel(
                    id = "SANDS_input",
                    width = 12,
                    h4("SANDS Format"),
                    checkboxGroupButtons(inputId = "MT_par_group",
                                         label = "Select the filter groups",
                                         choices = c("Metals (Water Column)" = "metal_w", 
                                                     "Metals (Sediment)" = "metal_s", 
                                                     "Salinity" = "salinity", 
                                                     "Oil & Gas" = "oil_gas", 
                                                     "Nutrients" = "nutrients")),
                  )
                )
              ),
              box(
                title = "Geographic and Date Input", 
                status = "primary", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                dateRangeInput(inputId = "par_date", label = "Select the date range",
                               start = "2000-01-01",
                               min = "1900-01-01",
                               end = Sys.Date(),
                               max = Sys.Date(),
                               width = "40%"),
                radioGroupButtons(inputId = "area_se", label = "Choose the method to select geographic area",
                                  choices = c("State", "HUC8", "BBox"),
                                  selected = "State"),
                conditionalPanel(condition = "input.area_se == 'HUC8' || input.area_se == 'BBox'",
                                 fluidRow(
                                   column(
                                     width = 6,
                                     checkboxInput(inputId = "state_out_bound", 
                                                   label = "Including sites outside the state boundary but within HUC8 intersecting with the state boundary")
                                   )
                                 )
                ),
                conditionalPanel(condition = "input.area_se == 'State'",
                                 fluidRow(
                                   column(
                                     width = 4,
                                     selectizeInput(inputId = "State_se", label = "Select state",
                                                    choices = c("Montana", "Wyoming"), multiple = FALSE)
                                   ),
                                   column(
                                     width = 8
                                   )
                                 )
                ),
                conditionalPanel(condition = "input.area_se == 'HUC8'",
                                 fluidRow(
                                   column(
                                     width = 6,
                                     strong("Select the HUC8 based on the map or the dropdown menu"),
                                     leafletOutput("HUC8map")
                                   ),
                                   column(
                                     width = 6,
                                     selectizeInput(inputId = "HUC_se", label = "HUC8 dropdown menu",
                                                    choices = HUC8_label, multiple = TRUE,
                                                    width = "60%")
                                   )
                                 )
                ),
                conditionalPanel(condition = "input.area_se == 'BBox'",
                                 fluidRow(
                                   column(
                                     width = 6,
                                     strong("Provide the coordinates (latitude and longitude) by drawing a rectangle on 
                                           the map or type in the values"),
                                     leafletOutput("BBox_map")
                                   ),
                                   column(
                                     width = 6,
                                     fluidRow(
                                       id = "BBox_Panel",
                                       column(
                                         width = 3,
                                         br(),
                                         br(),
                                         numericInput(inputId = "bb_W", value = -117.65217, label = "West bound",
                                                      min = -117.65217, max = -102.11928)
                                       ),
                                       column(
                                         width = 3,
                                         numericInput(inputId = "bb_N", value = 50.58677, label = "North bound",
                                                      min = 40.37306, max = 50.58677),
                                         numericInput(inputId = "bb_S", value = 40.37306, label = "Sound bound",
                                                      min = 40.37306, max = 50.58677)
                                       ),
                                       column(
                                         width = 3,
                                         br(),
                                         br(),
                                         numericInput(inputId = "bb_E", value = -102.11928, label = "East bound",
                                                      min = -117.65217, max = -102.11928)
                                       )
                                     )
                                   )
                                 )
                                 
                )
              )
            ),
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Download Data from WQP", 
                status = "primary", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                actionButton(inputId = "par_download", "Download the data"),
                actionButton(inputId = "SANDS_download", "Download the data")
              ) 
            )
          ),
          uiOutput("summary_box"),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Save Data", 
                status = "warning", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                downloadButton(outputId = "data_download", label = "Save Data"),
                downloadButton(outputId = "SANDS_data_download", label = "Save Data"),
                actionButton(inputId = "Reset", "Reset")
              )
            )
          )
        ),
        # Tab 3: Analysis Page
        tabItem(
          # The tab name
          tabName = "Analysis",
          h2("Data Analysis"),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Data Input", 
                status = "primary", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 4,
                    strong("Data Condition:"),
                    verbatimTextOutput(outputId = "file_indicator"),
                    fileInput(inputId = "file", label = "", accept = ".csv"),
                    verbatimTextOutput(outputId = "Date_Info3", placeholder = TRUE),
                    strong("Review all sites"),
                    switchInput(inputId = "all_sites", label = "", value = TRUE),
                    sidebarPanel(id = "site_sidebar",
                      width = 12,
                      fluidRow(
                        column(
                          width = 12,
                          selectizeInput(inputId = "site_ID_se", label = "Site ID dropdown menu",
                                         choices = NULL, multiple = TRUE,
                                         width = "100%")
                        )
                      )
                    ),
                    strong("Review all fractions"),
                    switchInput(inputId = "all_fractions", label = "", value = TRUE),
                    sidebarPanel(id = "site_sidebar2",
                                 width = 12,
                                 fluidRow(
                                   column(
                                     width = 12,
                                     selectizeInput(inputId = "fraction_combined", label = "Select the fractions to combined",
                                                    choices = NULL, multiple = TRUE,
                                                    width = "100%")
                                   )
                                 )
                    ),
                    strong("Criteria Information:"),
                    verbatimTextOutput(outputId = "criteria_info")
                  ),
                  column(
                    width = 8,
                    h2("Parameter Summary Table"),
                    fluidRow(
                      column(
                        width = 12,
                        actionLink(inputId = "type_ex", label = "View the definition of Type")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        DTOutput(outputId = "Par_Select")
                      )
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Monitoring Site Selection Table", 
                status = "warning", solidHeader = TRUE,
                width = 6, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 12,
                    # Slider to filter the exceedance percentage
                    strong("Selected Parameter:"),
                    textOutput(outputId = "parameter_name1"),
                    sliderInput(inputId = "slider_ex", label = "Filter the exceedance percentage (%)",
                                min = 0, max = 100, value = 0, step = 1),
                    DTOutput(outputId = "site_table")
                  )
                )
              ),
              box(
                title = "Map", 
                status = "warning", solidHeader = TRUE,
                width = 6, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 12,
                    strong("Selected Parameter:"),
                    textOutput(outputId = "parameter_name2"),
                    leafletOutput(outputId = "site_map")
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Time series plot", 
                status = "warning", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 3,
                    strong("Log10 transformation on the y-axis:"),
                    switchInput(inputId = "time_log_trans"),
                    checkboxInput(inputId = "time_group",label = "Group sites"),
                    br("Orange dots/lines indicate the criteria.")
                  ),
                  column(
                    width = 9,
                    strong("Selected Parameter:"),
                    textOutput(outputId = "parameter_name3"),
                    plotlyOutput(outputId = "time_plot")
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Box plot", 
                status = "warning", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 3,
                    strong("Log10 transformation on the y-axis:"),
                    switchInput(inputId = "box_log_trans"),
                    checkboxInput(inputId = "box_group",label = "Group sites")
                  ),
                  column(
                    width = 9,
                    strong("Selected Parameter:"),
                    textOutput(outputId = "parameter_name4"),
                    plotlyOutput(outputId = "box_plot")
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Save Data", 
                status = "warning", solidHeader = TRUE,
                width = 12, collapsible = TRUE,
                downloadButton(outputId = "data_download2", label = "Save selected data"),
                actionButton(inputId = "Reset2", "Reset")
              )
            )
          )
        )
      )
    )
  )
}

### Server
server <- function(input, output, session){
  
  ### The Download tab
  
  # Control the parameter selection
  observeEvent(input$out_se, {
    toggle(id = "par_se", condition = input$out_se == "Individual Parameter")
    toggle(id = "SANDS_download", condition = input$out_se == "SANDS Format")
    toggle(id = "par_download", condition = input$out_se == "Individual Parameter")
  })
  
  observe({
    if(input$out_se == "Individual Parameter" & !is.null(reVal$WQP_dat2)){
      shinyjs::show(id = "data_download")
    } else {
      shinyjs::hide(id = "data_download")
    }
  })
  
  observe({
    if(input$out_se == "Individual Parameter" & !is.null(input$Par_Select_rows_selected)){
      shinyjs::show(id = "data_download2")
    } else {
      shinyjs::hide(id = "data_download2")
    }
  })
  
  observe({
    if(input$out_se == "SANDS Format" & !is.null(SANDS_object$summary_temp)){
      shinyjs::show(id = "SANDS_data_download")
    } else {
      shinyjs::hide(id = "SANDS_data_download")
    }
  })
  
  observe({
    toggleState("Ind_input", input$out_se == "Individual Parameter")
  })
  observe({
    toggleState("SANDS_input", input$out_se == "SANDS Format")
  })
  
  # Turn on the Analysis Page if input$out_se == "Individual Parameter"
  output$Analysis_page <- renderMenu({
    req(input$out_se)
    if(input$out_se == "Individual Parameter") {
      menuItem(
        text = "Analysis Page",
        tabName = "Analysis",
        icon = icon("poll")
      )
    } else {
      shinyjs::hide(selector = "a[data-value='Analysis']"
      )
    }
  })
  
  observeEvent(input$out_se, {
    if (input$out_se == "SANDS Format"){
      updateRadioGroupButtons(inputId = "area_se", session = session, 
                              choices = c("HUC8", "BBox"),
                              selected = "HUC")
    } else {
      updateRadioGroupButtons(inputId = "area_se", session = session,
                              choices = c("State", "HUC8", "BBox"),
                              selected = "State")
    }
  })
  
  output$multi_label <- renderUI({
    if (input$out_se %in% "Individual Parameter"){
      return()
    } else {
      HTML(paste0("<b>","Select the parameter to download","</b>"))
    }
  })
  
  # Control the method to select area
  observeEvent(input$area_se, {
    toggleState(id = "State_se", condition = input$area_se == "State")
    toggleState(id = "HUC_se", condition = input$area_se == "HUC8")
    toggleState(id = "bb_N", condition = input$area_se == "BBox")
    toggleState(id = "bb_S", condition = input$area_se == "BBox")
    toggleState(id = "bb_W", condition = input$area_se == "BBox")
    toggleState(id = "bb_E", condition = input$area_se == "BBox")
  })
  
  ### HUC8 map
  
  # Update input$state_out_bound if "input.area_se == 'State'"
  observe({
    req(input$area_se)
    if (input$area_se == "State"){
      updateCheckboxInput(inputId = "state_out_bound", value = FALSE)
    }
  })
  
  # A reactive object to select HUC8 map
  HUC8_dat <- reactive({
    req(input$out_se)
    
    if (input$out_se %in% "Individual Parameter" & !input$state_out_bound){
      return(HUC8_WY_MT_simple)
    } else if (input$out_se %in% "Individual Parameter" & input$state_out_bound){
      return(HUC8_WY_MT_out_simple)
    } else if (input$out_se %in% "SANDS Format" & !input$state_out_bound){
      return(HUC8_MT_simple)
    } else if (input$out_se %in% "SANDS Format" & input$state_out_bound){
      return(HUC8_MT_out_simple)
    }
  })
  
  # Update the HUC8 list
  observeEvent(input$out_se, {
    if (input$out_se %in% "Individual Parameter" & !input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_label)
    } else if (input$out_se %in% "Individual Parameter" & input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_out_label)
    } else if (input$out_se %in% "SANDS Format" & !input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_MT_label)
    } else if (input$out_se %in% "SANDS Format" & input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_MT_out_label)
    }
  })
  
  
  # The map object
  output$HUC8map <- renderLeaflet({
    
    map <- leaflet() %>%
      addPolygons(data = HUC8_dat(), 
                  fill = TRUE,
                  fillColor = "blue", 
                  color = "blue",
                  label = ~huc8,
                  weight = 1,
                  layerId = ~huc8,
                  group = "base_map") %>%
      add_USGS_base()
    
    return(map)
  })
  
  # Update the map selection
  
  HUC_list <- reactiveValues(Selected = character(0))
  
  observeEvent(input$HUC8map_shape_click, {
    
    click <- input$HUC8map_shape_click
    click_id <- click$id
    
    if (isTruthy(!click_id %in% HUC_list$Selected)){
      HUC_list$Selected[[click_id]] <- click_id
    } else {
      HUC_list$Selected <- HUC_list$Selected[!HUC_list$Selected %in% click_id]
    }
    
    # Update the HUC_se
    updateSelectizeInput(session = session, inputId = "HUC_se",
                         selected = HUC_list$Selected)
    
  })
  
  # Update the map with selected HUC8
  HUC8map_proxy <- leafletProxy("HUC8map")
  
  HUC8_temp <- reactive({
    HUC8_temp <- HUC8_dat() %>%
      filter(huc8 %in% input$HUC_se)
    return(HUC8_temp)
  })
  
  observe({
    
    if (nrow(HUC8_temp()) > 0){
      
      HUC8map_proxy %>% clearGroup(group = "highlighted_polygon")
      HUC8map_proxy %>%
        addPolylines(data = HUC8_temp(),
                     stroke = TRUE,
                     color = "red",
                     weight = 2,
                     group = "highlighted_polygon") 
      
    } else {
      HUC8map_proxy %>% clearGroup(group = "highlighted_polygon")
    }
    
  })
  
  # Create the map for BBox
  output$BBox_map <- renderLeaflet({
    map <- leaflet() %>%
      fitBounds(lng1 = -102.11928, lat1 = 50.58677, 
                lng2 = -117.65217, lat2 = 40.37306) %>%
      addDrawToolbar(polylineOptions = FALSE, 
                     circleOptions = FALSE, 
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE, 
                     polygonOptions = FALSE,
                     singleFeature = TRUE) %>%
      add_USGS_base()
    
    return(map)
  })
  
  bbox_reVal <- reactiveValues()
  
  observeEvent(input$BBox_map_draw_new_feature, {
    feat <- input$BBox_map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords_m <- matrix(coords, ncol = 2, byrow = TRUE)
    poly <- st_sf(st_sfc(st_polygon(list(coords_m))), crs = st_crs(27700))
    bbox_temp <- st_bbox(poly)
    
    # Update the bounding box
    bbox_reVal$w_num <- as.numeric(bbox_temp[1])
    bbox_reVal$s_num <- as.numeric(bbox_temp[2])
    bbox_reVal$e_num <- as.numeric(bbox_temp[3])
    bbox_reVal$n_num <- as.numeric(bbox_temp[4])
    
  })
  
  observe({
    updateNumericInput(session = session, inputId = "bb_W", value = bbox_reVal$w_num)
    updateNumericInput(session = session, inputId = "bb_S", value = bbox_reVal$s_num)
    updateNumericInput(session = session, inputId = "bb_E", value = bbox_reVal$e_num)
    updateNumericInput(session = session, inputId = "bb_N", value = bbox_reVal$n_num)
  })
  
  bb_W_debounce <- reactive(input$bb_W) %>% debounce(2000)
  bb_S_debounce <- reactive(input$bb_S) %>% debounce(2000)
  bb_E_debounce <- reactive(input$bb_E) %>% debounce(2000)
  bb_N_debounce <- reactive(input$bb_N) %>% debounce(2000)
  
  # Check if the numbers are within the range
  observe({
    req(input$bb_N, input$bb_S, input$bb_W, input$bb_E)
    if (input$bb_W < -117.65217 | input$bb_W > -102.11928){
      shinyalert(text = "The west bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    if (input$bb_E < -117.65217 | input$bb_E > -102.11928){
      shinyalert(text = "The east bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    if (input$bb_N < 40.37306 | input$bb_N > 50.58677){
      shinyalert(text = "The north bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    if (input$bb_S < 40.37306 | input$bb_S > 50.58677){
      shinyalert(text = "The south bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    
    if (input$bb_W >= input$bb_E){
      shinyalert(text = "The longitude of west bound should not be larger or equal to the east bound. 
                 Please provide a valid longitude entry.", 
                 type = "error")
    }
    
    if (input$bb_S >= input$bb_N){
      shinyalert(text = "The latitude of south bound should not be larger or equal to the north bound.
                 Please provide a valid latitude entry.", 
                 type = "error")
    }
    
  }) %>%
    bindEvent(bb_W_debounce(), bb_S_debounce(), bb_E_debounce(), bb_N_debounce())
  
  
  # Update par_se based on par_group_se
  par_value <- reactiveValues()
  
  observe({
    if ("All" %in% input$par_group_se){
      variable <- parameter_reference_table$Standard_Name
    } else {
      group_par <- parameter_reference_table %>%
        filter(Group %in% input$par_group_se)
      variable <- group_par$Standard_Name
    }
    par_value$variable <- variable
  }) 
  
  observe({
    if (length(input$par_group_se) > 0){
      updateMultiInput(session = session, inputId = "par_se",
                       selected = par_value$variable) 
    } else if (length(input$par_group_se) == 0){
      updateMultiInput(session = session, inputId = "par_se",
                       selected = character(0))
    }
  })
  
  # Reactive objects
  reVal <- reactiveValues(download = FALSE, upload = FALSE)
  
  ### SADNS data download
  location_ind <- reactive({
    if (!is.null(input$area_se)){
      if (input$out_se == "Individual Parameter"){
        if (input$area_se == "State"){
          if (!is.null(input$State_se)){
            return(TRUE)
          } else {
            return(FALSE)
          } 
        } else if (input$area_se == "HUC8"){
          if (!is.null(input$HUC_se)){
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else if (input$area_se == "BBox"){
          if (!is.null(input$bb_W) & 
              !is.null(input$bb_N) & 
              !is.null(input$bb_S) & 
              !is.null(input$bb_E)){
            return(TRUE)
          } else {
            FALSE
          }
        } else {
          return(FALSE)
        } 
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE) 
    }
  })
  
  # Download the data for individual parameters
  observeEvent(input$par_download, {
    req(input$out_se == "Individual Parameter", input$par_se, input$par_date,
        input$sitetype_se, location_ind())
    
    showModal(modalDialog(title = "Estimating the sample size...", footer = NULL))
    
    # Get the parameter
    char_par <- get_par(dat = Parameter_Name_Unit, par = input$par_se)
    
    ### Use readWQPsummary and a for loop to download the parameters and report progress
    
    # Calculate the year summary number
    YearSum <- year(Sys.Date()) - year(ymd(input$par_date[1])) + 1

    if (YearSum <= 1){
      YearSum2 <- 1
    } else if (YearSum > 1 & YearSum <= 5){
      YearSum2 <- 5
    } else {
      YearSum2 <- "all"
    }
    
    if (input$area_se == "State"){
      req(input$State_se)
      

      dat_summary_list <- list(readWQPsummary(statecode = input$State_se,
                                              summaryYears = YearSum2,
                                              siteType = input$sitetype_se)) 
        
    } else if (input$area_se == "HUC8"){
      req(input$HUC_se)
      
      dat_summary_list <- map(input$HUC_se,
                              ~readWQPsummary(huc = .x, summaryYears = YearSum2,
                                              siteType = input$sitetype_se)) 
      
    } else if (input$area_se == "BBox"){
      req(input$bb_W >= -117.65217 & input$bb_W <= -102.11928,
          input$bb_E >= -117.65217 & input$bb_E <= -102.11928,
          input$bb_N >= 40.37306 & input$bb_N <= 50.58677,
          input$bb_S >= 40.37306 & input$bb_S <= 50.58677,
          input$bb_N > input$bb_S,
          input$bb_E > input$bb_W)
      bBox <- c(input$bb_W, input$bb_S, input$bb_E, input$bb_N)
      
      dat_summary_list <- list(readWQPsummary(bBox = bBox, 
                                              summaryYears = YearSum2,
                                              siteType = input$sitetype_se)) 
    }
    
    dat_summary_list <- keep(dat_summary_list, .p = function(x) nrow(x) > 0)
    
    dat_summary <- rbindlist(dat_summary_list, fill = TRUE)
    
    if (nrow(dat_summary) == 0 & ncol(dat_summary) == 0){
      dat_summary <- WQP_summary_template
    }
    
    # Filter by char_par
    dat_summary_filter <- dat_summary %>%
      fsubset(CharacteristicName %in% char_par)
    
    # Use dat_summary_filter to identify HUC8 that needed to be downloaded
    dat_summary2 <- dat_summary %>%
      fsubset(HUCEightDigitCode 
              %in% unique(dat_summary_filter$HUCEightDigitCode))

    removeModal()
    
    # nrow(dat_summary2) > 0, proceed
    if (nrow(dat_summary2) > 0){
      dat_summary3 <- dat_summary2 %>%
        fsubset(YearSummarized >= year(ymd(input$par_date[1])) & 
                  YearSummarized <= year(ymd(input$par_date[2])))
      
      # Filter the dat_summary3 by HUC8_dat()
      dat_summary3_sf <- dat_summary3 %>% 
        distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                 MonitoringLocationLongitude, HUCEightDigitCode) %>%
        drop_na(MonitoringLocationLongitude) %>%
        drop_na(MonitoringLocationLatitude) %>%
        st_as_sf(coords = c("MonitoringLocationLongitude",
                            "MonitoringLocationLatitude"),
                 crs = 4326) %>%
        st_filter(HUC8_dat())

    # Filter by HUCEightDigitCode
      site_all <- dat_summary3 %>% 
        fsubset(HUCEightDigitCode 
               %in% unique(dat_summary3_sf$HUCEightDigitCode)) %>%
        group_by(HUCEightDigitCode, YearSummarized) %>%
        fsummarize(ResultCount = fsum(ResultCount)) %>%
        fungroup() %>%
        arrange(HUCEightDigitCode, YearSummarized) 

      # If nrow(site_all) > 0, download the data
      if (nrow(site_all) > 0){
      
        site_all_list <- site_all %>% split(.$HUCEightDigitCode)
        site_all2 <- map_dfr(site_all_list, cumsum_group, col = "ResultCount", threshold = 25000)
        site_all3 <- site_all2 %>% summarized_year()
        
        
        progressSweetAlert(
          session = session, id = "myprogress",
          title = "Download Progress",
          display_pct = TRUE, value = 0
        )
        
        dat_temp_all_list <- list()

        # Download result and narrow profile
        
        for (i in 1:nrow(site_all3)){
          
          updateProgressBar(
            session = session,
            id = "myprogress",
            value = round(i/nrow(site_all3) * 100, 2)
          )
      
          HUC_temp <- site_all3$HUCEightDigitCode[i]
          Start_temp <- site_all3$Start[i]
          End_temp <- site_all3$End[i]
          
          if (length(char_par) <= 5){
            args_temp <- args_create(
              huc = HUC_temp,
              characteristicName = char_par,
              startDateLo = Start_temp,
              startDateHi = End_temp,
              siteType = input$sitetype_se
            )
          } else {
            args_temp <- args_create(
              huc = HUC_temp,
              startDateLo = Start_temp,
              startDateHi = End_temp,
              siteType = input$sitetype_se
            )
          }
          
          resultPhysChem_temp <- resultPhysChem_download(args_temp, 
                                                         ref = resultPhysChem_template)
          
          project_temp <- project_download(args_temp, ref = project_template)
          
          site_temp <- site_download(args_temp, ref = site_template)
          
          output_temp <- TADA_join3(site = site_temp, 
                                   resultphyschem = resultPhysChem_temp,
                                   project = project_temp, 
                                   ref = TADA_download_temp)
          
          if (is.null(output_temp)){
            output_temp <- TADA_download_temp
          }
          
          dat_temp_all_list[[i]] <- output_temp
        }
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title = "Download completed !",
          type = "success"
        )
        
        # Subset the site data by the bBox
        if (input$area_se == "BBox"){
          bBox_sf <- st_bbox(
            c(
              xmin = input$bb_W,
              xmax = input$bb_E,
              ymax = input$bb_N,
              ymin = input$bb_S
            ),
            crs = st_crs(4326)
          ) %>%
            st_as_sfc()

          dat_summary3_sf <- dat_summary3_sf %>%
            st_filter(bBox_sf)
        }
        
        dat_temp_com <- rbindlist(dat_temp_all_list, fill = TRUE)
        
        dat_temp_all <- dat_temp_com %>%
          # Filter the data with the parameter names
          fsubset(CharacteristicName %in% char_par) %>%
          # Filter with the site ID in the state and dates
          fsubset(MonitoringLocationIdentifier %in% dat_summary3_sf$MonitoringLocationIdentifier) %>%
          fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
        
        # If nrow(site_all) == 0, return an empty data frame with the header as dat_temp_all
      } else {
        
        dat_temp_all <- TADA_download_temp
      
      }
      
      # If nrow(dat_summary) == 0, return an empty data frame with the header as dat_temp_all
    } else {
      
      dat_temp_all <- TADA_download_temp
      
    }
    
    reVal$WQP_dat <- dat_temp_all
    reVal$download <- TRUE
    reVal$upload <- FALSE
    
  })
  
  ### Update the UI for data summary
  output$summary_box <- renderUI({
    if (input$out_se %in% "Individual Parameter"){
      tagList(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Data Summary",
              status = "warning", solidHeader = TRUE,
              width = 12, collapsible = TRUE,
              h2("Raw Data Summary"),
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(outputId = "Date_Info", placeholder = TRUE)
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  selectInput(inputId = "raw_summary", label = "Data attribute to summarize",
                              choices = c("Site Type" = "MonitoringLocationTypeName",
                                          "Sample Status" = "ResultStatusIdentifier",
                                          "Original Parameter Names" = "CharacteristicName",
                                          "Sample Fraction" = "ResultSampleFractionText"),
                              multiple = TRUE)
                ),
                column(
                  width = 9,
                  DTOutput(outputId = "raw_summary_table")
                )
              ),
              h2("Clean Data Summary"),
              br(),
              fluidRow(
                column(
                  width = 3,
                  selectInput(inputId = "clean_summary", label = "Data attribute to summarize",
                              choices = c("Standard Name" = "TADA.CharacteristicName",
                                          "Standard Fraction" = "TADA.ResultSampleFractionText",
                                          "Censored Data" = "TADA.CensoredData.Flag"),
                              multiple = TRUE)
                ),
                column(
                  width = 9,
                  DTOutput(outputId = "clean_summary_table")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(outputId = "Date_Info2", placeholder = TRUE)
                )
              )
            )
          )
        )
      )
    } else {
      tagList(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Data Summary",
              status = "warning", solidHeader = TRUE,
              width = 12, collapsible = TRUE,
              h2("Raw Data Summary"),
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(outputId = "SANDS_Date_Info", placeholder = TRUE)
                )
              )
            )
          )
        )
      )
    }
  })
  
  ### Get raw data summary
  observe({
    req(reVal$WQP_dat)
    
    # Store the download date
    download_date <- format(Sys.time())
    
    # Site number and sample size
    site_number <- length(unique(reVal$WQP_dat$MonitoringLocationIdentifier))
    sample_size <- nrow(reVal$WQP_dat)
    
    # Min and Max Date
    if (sample_size == 0){
      min_date <- ""
      max_date <- ""
    } else {
      min_date <- min(reVal$WQP_dat$ActivityStartDate, na.rm = TRUE)
      max_date <- max(reVal$WQP_dat$ActivityStartDate, na.rm = TRUE) 
    }
    
    # Save the value
    reVal$download_date <- download_date
    reVal$min_date <- min_date
    reVal$max_date <- max_date
    reVal$site_number <- site_number
    reVal$sample_size <- sample_size
  })
  
  observe({
    req(reVal$WQP_dat, reVal$sample_size)
    output$Date_Info <- renderText({
      req(reVal$WQP_dat, reVal$sample_size)
      if (reVal$sample_size == 0){
        paste0("Data are downloaded at ", reVal$download_date, ".\n",
               "There are ", reVal$sample_size, " records from ", reVal$site_number, " sites.\n") 
      } else {
        paste0("Data are downloaded at ", reVal$download_date, ".\n",
               "The date range is from ", reVal$min_date, " to ", reVal$max_date, ".\n",
               "There are ", reVal$sample_size, " records from ", reVal$site_number, " sites.\n") 
      }
    }) 
  })
  
  ### Summarize the raw data property
  
  observe({
    req(reVal$WQP_dat, input$raw_summary)
    output$raw_summary_table <- renderDT({
      tt <- count_fun(reVal$WQP_dat, input$raw_summary)
      datatable(tt, options = list(scrollX = TRUE)) %>%
        formatPercentage(columns = "Percent (%)", digits = 2)
    })
  })
  
  ### Clean the data
  
  # TADA_AutoClean
  observe({
    req(reVal$WQP_dat)
    
    showModal(modalDialog(title = "Run TADA_AutoClean", 
                          footer = NULL))
    
    temp_dat <- TADA_AutoClean_poss(
      .data = reVal$WQP_dat
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_AutoClean_temp
    }
    
    reVal$WQP_AutoClean <- temp_dat
    
    removeModal()
    
    print("WQP_AutoClean")
    print(nrow(temp_dat))
    
  })
  
  # TADA_SimpleCensoredMethods
  observe({
    req(reVal$WQP_AutoClean)
    
    showModal(modalDialog(title = "Run TADA_SimpleCensoredMethods", 
                          footer = NULL))
    
    temp_dat <- TADA_SimpleCensoredMethods_poss(
      .data = reVal$WQP_AutoClean,
      nd_multiplier = 1
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_SimpleCensoredMethods_temp
    }
    
    reVal$WQP_SimpleCensoredMethods <- temp_dat
    
    removeModal()
    
    print("WQP_SimpleCensoredMethods")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagMethod
  observe({
    req(reVal$WQP_SimpleCensoredMethods)
    
    showModal(modalDialog(title = "Run TADA_FlagMethod", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagMethod_poss(
      .data = reVal$WQP_SimpleCensoredMethods,
      clean = FALSE
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagMethod_temp
    }
    
    reVal$WQP_FlagMethod <- temp_dat
    
    removeModal()
    
    print("WQP_FlagMethod")
    print(nrow(temp_dat))
  })
  
  # TADA_FlagSpeciation
  observe({
    req(reVal$WQP_FlagMethod)
    
    showModal(modalDialog(title = "Run TADA_FlagSpeciation", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagSpeciation_poss(
      .data = reVal$WQP_FlagMethod,
      clean = "none"
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagSpeciation_temp
    }
    
    reVal$WQP_FlagSpeciation <- temp_dat
    
    removeModal()
    
    print("WQP_FlagSpeciation")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagResultUnit
  observe({
    req(reVal$WQP_FlagSpeciation)
    
    showModal(modalDialog(title = "Run TADA_FlagResultUnit", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagResultUnit_poss(
      .data = reVal$WQP_FlagSpeciation,
      clean = "none"
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagResultUnit_temp
    }
    
    reVal$WQP_FlagResultUnit <- temp_dat
    
    removeModal()
    
    print("WQP_FlagResultUnit")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagFraction
  observe({
    req(reVal$WQP_FlagResultUnit)
    
    showModal(modalDialog(title = "Run TADA_FlagFraction", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagFraction_poss(
      .data = reVal$WQP_FlagResultUnit,
      clean = FALSE
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagFraction_temp
    }
    
    reVal$WQP_FlagFraction <- temp_dat
    
    removeModal()
    
    print("WQP_FlagFraction")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagCoordinates
  observe({
    req(reVal$WQP_FlagFraction)
    
    showModal(modalDialog(title = "Run TADA_FlagCoordinates", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagCoordinates_poss(
      .data = reVal$WQP_FlagFraction
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagCoordinates_temp
    }
    
    reVal$WQP_FlagCoordinates <- temp_dat
    
    removeModal()
    
    print("WQP_FlagCoordinates")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FindQCActivities
  observe({
    req(reVal$WQP_FlagCoordinates)
    
    showModal(modalDialog(title = "Run TADA_FindQCActivities", 
                          footer = NULL))
    
    temp_dat <- TADA_FindQCActivities_poss(
      .data = reVal$WQP_FlagCoordinates
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FindQCActivities_temp
    }
    
    reVal$WQP_FindQCActivities <- temp_dat
    
    removeModal()
    
    print("WQP_FindQCActivities")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagMeasureQualifierCode
  observe({
    req(reVal$WQP_FindQCActivities)
    
    showModal(modalDialog(title = "Run TADA_FlagMeasureQualifierCode", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagMeasureQualifierCode_poss(
      .data = reVal$WQP_FindQCActivities
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagMeasureQualifierCode_temp
    }
    
    reVal$WQP_FlagMeasureQualifierCode <- temp_dat
    
    removeModal()
    
    print("WQP_FlagMeasureQualifierCode")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FindPotentialDuplicatesSingleOrg
  observe({
    req(reVal$WQP_FlagMeasureQualifierCode)
    
    showModal(modalDialog(title = "Run TADA_FindPotentialDuplicatesSingleOrg", 
                          footer = NULL))
    
    temp_dat <- TADA_FindPotentialDuplicatesSingleOrg_poss(
      .data = reVal$WQP_FlagMeasureQualifierCode
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FindPotentialDuplicatesSingleOrg_temp
    }
    
    reVal$WQP_FindPotentialDuplicatesSingleOrg <- temp_dat
    
    removeModal()
    
    print("WQP_FindPotentialDuplicatesSingleOrg")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FindPotentialDuplicatesMultipleOrgs
  observe({
    req(reVal$WQP_FindPotentialDuplicatesSingleOrg)
    
    showModal(modalDialog(title = "Run TADA_FindPotentialDuplicatesMultipleOrgs", 
                          footer = NULL))
    
    temp_dat <- TADA_FindPotentialDuplicatesMultipleOrgs_poss(
      .data = reVal$WQP_FindPotentialDuplicatesSingleOrg
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FindPotentialDuplicatesMultipleOrgs_temp
    }
    
    reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- temp_dat
    
    removeModal()
    
    print("WQP_FindPotentialDuplicatesMultipleOrgs")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FindQAPPApproval
  observe({
    req(reVal$WQP_FindPotentialDuplicatesMultipleOrgs)
    
    showModal(modalDialog(title = "Run TADA_FindQAPPApproval", 
                          footer = NULL))
    
    temp_dat <- TADA_FindQAPPApproval_poss(
      .data = reVal$WQP_FindPotentialDuplicatesMultipleOrgs
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FindQAPPApproval_temp
    }
    
    reVal$WQP_FindQAPPApproval <- temp_dat
    
    removeModal()
    
    print("WQP_FindQAPPApproval")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FindQAPPDoc
  observe({
    req(reVal$WQP_FindQAPPApproval)
    
    showModal(modalDialog(title = "Run TADA_FindQAPPDoc", 
                          footer = NULL))
    
    temp_dat <- TADA_FindQAPPDoc_poss(
      .data = reVal$WQP_FindQAPPApproval
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FindQAPPDoc_temp
    }
    
    reVal$WQP_FindQAPPDoc <- temp_dat
    
    removeModal()
    
    print("WQP_FindQAPPDoc")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FindContinuousData
  observe({
    req(reVal$WQP_FindQAPPDoc)
    
    showModal(modalDialog(title = "Run TADA_FindContinuousData", 
                          footer = NULL))
    
    temp_dat <- TADA_FindContinuousData_poss(
      .data = reVal$WQP_FindQAPPDoc
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FindContinuousData_temp
    }
    
    reVal$WQP_FindContinuousData <- temp_dat
    
    removeModal()
    
    print("WQP_FindContinuousData")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagAboveThreshold
  observe({
    req(reVal$WQP_FindContinuousData)
    
    showModal(modalDialog(title = "Run TADA_FlagAboveThreshold", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagAboveThreshold_poss(
      .data = reVal$WQP_FindContinuousData
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagAboveThreshold_temp
    }
    
    reVal$WQP_FlagAboveThreshold <- temp_dat
    
    removeModal()
    
    print("WQP_FlagAboveThreshold")
    print(nrow(temp_dat))
    
  })
  
  # TADA_FlagBelowThreshold
  observe({
    req(reVal$WQP_FlagAboveThreshold)
    
    showModal(modalDialog(title = "Run TADA_FlagBelowThreshold", 
                          footer = NULL))
    
    temp_dat <- TADA_FlagBelowThreshold_poss(
      .data = reVal$WQP_FlagAboveThreshold
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_FlagBelowThreshold_temp
    }
    
    reVal$WQP_FlagBelowThreshold <- temp_dat
    
    removeModal()
    
    print("WQP_FlagBelowThreshold")
    print(nrow(temp_dat))
    
  })
  
  # TADA_HarmonizeSynonyms
  observe({
    req(reVal$WQP_FlagBelowThreshold)
    
    showModal(modalDialog(title = "Run TADA_HarmonizeSynonyms", 
                          footer = NULL))
    
    temp_dat <- TADA_HarmonizeSynonyms_poss(
      .data = reVal$WQP_FlagBelowThreshold, ref = ref_harm
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_HarmonizeSynonyms_temp
    }
    
    reVal$WQP_HarmonizeSynonyms <- temp_dat
    
    removeModal()
    
    print("WQP_HarmonizeSynonyms")
    print(nrow(temp_dat))
    
  })
  
  # TADA_depth_combine
  observe({
    req(reVal$WQP_HarmonizeSynonyms)
    
    showModal(modalDialog(title = "Combine depth columns.", 
                          footer = NULL))
    
    temp_dat <- TADA_depth_combine_poss(
      TADA_dat = reVal$WQP_HarmonizeSynonyms
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_depth_combine_temp
    }
    
    reVal$WQP_depth_combine <- temp_dat
    
    removeModal()
    
    print("WQP_depth_combine")
    print(nrow(temp_dat))
    
  })
  
  
  # Select the correct columns to create WQP_dat2
  observe({
    req(reVal$WQP_depth_combine)
    
    temp_dat <- reVal$WQP_depth_combine %>%
      TADA_selector_poss()
    
    if (is.null(temp_dat)){
      .data <- TADA_selector_temp
    }
    
    reVal$WQP_dat2 <- temp_dat
    
  })
  
  # Filter the data for the following criteria
  # TADA.ActivityType.Flag: Non_QC
  # TADA.MethodSpeciation.Flag: Remove "Rejected"
  # TADA.ResultMeasureValueDataTypes.Flag: Remove "Text" and "NA - Not Available"
  # TADA.ResultMeasureValue: Remove NA
  # TADA.ResultValueAboveUpperThreshold.Flag: Remove "Suspect"
  # TADA.ResultValueBelowLowerThreshold.Flag: Remove "Suspect"
  # TADA.ResultUnit.Flag: Remove "Rejected"
  # TADA.AnalyticalMethod.Flag: Remove "Invalid"
  # TADA.MeasureQualifierCode.Flag: Remove "Suspect"
  
  observe({
    req(reVal$WQP_dat2)
    
    reVal$WQP_dat3 <- reVal$WQP_dat2 %>%
      fsubset(TADA.ActivityType.Flag %in% "Non_QC") %>%
      fsubset(!TADA.MethodSpeciation.Flag %in% c("Rejected")) %>%
      fsubset(!TADA.ResultMeasureValueDataTypes.Flag %in% c("Text", "NA - Not Available")) %>%
      fsubset(!is.na(TADA.ResultMeasureValue)) %>%
      fsubset(!TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect") %>%
      fsubset(!TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect") %>%
      fsubset(!TADA.ResultUnit.Flag %in% "Rejected") %>%
      fsubset(!TADA.AnalyticalMethod.Flag %in% "Invalid") %>%
      fsubset(!TADA.MeasureQualifierCode.Flag %in% "Suspect")
    
    req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
    reVal$min_date2 <- min(reVal$WQP_dat3$ActivityStartDate)
    reVal$max_date2 <- max(reVal$WQP_dat3$ActivityStartDate)
    reVal$site_number2 <- length(unique(reVal$WQP_dat3$MonitoringLocationIdentifier))
    reVal$sample_size2 <- nrow(reVal$WQP_dat3)
  })
  
  # Display data information after data cleaning
  observe({
    req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
    output$Date_Info2 <- renderText({
      req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
      paste0("There are ", reVal$sample_size2, " records from ", 
             reVal$site_number2, " sites after data cleaning.")
    })
  })
  
  observe({
    req(reVal$WQP_dat2, input$clean_summary)
    output$clean_summary_table <- renderDT({
      tt <- count_fun(reVal$WQP_dat2, input$clean_summary)
      datatable(tt, options = list(scrollX = TRUE)) %>%
        formatPercentage(columns = "Percent (%)", digits = 2)
    })
  })
  
  SANDS_object <- reactiveValues()
  
  ### SADNS data download
  SANDS_location_ind <- reactive({
    if (!is.null(input$area_se)){
      if (input$out_se == "SANDS Format"){
        if (input$area_se == "HUC8"){
          if (!is.null(input$HUC_se)){
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else if (input$area_se == "BBox"){
          if (!is.null(input$bb_W) & 
              !is.null(input$bb_N) & 
              !is.null(input$bb_S) & 
              !is.null(input$bb_E)){
            return(TRUE)
          } else {
            FALSE
          }
        } else {
          return(FALSE)
        } 
      } else {
        return(FALSE)
      }
    } else {
     return(FALSE) 
    }
  })
  
  observeEvent(input$SANDS_download, {
    req(input$out_se == "SANDS Format", length(input$MT_par_group) > 0, input$par_date,
        SANDS_location_ind())
    
    ### Use readWQPsummary and a for loop to download the parameters and report progress
    
    showModal(modalDialog(title = "Estimating the sample size...", footer = NULL))
    
    # Calculate the year summary number
    YearSum <- ceiling(as.numeric(Sys.Date() - ymd(input$par_date[1]))/365)
    
    if (YearSum <= 1){
      YearSum2 <- 1
    } else if (YearSum > 1 & YearSum <= 5){
      YearSum2 <- 5
    } else {
      YearSum2 <- "all"
    }
    
    if (input$area_se == "HUC8"){
      req(input$HUC_se)
      
      dat_summary_list <- map(input$HUC_se,
                              ~readWQPsummary(huc = .x, summaryYears = YearSum2))
      
      
    } else if (input$area_se == "BBox"){
      req(input$bb_W >= -117.65217 & input$bb_W <= -102.11928,
          input$bb_E >= -117.65217 & input$bb_E <= -102.11928,
          input$bb_N >= 40.37306 & input$bb_N <= 50.58677,
          input$bb_S >= 40.37306 & input$bb_S <= 50.58677,
          input$bb_N > input$bb_S,
          input$bb_E > input$bb_W)
      bBox <- c(input$bb_W, input$bb_S, input$bb_E, input$bb_N)
      
      dat_summary_list <- list(readWQPsummary(bBox = bBox, 
                                              summaryYears = YearSum2))
    }
    
    dat_summary_list <- keep(dat_summary_list, .p = function(x) nrow(x) > 0)
    
    dat_summary <- rbindlist(dat_summary_list, fill = TRUE)
    
    removeModal()
    
    # nrow(dat_summary) > 0, proceed
    if (nrow(dat_summary) > 0){
      dat_summary2 <- dat_summary %>%
        fsubset(YearSummarized >= year(ymd(input$par_date[1])) & 
                  YearSummarized <= year(ymd(input$par_date[2])))
      
      # Filter the dat_summary2 by HUC8_dat()
      dat_summary2_sf <- dat_summary2 %>% 
        distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                 MonitoringLocationLongitude, HUCEightDigitCode) %>%
        drop_na(MonitoringLocationLongitude) %>%
        drop_na(MonitoringLocationLatitude) %>%
        st_as_sf(coords = c("MonitoringLocationLongitude",
                            "MonitoringLocationLatitude"),
                 crs = 4326) %>%
        st_filter(HUC8_dat())
      
      # Filter by HUCEightDigitCode
      site_all <- dat_summary2 %>% 
        fsubset(HUCEightDigitCode 
               %in% unique(dat_summary2_sf$HUCEightDigitCode)) %>%
        group_by(HUCEightDigitCode, YearSummarized) %>%
        fsummarize(ResultCount = fsum(ResultCount)) %>%
        fungroup() %>%
        arrange(HUCEightDigitCode, YearSummarized) 
      
      # If nrow(site_all) > 0, download the data
      if (nrow(site_all) > 0){
        
        site_all_list <- site_all %>% split(.$HUCEightDigitCode)
        site_all2 <- map_dfr(site_all_list, cumsum_group, col = "ResultCount", threshold = 25000)
        site_all3 <- site_all2 %>% summarized_year()
        
        site_temp_all_list <- list()
        TADA_site_temp_all_list <- list()
        result_temp_all_list <- list()
        narrow_temp_all_list <- list()
        resultphyschem_temp_all_list <- list()
        
        progressSweetAlert(
          session = session, id = "myprogress_sands",
          title = "Download Progress",
          display_pct = TRUE, value = 0
        )
        
        # Download result and narrow profile
        
        for (i in 1:nrow(site_all3)){
          
          updateProgressBar(
            session = session,
            id = "myprogress_sands",
            value = round(i/nrow(site_all3) * 100, 2)
          )
          
          HUC_temp <- site_all3$HUCEightDigitCode[i]
          Start_temp <- site_all3$Start[i]
          End_temp <- site_all3$End[i]
          
          SANDS_args_temp1 <- args_create(
            huc = HUC_temp,
            startDateLo = Start_temp,
            startDateHi = End_temp,
          )
          
          TADA_site_temp <- SANDS_site_fun(SANDS_args_temp1, ref = site_template)
          
          result_temp <- SANDS_result_fun(SANDS_args_temp1, ref = result_template)
          
          narrow_temp <- SANDS_narrow_fun(SANDS_args_temp1, ref = narrow_template)
          
          resultphyschem_temp <- SANDS_resultphyschem_fun(SANDS_args_temp1, ref = resultPhysChem_template)
          
          if (nrow(TADA_site_temp) == 0) {
            TADA_site_temp <- site_template
          }
          
          if (nrow(result_temp) == 0) {
            result_temp <- result_template
          }
          
          if (nrow(narrow_temp) == 0) {
            narrow_temp <- narrow_template
          }
          
          if (nrow(resultphyschem_temp) == 0) {
            resultphyschem_temp <- resultPhysChem_template
          }
          
          TADA_site_temp_all_list[[i]] <- TADA_site_temp
          result_temp_all_list[[i]] <- result_temp
          narrow_temp_all_list[[i]] <- narrow_temp
          resultphyschem_temp_all_list[[i]] <- resultphyschem_temp
        
        }
        
        # Download site
        HUC_unique <- unique(site_all3$HUCEightDigitCode)
        
        for (j in 1:length(HUC_unique)){
          HUC_temp2 <- HUC_unique[j]
          SANDS_args_temp2 <- args_create(
            huc = HUC_temp2,
          )
          
          site_temp <- SANDS_site_fun(SANDS_args_temp2, ref = site_template)
          
          if (nrow(site_temp) == 0) {
            site_temp <- site_template
          }
          
          site_temp_all_list[[j]] <- site_temp
        }
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title = "Download completed !",
          type = "success"
        )
        
        site_temp_com <- rbindlist(site_temp_all_list, fill = TRUE)
        
        site_temp_sf <- site_temp_com %>%
          drop_na(LongitudeMeasure) %>%
          drop_na(LatitudeMeasure) %>%
          st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"),
                   crs = 4326) %>%
          st_filter(HUC8_dat()) 
        
        # Subset the site data by the bBox
        if (input$area_se == "BBox"){
          bBox_sf <- st_bbox(
            c(
              xmin = input$bb_W, 
              xmax = input$bb_E,
              ymax = input$bb_N,
              ymin = input$bb_S
            ),
            crs = st_crs(4326)
          ) %>% 
            st_as_sfc()

          site_temp_sf <- site_temp_sf %>%
            st_filter(bBox_sf)
        }
        
        site_temp_filter <- site_temp_com %>%
          fsubset(MonitoringLocationIdentifier %in% site_temp_sf$MonitoringLocationIdentifier)
        
        site_temp_all <- site_temp_filter
        result_temp_all <- rbindlist(result_temp_all_list, fill = TRUE)
        narrow_temp_all <- rbindlist(narrow_temp_all_list, fill = TRUE)
        resultphyschem_temp_all <- rbindlist(resultphyschem_temp_all_list, fill = TRUE)
        
        # A site data frame for the TADA clean
        TADA_site_temp_all <- rbindlist(TADA_site_temp_all_list, fill = TRUE)
        site_temp_all_TADA_sub <- TADA_site_temp_all %>%
          fsubset(MonitoringLocationIdentifier %in% site_temp_all$MonitoringLocationIdentifier)
        
        # filter result_temp_all and narrow_temp_all with sites in site_temp_all
        result_temp_all <- result_temp_all %>%
          fsubset(MonitoringLocationIdentifier %in% site_temp_all$MonitoringLocationIdentifier) %>%
          fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
        
        narrow_temp_all <- narrow_temp_all %>%
          fsubset(MonitoringLocationIdentifier %in% site_temp_all$MonitoringLocationIdentifier) %>%
          fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
        
        resultphyschem_temp_all <- resultphyschem_temp_all %>%
          fsubset(MonitoringLocationIdentifier %in% site_temp_all$MonitoringLocationIdentifier) %>%
          fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
      
      # If nrow(site_all) == 0, return empty templates  
      } else {
        
        site_temp_all <- site_template
        TADA_site_temp_all <- site_template
        result_temp_all <- result_template
        narrow_temp_all <- narrow_template
        resultphyschem_temp_all <- resultPhysChem_template
      }
      
      # nrow(dat_summary) == 0, return empty templates
    } else {
      
      site_temp_all <- site_template
      TADA_site_temp_all <- site_template
      result_temp_all <- result_template
      narrow_temp_all <- narrow_template
      resultphyschem_temp_all <- resultPhysChem_template
      
    }
    
    SANDS_object$site_temp <- site_temp_all
    SANDS_object$TADA_site_temp <- TADA_site_temp_all
    SANDS_object$result_temp <- result_temp_all
    SANDS_object$narrow_temp <- narrow_temp_all
    SANDS_object$resultphyschem_temp <- resultphyschem_temp_all
    
    # # Store the download date
    download_date <- format(Sys.time())
    
    SANDS_object$download_date <- download_date
    
  })
  
  ### Get TADA clean
  observe({
    req(SANDS_object$TADA_site_temp, SANDS_object$resultphyschem_temp, SANDS_object$narrow_temp)
    
    showModal(modalDialog(title = "Preparing the TADA data frame...", footer = NULL))
    
    TADA_site_temp = SANDS_object$TADA_site_temp
    resultphyschem_temp = SANDS_object$resultphyschem_temp
    narrow_temp = SANDS_object$narrow_temp
    
    SANDS_dat <- TADA_join(site = TADA_site_temp, 
                           resultphyschem = resultphyschem_temp,
                           ref = TADA_join_temp)
    
    # Create the Depth columns
    SANDS_dat <- SANDS_dat %>% TADA_depth_combine()
    
    SANDS_object$SANDS_dat <- SANDS_dat
    
    removeModal()
    
  })
  
  ### Get raw SANDS data summary
  observe({
    req(SANDS_object$site_temp, SANDS_object$result_temp, SANDS_object$narrow_temp)

    # Site number and sample size
    site_number <- length(unique(SANDS_object$site_temp$MonitoringLocationIdentifier))
    sample_size <- fnrow(SANDS_object$result_temp)

    # Min and Max Date
    if (sample_size == 0){
      min_date <- ""
      max_date <- ""
    } else {
      min_date <- fmin(SANDS_object$result_temp$ActivityStartDate, na.rm = TRUE)
      max_date <- fmax(SANDS_object$result_temp$ActivityStartDate, na.rm = TRUE)
    }

    if (sample_size == 0){
      Date_info <- paste0("Data are downloaded at ", SANDS_object$download_date, ".\n",
             "There are ", SANDS_object$sample_size, " records from ",
             SANDS_object$site_number, " sites.\n")
    } else {
      Date_info <- paste0("Data are downloaded at ", SANDS_object$download_date, ".\n",
             "The date range is from ", SANDS_object$min_date, " to ", SANDS_object$max_date, ".\n",
             "There are ", SANDS_object$sample_size, " records from ", SANDS_object$site_number, " sites.\n")
    }

    # Save the value
    SANDS_object$min_date <- min_date
    SANDS_object$max_date <- max_date
    SANDS_object$site_number <- site_number
    SANDS_object$sample_size <- sample_size
    SANDS_object$Date_info <- Date_info

  })
  
  observe({
    output$SANDS_Date_Info <- renderText({
      if (is.null(SANDS_object$Date_info)){
        
      } else {
        SANDS_object$Date_info
      }
    })
  })
  
  # Join the AU and MT res
  observe({
    req(input$out_se == "SANDS Format", length(input$MT_par_group) > 0, input$par_date,
        !is.null(SANDS_object$site_temp))
    
    SANDS_object$site_temp1 <- SANDS_object$site_temp %>% 
      SANDS_site_join(tribal_sf = MT_tribal, AU_sf = AU)
    
  })
  
  # Clean up the SANDS data
  observe({
    req(input$out_se == "SANDS Format", length(input$MT_par_group) > 0, input$par_date,
        !is.null(SANDS_object$site_temp1), !is.null(SANDS_object$result_temp), !is.null(SANDS_object$narrow_temp))

    SANDS_object$site_temp2 <- SANDS_object$site_temp1 %>% 
      SANDS_clean_site(ref = STATIONS, state_county = state_county)
    SANDS_object$result_temp2 <- SANDS_object$result_temp %>% 
      SANDS_clean_result(ref = RESULTS)
    SANDS_object$narrow_temp2 <- SANDS_object$narrow_temp %>% 
      SANDS_clean_narrow(ref = NARROW) 

  })
  
  # SANDS data summary
  observe({
    req(SANDS_object$site_temp1, SANDS_object$result_temp, SANDS_object$narrow_temp)
    
    showModal(modalDialog(title = "Summarizing the data...", footer = NULL))
    
    SANDS_object$summary_temp_part1 <- SANDS_create_summary_part1(
      sites = SANDS_object$site_temp1,
      results = SANDS_object$result_temp,
      narrows = SANDS_object$narrow_temp,
      ref = SUMMARY,
      state_county = state_county
    )
    
    SANDS_object$summary_temp <- SANDS_create_summary_part2(
      SANDS_object$summary_temp_part1,
      ref = SUMMARY
    )
    
    removeModal()
        
  })
  
  # Create individual tabs
  observe({
    req(SANDS_object$site_temp2, SANDS_object$result_temp2, SANDS_object$narrow_temp2,
        SANDS_object$summary_temp, SANDS_object$SANDS_dat)
    
    showModal(modalDialog(title = "Formating the data...", footer = NULL))
    
    # H20 tab
    if ("metal_w" %in% input$MT_par_group){
      SANDS_object$h20_tab <- H2O_tab(
        x = SANDS_object$summary_temp_part1,
        address_dat = metals_h2o_head_long,
        lookup = metals_h2o_lookup,
        assign_fraction = metals_h2o_special,
        unit_dat = unit_dat2,
        header = metals_h2o_head,
        TADA_file = SANDS_object$SANDS_dat
      )
      SANDS_object$h20_GIS_tab <- GIS_output_metal_h2o(SANDS_object$h20_tab)
    }
    
    # sed tab
    if ("metal_s" %in% input$MT_par_group){
      SANDS_object$sed_tab <- sed_tab(
        x = SANDS_object$summary_temp_part1,
        address_dat = metals_sed_head_long,
        lookup = metals_sed_lookup,
        unit_dat = unit_dat2,
        header = metals_sed_head
      )
      SANDS_object$sed_GIS_tab <- GIS_output_metal_sed(SANDS_object$sed_tab)
    }
    
    # salinity tab
    if ("salinity" %in% input$MT_par_group){
      SANDS_object$salinity_tab <- salinity_tab(
        x = SANDS_object$summary_temp_part1,
        address_dat = salinity_head_long,
        lookup = salinity_lookup,
        assign_fraction = salinity_special,
        unit_dat = unit_dat2,
        header = salinity_head,
        TADA_file = SANDS_object$SANDS_dat
      )
      SANDS_object$salinity_GIS_tab <- GIS_output_salinity(SANDS_object$salinity_tab)
    }
    
    # oil_gas tab
    if ("oil_gas" %in% input$MT_par_group){
      SANDS_object$oil_tab <- oil_tab(
        x = SANDS_object$summary_temp_part1,
        address_dat = oil_head_long,
        lookup = Oil_lookup,
        assign_fraction = Oil_special,
        unit_dat = unit_dat2,
        header = oil_head
      )
      SANDS_object$oil_GIS_tab <- GIS_output_oil(SANDS_object$oil_tab)
    }
    
    # nutrients
    if ("nutrients" %in% input$MT_par_group){
      SANDS_object$nutrients_tab <- nutrients_tab(
        x = SANDS_object$summary_temp_part1,
        address_dat = nutrients_head_long,
        lookup_main = nutrients_lookup,
        lookup2_spec = nutrients_special_spec,
        narrow_dat = SANDS_object$narrow_temp2,
        assign_fraction = nutrients_special,
        unit_dat = unit_dat2,
        fraction_dat = Fraction,
        header = nutrients_head,
        TADA_file = SANDS_object$SANDS_dat
      )
      SANDS_object$nutrients_GIS_tab <- GIS_output_nutrient(SANDS_object$nutrients_tab)
    }
    
    removeModal()
    
  })

  output$SANDS_data_download <- downloadHandler(
    filename = function() {
      req(SANDS_object$site_temp2, SANDS_object$result_temp2, SANDS_object$narrow_temp2,
          SANDS_object$summary_temp)
      paste0("WQP_data_SANDS_", Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      req(SANDS_object$site_temp2, SANDS_object$result_temp2, SANDS_object$narrow_temp2,
          SANDS_object$summary_temp)
      
      showModal(modalDialog(title = "Saving the ZIP...", footer = NULL))
      
      # Saving the data to local computer
      # wb_save(SANDS_object$wb2, file = con)
      
      file_names <- character()
      
      if (nrow(SANDS_object$site_temp2) > 0){
        
        write_csv(SANDS_object$site_temp2, file.path(tempdir(), "STATIONS.csv"), na = "")
        
        file_names["STATIONS"] <- "STATIONS.csv"
      }
      
      if (nrow(SANDS_object$result_temp2) > 0){
        
        write_csv(SANDS_object$result_temp2, file.path(tempdir(), "RESULTS.csv"), na = "")
        
        file_names["RESULTS"] <- "RESULTS.csv"
        
      }
      
      if (nrow(SANDS_object$summary_temp) > 0){
        
        write_csv(SANDS_object$summary_temp, file.path(tempdir(), "SUMMARY.csv"), na = "")
        
        file_names["SUMMARY"] <- "SUMMARY.csv"
        
      }
      
      if (nrow(SANDS_object$narrow_temp2) > 0){
        
        write_csv(SANDS_object$narrow_temp2, file.path(tempdir(), "Narrow.csv"), na = "")
        
        file_names["Narrow"] <- "Narrow.csv"
        
      }
      
      if (is.data.frame(SANDS_object$nutrients_tab)){
        if (nrow(SANDS_object$nutrients_tab) > 0){
          
          write_csv(SANDS_object$nutrients_tab, file.path(tempdir(), "Nutrients.csv"), na = "")
          
          file_names["Nutrients"] <- "Nutrients.csv"
          
          write_csv(SANDS_object$nutrients_GIS_tab, file.path(tempdir(), "Nutrients_GIS.csv"), na = "")
          
          file_names["Nutrients_GIS"] <- "Nutrients_GIS.csv"
          
        }
      }
      
      if (is.data.frame(SANDS_object$h20_tab)){
        if (nrow(SANDS_object$h20_tab) > 0){
          
          write_csv(SANDS_object$h20_tab, file.path(tempdir(), "Metals_H2O.csv"), na = "")
          
          file_names["Metals(H2O)"] <- "Metals_H2O.csv"
          
          write_csv(SANDS_object$h20_GIS_tab, file.path(tempdir(), "Metals_H2O_GIS.csv"), na = "")
          
          file_names["Metals(H2O)_GIS"] <- "Metals_H2O_GIS.csv"
          
        }
      }
      
      if (is.data.frame(SANDS_object$sed_tab)){
        if (nrow(SANDS_object$sed_tab) > 0){
          
          write_csv(SANDS_object$sed_tab, file.path(tempdir(), "Metals_Sediment.csv"), na = "")
          
          file_names["Metals(Sediment)"] <- "Metals_Sediment.csv"
          
          write_csv(SANDS_object$sed_GIS_tab, file.path(tempdir(), "Metals_Sediment_GIS.csv"), na = "")
          
          file_names["Metals(Sediment)_GIS"] <- "Metals_Sediment_GIS.csv"
          
        }
      }
      
      if (is.data.frame(SANDS_object$salinity_tab)){
        if (nrow(SANDS_object$salinity_tab) > 0){
          
          write_csv(SANDS_object$salinity_tab, file.path(tempdir(), "Salinity.csv"), na = "")
          
          file_names["Salinity"] <- "Salinity.csv"
          
          write_csv(SANDS_object$salinity_GIS_tab, file.path(tempdir(), "Salinity_GIS.csv"), na = "")
          
          file_names["Salinity_GIS"] <- "Salinity_GIS.csv"
          
        }
      }
      
      if (is.data.frame(SANDS_object$oil_tab)){
        if (nrow(SANDS_object$oil_tab) > 0){
          
          write_csv(SANDS_object$oil_tab, file.path(tempdir(), "Oil_Gas.csv"), na = "")
          
          file_names["Oil_Gas"] <- "Oil_Gas.csv"
          
          write_csv(SANDS_object$oil_GIS_tab, file.path(tempdir(), "Oil_Gas_GIS.csv"), na = "")
          
          file_names["Oil_Gas_GIS"] <- "Oil_Gas_GIS.csv"
          
        }
      }
      
      zip::zip(con, files = map_chr(file_names, function(x) file.path(tempdir(), x)),
               mode = "cherry-pick")
      
      removeModal()
    }
  )

  ### Save the data
  output$data_download <- downloadHandler(
    filename = function() {
      paste0("WQP_data_", Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(reVal$WQP_dat2, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  output$data_download2 <- downloadHandler(
    filename = function() {
      paste0("WQP_data_selected_", Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(plot_dat(), file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  # ## Clear the work space
  # observe({
  #   
  #   output$Date_Info <- renderText({
  #   })
  #   
  #   output$SANDS_Date_Info <- renderText({
  #   })
  #   
  #   output$raw_summary_table <- renderDT({
  #   })
  #   
  #   output$clean_summary_table <- renderDT({
  #     
  #   })
  #   
  #   output$Date_Info2 <- renderText({
  #   })
  #   
  #   # In the Assessment tab
  #   output$Date_Info3 <- renderText({
  #   })
  #   
  #   output$file_indicator <- renderText({
  #     
  #   })
  #   
  #   output$criteria_indicator <- renderText({
  #     
  #   })
  #   
  #   output$Par_Select <- renderDT({
  #     
  #   })
  #   
  #   output$site_table <- renderDT({
  #     
  #   })
  #   
  #   output$site_map <- renderLeaflet({
  #     
  #   })
  #   
  #   output$time_plot <- renderPlotly({
  #     
  #   })
  #   
  #   output$box_plot <- renderPlotly({
  #     
  #   })
  #   
  #   output$parameter_name1 <- renderText({
  #     
  #   })
  #   
  #   output$parameter_name2 <- renderText({
  #     
  #   })
  #   
  #   output$parameter_name3 <- renderText({
  #     
  #   })
  #   
  #   output$parameter_name4 <- renderText({
  #     
  #   })
  #   
  # }) %>%
  #   bindEvent(input$Reset)
  
  observe({
    reVal$download <- FALSE 
    reVal$upload <- FALSE
    reVal$WQP_dat <- NULL
    reVal$download_date <- NULL
    reVal$min_date <- NULL
    reVal$max_date <- NULL
    reVal$site_number <- NULL
    reVal$sample_size <- NULL
    reVal$WQP_AutoClean <- NULL
    reVal$WQP_SimpleCensoredMethods <- NULL
    reVal$WQP_FlagMethod <- NULL
    reVal$WQP_FlagSpeciation <- NULL
    reVal$WQP_FlagResultUnit <- NULL
    reVal$WQP_FlagFraction <- NULL
    reVal$WQP_FlagCoordinates <- NULL
    reVal$WQP_FindQCActivities <- NULL
    reVal$WQP_FlagMeasureQualifierCode <- NULL
    reVal$WQP_FindPotentialDuplicatesSingleOrg <- NULL
    reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- NULL
    reVal$WQP_FindQAPPApproval <- NULL
    reVal$WQP_FindQAPPDoc <- NULL
    reVal$WQP_FindContinuousData <- NULL
    reVal$WQP_FlagAboveThreshold <- NULL
    reVal$WQP_FlagBelowThreshold <- NULL
    reVal$WQP_HarmonizeSynonyms <- NULL
    reVal$WQP_depth_combine <- NULL
    reVal$WQP_dat2 <- NULL
    reVal$WQP_dat3 <- NULL
    reVal$min_date2 <- NULL
    reVal$max_date2 <- NULL
    reVal$site_number2 <- NULL
    reVal$sample_size2 <- NULL
    reVal$WQP_site <- NULL
    reVal$WQP_site2 <- NULL
    reVal$site_summary <- NULL
    reVal$ex <- NULL
    reVal$ex_sum <- NULL
    
    bbox_reVal$w_num <- NULL
    bbox_reVal$s_num <- NULL
    bbox_reVal$e_num <- NULL
    bbox_reVal$n_num <- NULL
    
    par_value$variable <- NULL
    
    HUC_list$Selected <- character(0)
    
    SANDS_object$site_temp <- NULL
    SANDS_object$site_temp1 <- NULL
    SANDS_object$result_temp <- NULL
    SANDS_object$narrow_temp <- NULL
    SANDS_object$download_date <- NULL
    SANDS_object$min_date <- NULL
    SANDS_object$max_date <- NULL
    SANDS_object$site_number <- NULL
    SANDS_object$sample_size <- NULL
    SANDS_object$Date_info <- NULL
    SANDS_object$site_temp2 <- NULL
    SANDS_object$result_temp2 <- NULL
    SANDS_object$narrow_temp2 <- NULL
    SANDS_object$summary_temp <- NULL
    SANDS_object$h20_tab <- NULL
    SANDS_object$sed_tab <- NULL
    SANDS_object$salinity_tab <- NULL
    SANDS_object$oil_tab <- NULL
    SANDS_object$nutrients_tab <- NULL
    SANDS_object$TADA_site_temp <- NULL
    SANDS_object$resultphyschem_temp <- NULL
    SANDS_object$SANDS_dat <- NULL
    
    
  }) %>%
    bindEvent(input$Reset)
  
  observeEvent(input$Reset,{
    reset()
    updateRadioGroupButtons(session = session,
                            inputId = "out_se", selected = "Individual Parameter")
    updateMultiInput(session = session, inputId = "par_se")
    updateRadioGroupButtons(session = session,
                            inputId = "area_se", selected = "State")
    updateSelectizeInput(session = session,
                         inputId = "par_list", choices = character(0))
    updateSelectizeInput(session = session,
                         inputId = "fraction_list", choices = character(0))
    updateSelectizeInput(session = session,
                         inputId = "state_cri", choices = c("Montana", "Wyoming"))
    updateSelectizeInput(session = session,
                         inputId = "use_cri", 
                         choices = c("Aquatic Life", "Human Health", "Trigger Value"))
    updateSelectizeInput(session = session,
                         inputId = "details_cri", 
                         choices = c("Acute", "Chronic", "Fish and Drinking Water", 
                                     "Fish", "Trigger Value", "Surface Water"))
    updateCheckboxGroupButtons(session = session,
                               inputId = "MT_par_group",
                               label = "Select the filter groups",
                               choices = c("Metals (Water Column)" = "metal_w", 
                                           "Metals (Sediment)" = "metal_s", 
                                           "Salinity" = "salinity", 
                                           "Oil & Gas" = "oil_gas", 
                                           "Nutrients" = "nutrients"))
    updateSelectizeInput(session, inputId = "site_ID_se", choices = NULL)
    updateSelectizeInput(session, inputId = "fraction_combined", choices = NULL)
  })
  
  observe({
    reVal$download <- FALSE 
    reVal$upload <- FALSE
    reVal$WQP_dat <- NULL
    reVal$download_date <- NULL
    reVal$min_date <- NULL
    reVal$max_date <- NULL
    reVal$site_number <- NULL
    reVal$sample_size <- NULL
    reVal$WQP_AutoClean <- NULL
    reVal$WQP_SimpleCensoredMethods <- NULL
    reVal$WQP_FlagMethod <- NULL
    reVal$WQP_FlagSpeciation <- NULL
    reVal$WQP_FlagResultUnit <- NULL
    reVal$WQP_FlagFraction <- NULL
    reVal$WQP_FlagCoordinates <- NULL
    reVal$WQP_FindQCActivities <- NULL
    reVal$WQP_FlagMeasureQualifierCode <- NULL
    reVal$WQP_FindPotentialDuplicatesSingleOrg <- NULL
    reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- NULL
    reVal$WQP_FindQAPPApproval <- NULL
    reVal$WQP_FindQAPPDoc <- NULL
    reVal$WQP_FindContinuousData <- NULL
    reVal$WQP_FlagAboveThreshold <- NULL
    reVal$WQP_FlagBelowThreshold <- NULL
    reVal$WQP_HarmonizeSynonyms <- NULL
    reVal$WQP_depth_combine <- NULL
    reVal$WQP_dat2 <- NULL
    reVal$WQP_dat3 <- NULL
    reVal$min_date2 <- NULL
    reVal$max_date2 <- NULL
    reVal$site_number2 <- NULL
    reVal$sample_size2 <- NULL
    reVal$WQP_site <- NULL
    reVal$WQP_site2 <- NULL
    reVal$site_summary <- NULL
    reVal$ex <- NULL
    reVal$ex_sum <- NULL
    
    bbox_reVal$w_num <- NULL
    bbox_reVal$s_num <- NULL
    bbox_reVal$e_num <- NULL
    bbox_reVal$n_num <- NULL
    
    par_value$variable <- NULL
    
    HUC_list$Selected <- character(0)
    
    SANDS_object$site_temp <- NULL
    SANDS_object$site_temp1 <- NULL
    SANDS_object$result_temp <- NULL
    SANDS_object$narrow_temp <- NULL
    SANDS_object$download_date <- NULL
    SANDS_object$min_date <- NULL
    SANDS_object$max_date <- NULL
    SANDS_object$site_number <- NULL
    SANDS_object$sample_size <- NULL
    SANDS_object$Date_info <- NULL
    SANDS_object$site_temp2 <- NULL
    SANDS_object$result_temp2 <- NULL
    SANDS_object$narrow_temp2 <- NULL
    SANDS_object$summary_temp <- NULL
    SANDS_object$h20_tab <- NULL
    SANDS_object$sed_tab <- NULL
    SANDS_object$salinity_tab <- NULL
    SANDS_object$oil_tab <- NULL
    SANDS_object$nutrients_tab <- NULL
    SANDS_object$TADA_site_temp <- NULL
    SANDS_object$resultphyschem_temp <- NULL
    SANDS_object$SANDS_dat <- NULL
    
    
  }) %>%
    bindEvent(input$Reset2)
  
  observeEvent(input$Reset2,{
    reset()
    updateRadioGroupButtons(session = session,
                            inputId = "out_se", selected = "Individual Parameter")
    updateMultiInput(session = session, inputId = "par_se")
    updateRadioGroupButtons(session = session,
                            inputId = "area_se", selected = "State")
    updateSelectizeInput(session = session,
                         inputId = "par_list", choices = character(0))
    updateSelectizeInput(session = session,
                         inputId = "fraction_list", choices = character(0))
    updateSelectizeInput(session = session,
                         inputId = "state_cri", choices = c("Montana", "Wyoming"))
    updateSelectizeInput(session = session,
                         inputId = "use_cri", 
                         choices = c("Aquatic Life", "Human Health", "Trigger Value"))
    updateSelectizeInput(session = session,
                         inputId = "details_cri", 
                         choices = c("Acute", "Chronic", "Fish and Drinking Water", 
                                     "Fish", "Trigger Value", "Surface Water"))
    updateCheckboxGroupButtons(session = session,
                               inputId = "MT_par_group",
                               label = "Select the filter groups",
                               choices = c("Metals (Water Column)" = "metal_w", 
                                           "Metals (Sediment)" = "metal_s", 
                                           "Salinity" = "salinity", 
                                           "Oil & Gas" = "oil_gas", 
                                           "Nutrients" = "nutrients"))
    updateSelectizeInput(session, inputId = "site_ID_se", choices = NULL)
    updateSelectizeInput(session, inputId = "fraction_combined", choices = NULL)
  })
  
  ### The Assessment tab
  
  # Display data information after data cleaning
  observe({
    req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
    output$Date_Info3 <- renderText({
      paste0("There are ", reVal$sample_size2, " records from ", 
             reVal$site_number2, " sites.")
    })
  })
  
  # Update site ID
  observeEvent(input$all_sites, {
    req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
    
    WQP_dat3 <- reVal$WQP_dat3
    
    site_ID <- sort(unique(WQP_dat3$MonitoringLocationIdentifier))
    
    updateSelectizeInput(session = session, inputId = "site_ID_se",
                         choices = site_ID)
    
  })
  
  # Update site ID
  observeEvent(input$all_fractions, {
    req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
    
    WQP_dat3 <- reVal$WQP_dat3
    
    fraction_list <- sort(unique(WQP_dat3$TADA.ResultSampleFractionText))
    
    updateSelectizeInput(session = session, inputId = "fraction_combined",
                         choices = fraction_list)
    
  })
  
  # File upload indicator
  upload_dat <- reactive({
    req(input$file)
    
    if (str_detect(input$file$name, "\\.csv$")){
      temp <- read_csv(input$file$datapath)
    } else {
      temp <- NULL
    }
    return(temp)
  })
  
  # Replace reVal$WQP_dat2 with upload_dat() if upload_dat() meets the requirements
  observe({
    req(upload_dat())
    
    reVal$upload <- TRUE
    reVal$download <- FALSE
    
    if (identical(names(upload_dat()), WQP_cols) & nrow(upload_dat()) > 0){
      reVal$WQP_dat2 <- upload_dat()
    }
  })
  
  output$file_indicator <- renderText({
    if (!reVal$download & !reVal$upload){
      temp_text <- "Require downloading data or uploading a CSV file with the data."
    } else if (reVal$download & !reVal$upload){
      temp_text <- "Downloaded data available."
    } else if (reVal$upload){
      validate(
        need(str_detect(input$file$name, "\\.csv$"), "The uploaded file needs to be a CSV file."),
        need(identical(names(upload_dat()), WQP_cols), "The uploaded file needs to have the specific columns."),
        need(nrow(upload_dat()) > 0, "The uploaded file need to have at least one row.")
      )
      temp_text <- "Data uploaded by users."
    } else {
      temp_text <- ""
    }
    return(temp_text)
  })
  
  # Show type definition
  observeEvent(input$type_ex, {
    showModal(modalDialog(
      strong("Type 1:"),
      p("The criteria is only evaluated when pH is between 6.5 to 9.0."),
      strong("Type 2:"),
      p("The criteria applies except where the receiving water after mixing has a pH >= 7.0 and a CaCO3 hardness >= 50 mg/L, where the 750 µg/L acute criterion will apply."),
      strong("Type 3:"),
      p("The criteria is dynamically based on the hardness."),
      strong("Type 4:"),
      p("The criteria is a fixed value."),
      strong("Type 5:"),
      p("The criteria is dynamically based on pH."),
      strong("Type 6:"),
      p("The criteria is dynamically based on pH and temperature."),
      strong("Type 7:"),
      p("No criteria information."),
      title = "The Criteria Type Definition ", footer = NULL, easyClose = TRUE))
  })
  
  ### Calculate the criteria
  
  observe({
    req(reVal$WQP_dat3)
    
    dat <- reVal$WQP_dat3
    
    # Add pH, temperature, and Hardness data
    dat <- dat %>% 
      column_name_change() %>%
      dateTime_create() %>%
      pH_fun() %>% 
      temp_fun() %>%
      hardness_fun() %>%
      temperature_pH_flag()
    
    # Filter the site ID
    if (!input$all_sites){
      dat <- dat %>%
        fsubset(MonitoringLocationIdentifier %in% input$site_ID_se)
    }
    
    # Filter the fraction
    if (!input$all_fractions){
      dat <- dat %>%
        fsubset(Standard_Fraction %in% input$fraction_combined) %>%
        fmutate(Standard_Fraction_ori = Standard_Fraction) %>%
        fmutate(Standard_Fraction = "Combined")
    }
    
    # Join criteria table
    dat <- dat %>%
      criteria_join(criteria_table)
    
    # Separate the data with and without criteria
    dat_criteria <- dat %>% 
      fsubset(!is.na(Use) & !is.na(Details)) 
    dat_no_criteria <- dat %>% 
      fsubset(is.na(Use) & is.na(Details)) %>%
      fmutate(Criteria_Upper = NA, 
              Criteria_Lower = NA,
              Type = 7)
    
    # Separate the data based on pH and Hardness_Notes
    group132 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 1 & Hardness_Notes %in% 3 & Temp_Notes %in% 2)
    group222 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 2 & Hardness_Notes %in% 2 & Temp_Notes %in% 2)
    group312 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 3 & Hardness_Notes %in% 1 & Temp_Notes %in% 2)
    group332 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 3 & Hardness_Notes %in% 3 & Temp_Notes %in% 2)
    group431 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 4 & Hardness_Notes %in% 3 & Temp_Notes %in% 1)
    group432 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 4 & Hardness_Notes %in% 3 & Temp_Notes %in% 2)
    
    # Group 132: pH 6.5-9.0 only
    
    # If pH < 6.5 or pH > 9, do not calculate the criteria
    group132_2 <- group132 %>%
      fmutate(Criteria_Upper = ifelse(pH >= 6.5 & pH <= 9, Magnitude_Upper, NA_real_)) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 1)
    
    # Group 222: "Applies except where the receiving water after mixing has 
    # a pH >= 7.0 and a CaCO3 hardness >= 50 mg/L, where the 750 µg/L acute criterion will apply."
    
    # If pH < 7 and hardness >= 50, use 750 ug/L as the criteria
    group222_2 <- group222 %>%
      fmutate(Criteria_Upper = case_when(
        pH >= 7 & Hardness >= 50                  ~750,
        is.na(pH) | is.na(Hardness)               ~NA_real_,
        TRUE                                      ~Magnitude_Upper
      )) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 2)
    
    group312_2 <- group312 %>%
      left_join(Equation_table %>% fselect(-Fraction), 
                by = c("StateName" = "State", 
                                       "StateAbbrev",
                                       "Standard_Name" = "Constituent",
                                       # "Standard_Fraction" = "Fraction",
                                       "Use", "Details")) %>%
      fmutate(Criteria_Upper = pmap_dbl(list(hardness = Hardness, 
                                       CF_A = CF_A, 
                                       CF_B = CF_B, 
                                       CF_C = CF_C, 
                                       E_A = E_A, 
                                       E_B = E_B), .f = hardness_criteria_fun)) %>%
      select(-(CF_A:E_B)) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>% 
      fmutate(Type = 3)
    
    # group332: The threshold is in the Magnitude
    group332_2 <- group332 %>% 
      fmutate(Criteria_Upper = Magnitude_Upper) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 4)
    
    # group431: The threshold calculation needs pH and temperature
    group431_2 <- group431 %>%
      fmutate(Criteria_Upper = case_when(
        # Details %in% "Acute, salmonids present"              ~CMC_criteria_fun(pH, salmonid = TRUE),
        # Details %in% "Acute, salmonids absent"               ~CMC_criteria_fun(pH, salmonid = FALSE),
        Details %in% "Chronic, early life stages present"    ~CCC_criteria_fun(pH, Temperature, earlylife = TRUE),
        Details %in% "Chronic, early life stages absent"     ~CCC_criteria_fun(pH, Temperature, earlylife = FALSE),
        TRUE                                                 ~NA_real_
      ))  %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 5)
    
    # group432: The threshold calculation needs pH and temperature
    group432_2 <- group432 %>%
      fmutate(Criteria_Upper = case_when(
        Details %in% "Acute, salmonids present"              ~CMC_criteria_fun(pH, salmonid = TRUE),
        Details %in% "Acute, salmonids absent"               ~CMC_criteria_fun(pH, salmonid = FALSE),
        # Details %in% "Chronic, early life stages present"    ~CCC_criteria_fun(pH, Temperature, earlylife = TRUE),
        # Details %in% "Chronic, early life stages absent"     ~CCC_criteria_fun(pH, Temperature, earlylife = FALSE),
        TRUE                                                 ~NA_real_
      ))  %>%
      fmutate(Type = 6)
    
    # Combine group13_2, group22_2, group31_2, and group33_2
    group_criteria <- rbindlist(list(group132_2, group222_2, group312_2, group332_2, group431_2, group432_2),
                                fill = TRUE)
    
    dat_ex <- group_criteria %>% exceedance_fun()
    
    dat_ex_sum <- dat_ex %>% exceedance_cal()
    
    # A function to prepare the site table for data without criteria
    dat_no_ex <- dat_no_criteria %>% exceedance_fun()
    dat_no_ex_sum <- dat_no_ex %>% exceedance_cal()
    
    ex <- rbindlist(list(dat_ex, dat_no_ex))
    ex_sum <- rbindlist(list(dat_ex_sum, dat_no_ex_sum))
    
    # Save the data
    reVal$ex <- ex
    reVal$ex_sum <- ex_sum
    
  })
  
  # Calculate the site summary
  observe({
    req(reVal$ex_sum)
    dat_criteria <- reVal$ex_sum %>% fsubset(!Type %in% 7)
    dat_no_criteria <- reVal$ex_sum %>% fsubset(Type %in% 7)
    
    site_criteria_summary <- dat_criteria %>% 
      criteria_summary()
    site_no_criteria_summary <- dat_no_criteria %>%
      criteria_no_summary()
    
    site_summary <- rbindlist(list(site_criteria_summary, 
                                   site_no_criteria_summary), fill = TRUE)
    
    reVal$site_summary <- site_summary
    
  })

  # Present the table as a DT
  output$Par_Select <- renderDT({
    req(reVal$site_summary)
    dat <- reVal$site_summary %>%
      fselect(-Standard_Unit) %>%
      fmutate(Standard_Name = as.factor(Standard_Name),
              Standard_Fraction = as.factor(Standard_Fraction),
              Use = factor(Use, levels = c("Aquatic Life", "Human Health", "Trigger Value", "Recretion", "All")), 
              Details = factor(Details, levels = sort(unique(criteria_table$Details))),
              Type = factor(Type, levels = 1:7),
              Frequency = as.factor(Frequency),
              Duration = as.factor(Duration)) %>%
      relocate(Use, .before = Details)
      
    datatable(dat,
              filter = "top", extensions = "Buttons",
              colnames = c("Parameter", "Fraction", 
                           "Use", "Details", "Type", 
                           "Frequency", "Duration",
                           "No. Site", "No. Site Exceedance", 
                           "Exceedance Percentage"),
              selection = "single",
              options = list(scrollX = TRUE,
                             dom = "Bfrtip",
                             pageLength = 5,
                             buttons = list(
                               list(
                                 extend = "csv",
                                 filename = "Par_Summary_Table",
                                 exportOptions = list(
                                   modifier = list(
                                     page = "all"
                                   )
                                 )
                               )
                             ))) %>%
      formatPercentage(columns = "Percentage", digits = 2)
  })
  
  Par_Select_table_proxy <- dataTableProxy("Par_Select")
  
  ### Monitoring Site Selection Table
  
  # Update the table by the parameter criteria table
  
  selected_par <- reactive({
    req(reVal$site_summary, reVal$ex_sum)
    
    if (!is.null(input$Par_Select_rows_selected)){
      sel_dat <- reVal$site_summary %>%
        slice(input$Par_Select_rows_selected)
    } else {
      sel_dat <- reVal$site_summary %>% slice(0)
    }
    return(sel_dat)
  })
  
  # Get the selected parameter
  selected_par_name <- reactive({
    req(reVal$site_summary, reVal$ex_sum, input$Par_Select_rows_selected)

    par_title <- paste0(selected_par()$Standard_Name[1], ", ", 
                        selected_par()$Standard_Fraction[1])
    
    return(par_title)

  })
  
  # Update the parameter names
  output$parameter_name1 <- renderText({
    req(reVal$site_summary, reVal$ex_sum, input$Par_Select_rows_selected,
        selected_par_name())
    return(selected_par_name())
  })
  
  # Update the parameter names
  output$parameter_name2 <- renderText({
    req(reVal$site_summary, reVal$ex_sum, input$Par_Select_rows_selected,
        selected_par_name())
    return(selected_par_name())
  })
  
  # Update the parameter names
  output$parameter_name3 <- renderText({
    req(reVal$site_summary, reVal$ex_sum, input$Par_Select_rows_selected,
        selected_par_name())
    return(selected_par_name())
  })
  
  # Update the parameter names
  output$parameter_name4 <- renderText({
    req(reVal$site_summary, reVal$ex_sum, input$Par_Select_rows_selected,
        selected_par_name())
    return(selected_par_name())
  })
  
  # Update the criteria information
  output$criteria_info <- renderText({
    req(reVal$site_summary, reVal$ex, reVal$ex_sum, input$Par_Select_rows_selected,
        selected_par(), selected_par_name())
    
    dat <- reVal$ex %>%
      semi_join(selected_par(), by = c("Standard_Name", "Standard_Fraction",
                                       "Standard_Unit", "Details", "Use", "Type", 
                                       "Frequency", "Duration"))
        
    if (!all(unique(dat$Standard_Name)[1] %in% c("pH", "Dissolved oxygen (DO)"))){
      if (length(unique(dat$Criteria_Upper)) == 1L){
        criteria <- paste0(round(unique(dat$Criteria_Upper)[1], 2))
      } else {
          criteria_mean <- round(mean(dat$Criteria_Upper, na.rm = TRUE), 2)
          criteria_max <- round(max(dat$Criteria_Upper, na.rm = TRUE), 2)
          criteria_min <- round(min(dat$Criteria_Upper, na.rm = TRUE), 2)

          criteria <- paste0("Average ", criteria_mean, ", with a range from ", criteria_min, " to ", criteria_max)

      }
    } else if (unique(dat$Standard_Name)[1] %in% "Dissolved oxygen (DO)"){
      if (length(unique(dat$Criteria_Lower)) == 1L){
        criteria <- paste0(round(unique(dat$Criteria_Lower)[1], 2))
      } else {
        criteria <- NA_character_
      }
    } else if (unique(dat$Standard_Name)[1] %in% "pH"){
      if (length(unique(dat$Criteria_Upper)) == 1L & length(unique(dat$Criteria_Lower)) == 1L){
        criteria <- paste0(round(unique(dat$Criteria_Lower)[1], 2), " - ",
                           round(unique(dat$Criteria_Upper)[1], 2))
      } else {
        criteria <- NA_character_
      }
    } else {
      criteria <- NA_character_
    }

    temp_text <- paste0("Parameter: ", selected_par()$Standard_Name[1], "\n",
                        "Fraction: ", selected_par()$Standard_Fraction[1], "\n",
                        "Unit: ", selected_par()$Standard_Unit[1], "\n",
                        "Details: ", selected_par()$Details[1], "\n",
                        "Use: ", selected_par()$Use[1], "\n",
                        "Criteria Type: ", selected_par()$Type[1], "\n",
                        "Frequency: ", selected_par()$Frequency[1], "\n",
                        "Duration: ", selected_par()$Duration[1], "\n",
                        "Criteria: ", criteria)
    return(temp_text)
  })
  
  # Prepare the WQP_site table
  observe({
    req(reVal$site_summary, reVal$ex_sum, input$Par_Select_rows_selected)
    
    dat <- reVal$ex_sum
    
    WQP_site <- dat %>%
      semi_join(selected_par(), by = c(
        "Standard_Name", "Standard_Fraction", "Standard_Unit",
        "Details", "Use", "Duration"
      ))
    
    reVal$WQP_site <- WQP_site
    
  })
  
  # Update WQP_site by slider_ex
  observe({
    req(reVal$WQP_site, input$slider_ex, input$Par_Select_rows_selected)
      
      WQP_site2 <- reVal$WQP_site %>% 
        # Select columns
        fselect(MonitoringLocationIdentifier, MonitoringLocationName, LongitudeMeasure, 
                LatitudeMeasure, Size, Exceedance_Size, Percentage) %>%
        filter(Percentage >= input$slider_ex/100 | is.na(Percentage)) 
      
      reVal$WQP_site2 <- WQP_site2
      
  })
    
  output$site_table <- renderDT({
    req(reVal$WQP_site2, input$slider_ex, input$Par_Select_rows_selected)

    datatable(reVal$WQP_site2, filter = "none", rownames = FALSE,
              extensions = "Buttons",
              colnames = c("Station No." = "MonitoringLocationIdentifier",
                           "Station Name" = "MonitoringLocationName",
                           "Longitude" = "LongitudeMeasure",                             
                           "Latitude" = "LatitudeMeasure",
                           "N" = "Size",
                           "No. of Exceedance" = "Exceedance_Size",
                           "Exceedance Percentage" = "Percentage"),
              selection = list(mode = "multiple"),
              options = list(pageLength = 5,
                             lengthMenu = c(5, 10, 15, 20),
                             dom = "Bfrtip",
                             scrollX = TRUE,
                             buttons = list(
                               list(
                                   extend = "csv",
                                   filename = "Site_Table",
                                   exportOptions = list(
                                     modifier = list(
                                       page = "all"
                                     )
                                   )
                                 )
                               ))) %>%
        formatPercentage(columns = "Exceedance Percentage", digits = 2)
  }, server = FALSE)
  
  site_table_proxy <- dataTableProxy("site_table")
  
  ### Map
  
  # Create a subset of reVal$WQP_site2 based on input$site_table_rows_selected
  selected_sites <- reactive({
    req(reVal$WQP_site2, input$Par_Select_rows_selected)
    
    if (!is.null(input$site_table_rows_selected)){
      sel_dat <- reVal$WQP_site2 %>%
        slice(input$site_table_rows_selected)
    } else {
      sel_dat <- reVal$WQP_site2 %>% slice(0)
    }
    return(sel_dat)
  })
  
  site_proxy <- leafletProxy("site_map")
  
  output$site_map <- renderLeaflet({
    req(nrow(reVal$WQP_site2) > 0, input$Par_Select_rows_selected)
    
    # Create an indicator column to show if any data are above limit
    temp_dat <- reVal$WQP_site2 %>%
      mutate(Indicator = ifelse(Exceedance_Size > 0, "Yes", "No")) %>%
      mutate(Indicator = factor(Indicator, levels = c("Yes", "No"))) %>%
      # Create a label column
      mutate(label = paste0("Site No: ", "<strong>", MonitoringLocationIdentifier, "</strong>", "<br/>",
                            "Site Name:", "<strong>", MonitoringLocationName, "</strong>", "<br/>",
                            "N: ", "<strong>", Size, "</strong>", "<br/>",
                            "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                            "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%")) 
    
    
    labs <- as.list(temp_dat$label)
    
    pal_sc <- colorFactor(c("#4292c6", "#f7fbff"), temp_dat$Indicator)
    
    leaflet(temp_dat) %>%
      add_USGS_base() %>%
      addCircleMarkers(lng = ~LongitudeMeasure,
                       lat = ~LatitudeMeasure,
                       layerId = ~MonitoringLocationIdentifier,
                       radius = 8, stroke = TRUE, weight = 1,
                       color = "black",
                       fillColor = ~pal_sc(Indicator),
                       fillOpacity = 1,
                       opacity = 0.5,
                       label = map(labs, HTML),
                       group = "base_map") %>%
      addLegend("topright",
                pal = pal_sc,
                values = temp_dat$Indicator,
                title = "Any Data Above Limit",
                opacity = 1) %>%
      setView(lng = -107.3045253, lat = 44.8494123, zoom = 5)
  })
  
  observe({
    req(reVal$WQP_site2, selected_sites())
    if (nrow(selected_sites()) > 0){
      
      temp_dat <- selected_sites() %>%
        mutate(new_ID = paste0(MonitoringLocationIdentifier, "_new")) %>%
        # Create a label column
        mutate(label = paste0("Site No: ", "<strong>", MonitoringLocationIdentifier, "</strong>", "<br/>",
                              "Site Name:", "<strong>", MonitoringLocationName, "</strong>", "<br/>",
                              "N: ", "<strong>", Size, "</strong>", "<br/>",
                              "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                              "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%")) 
      labs <- as.list(temp_dat$label)
      
      site_proxy %>% clearGroup(group = "highlighted_point")
      site_proxy %>%
        addCircleMarkers(lng = temp_dat$LongitudeMeasure,
                         lat = temp_dat$LatitudeMeasure,
                         layerId = temp_dat$new_ID,
                         radius = 8, stroke = TRUE, weight = 1,
                         color = "black", fillColor = "red",
                         fillOpacity = 1,
                         opacity = 0.5,
                         label = map(labs, HTML),
                         group = "highlighted_point")
      
    } else {
      site_proxy %>% clearGroup(group = "highlighted_point")
    }
  })
  
  # See the map click event
  observeEvent(input$site_map_marker_click, {
    req(reVal$WQP_site2)
    
    temp_map_click_sites <- input$site_map_marker_click$id
    
    # if the temp_map_click_sites ends with "_new", need to remove "_new
    if (str_detect(temp_map_click_sites, regex("_new$"))){
      
      temp_map_remove_sites <- str_remove(temp_map_click_sites, regex("_new$"))
      temp_site_remove_row <- which(reVal$WQP_site2$MonitoringLocationIdentifier %in%
                                      temp_map_remove_sites)
      
      reVal$site_select <- reVal$site_select[!reVal$site_select %in% temp_site_remove_row]
      
    } else {
      
      # Get the row ID based on MonitoringLocationIdentifier
      temp_site_row <- which(reVal$WQP_site2$MonitoringLocationIdentifier %in%
                               temp_map_click_sites)
      
      reVal$site_select <- unique(c(reVal$site_select, 
                                    temp_site_row, 
                                    input$site_table_rows_selected))
    }
    
    # Update the site_table
    site_table_proxy %>%
      selectRows(selected = reVal$site_select)
  })
  
  ### Time series plot
  
  # Subset reVal$WQP_dat6 based on selected_sites
  selected_dat <- reactive({
    req(reVal$WQP_site2, 
        nrow(selected_sites()) > 0, nrow(selected_par()) > 0,
        reVal$ex)
    
    temp_dat <- reVal$ex %>%
      semi_join(selected_par(), by = c(
        "Standard_Name", "Standard_Fraction", "Standard_Unit",
        "Details", "Use", "Frequency", "Duration"
      )) %>%
      fsubset(MonitoringLocationIdentifier %in% selected_sites()$MonitoringLocationIdentifier)
    
    return(temp_dat)
  })
  
  plot_dat <- reactive({
    req(reVal$WQP_site2, nrow(selected_sites()) > 0, selected_dat())
    
    print(selected_dat())
    
    temp_dat <- selected_dat() %>%
      rename(Site = MonitoringLocationIdentifier,
             Value = TADA.ResultMeasureValue) %>%
      fmutate(`ActivityStartTime.Time` = as.character(`ActivityStartTime.Time`)) %>%
      fmutate(`ActivityStartTime.Time` = ifelse(is.na(`ActivityStartTime.Time`),
                                                "00:00:00",
                                                `ActivityStartTime.Time`)) %>%
      fmutate(DateTime = ymd_hms(paste0(ActivityStartDate, " ", `ActivityStartTime.Time`))) %>%
      fmutate(Condition = case_when(
        TADA.CensoredData.Flag %in% "Uncensored" ~ "Uncensored",
        TADA.CensoredData.Flag %in% "Non-Detect" ~ "Non-Detect",
        TADA.CensoredData.Flag %in% "Over-Detect" ~ "Over-Detect",
        TRUE                                      ~"Other"
      )) %>%
      fmutate(Condition = factor(Condition,
                                 levels = c("Uncensored", "Non-Detect", "Over-Detect", "Other"))) %>%
      fmutate(Site = factor(Site)) %>%
      fmutate(Exceedance = ifelse(Exceedance, "Yes", "No")) %>%
      # Round the criteria
      fmutate(Criteria_Upper = round(Criteria_Upper, digits = 2),
              Criteria_Lower = round(Criteria_Lower, digits = 2))
    
    glimpse(temp_dat)
    
    return(temp_dat)
  })
  
  plot_unit <- reactive({
    req(reVal$WQP_site2, nrow(selected_sites()) > 0, selected_dat(), plot_dat())

    temp_unit <- unique(plot_dat()$Standard_Unit)[1]

    return(temp_unit)
  })
  
  output$time_plot <- renderPlotly({
    req(reVal$WQP_site2, nrow(selected_sites()) > 0, selected_dat(), 
        plot_dat(), plot_unit())
    
    temp_dat <- plot_dat() %>%
      mutate(Site_Text = Site)
    
    # Group sites
    if (input$time_group){
      temp_dat <- temp_dat %>%
        mutate(Site = "Combined")
    } else {
      temp_dat <- temp_dat
    }
    
    site_title <- unique(temp_dat$Site)
    
    p <- ggplot(temp_dat, aes(x = DateTime, y = Value, 
                              text = paste("Site:", Site_Text))) +
      geom_point(aes(shape = Condition), color = "blue") +
      scale_x_datetime(name = "") +
      scale_shape_manual(values = c("Uncensored" = 16,
                                    "Non-Detect" = 2,
                                    "Over-Detect" = 5,
                                    "Other" = 7),
                         limits = levels(temp_dat$Condition)) 
    
    if (length(unique(temp_dat$Site)) > 1){
      p <- p +
        facet_grid(Site ~ ., switch = "y")
    } else {
      p <- p +
        ggtitle(site_title)
    }
    
    p <- p +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    # If the Type 3, 5, and 6, plot the criteria data as points
    # Otherwise, plot the criteria as line
    
    if (unique(selected_dat()$Type)[1] %in% c(3, 5, 6)){
      p <- p +
        geom_point(aes(x = DateTime, y = Criteria_Upper), color = "orange", shape = 16) 
    } else if (unique(selected_dat()$Type)[1] %in% c(7)){
      p <- p
    } else if (unique(selected_dat()$Standard_Name)[1] %in% "Dissolved oxygen (DO)"){
      
      Criteria_Lower <- unique(selected_dat()$Criteria_Lower)[1]
      
      p <- p +
        geom_hline(yintercept = Criteria_Lower, color = "orange", linewidth = 1)
    } else if (unique(selected_dat()$Standard_Name)[1] %in% "pH"){ 
      
      Criteria_Lower <- unique(selected_dat()$Criteria_Lower)[1]
      Criteria_Upper <- unique(selected_dat()$Criteria_Upper)[1]
      
      p <- p +
        geom_hline(yintercept = Criteria_Upper, color = "orange", linewidth = 1) +
        geom_hline(yintercept = Criteria_Lower, color = "orange", linewidth = 1)
    } else {
      
      Criteria_Upper <- unique(selected_dat()$Criteria_Upper)[1]
      
      p <- p +
        geom_hline(yintercept = Criteria_Upper, color = "orange", linewidth = 1)
    }
    
    if (input$time_log_trans){
      p <- p +
        scale_y_log10(name = plot_unit())
    } else {
      p <- p +
        scale_y_continuous(name = plot_unit())
    }
    
    # shiny::validate(
    #   need(nrow(selected_dat()) > 1, "Plot not shown as there is only one data point.")
    # )

    gp <- p %>%
      ggplotly() %>%
      layout(legend = list(orientation = 'h'))
    
    return(gp)
  })
  
  
  ###  Box plot plot
  
  output$box_plot <- renderPlotly({
    req(reVal$WQP_site2, nrow(selected_sites()) > 0, selected_dat(), 
        plot_dat(), plot_unit())
    
  　temp_dat <- plot_dat() %>% mutate(Site_Text = Site)
  
    # Group sites
    if (input$box_group){
      temp_dat <- temp_dat %>%
        mutate(Site = "Combined")
    } else {
      temp_dat <- temp_dat
    }
    
    # Select sites with sample size more than 3
    temp_dat1 <- temp_dat  %>%
      group_by(Site) %>%
      filter(n() >= 3) %>%
      ungroup() %>%
      as.data.table()

    if (nrow(temp_dat1) == 0){
      p <- ggplot(temp_dat, aes(x = Site, y = Value)) +
        geom_jitter(aes(shape = Condition,
                        height = 0,
                        text = paste("Site:", Site_Text, "\n",
                                     "DateTime:", DateTime, "\n",
                                     "Value:", Value, "\n",
                                     "Condition:", Condition))) +
        scale_shape_manual(values = c("Uncensored" = 16,
                                      "Non-Detect" = 2,
                                      "Over-Detect" = 5,
                                      "Other" = 7),
                           limits = levels(plot_dat()$Condition))
    } else {
      p <- ggplot(temp_dat1, aes(x = Site, y = Value)) +
        geom_boxplot(outlier.shape = NA,
                     outlier.color = NA,
                     outlier.size = 0,
                     width = 0.2) +
        geom_jitter(dat = temp_dat1, 
                    aes(shape = Condition,
                        height = 0,
                        text = paste("Site:", Site_Text, "\n",
                                     "DateTime:", DateTime, "\n",
                                     "Value:", Value, "\n",
                                     "Condition:", Condition)),
                    width = 0.2) +
        scale_shape_manual(values = c("Uncensored" = 16,
                                      "Non-Detect" = 2,
                                      "Over-Detect" = 5,
                                      "Other" = 7),
                           limits = levels(plot_dat()$Condition))
    }
    
    p <- p +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    if (input$box_log_trans){
      p <- p +
        scale_y_log10(name = plot_unit())
    } else {
      p <- p +
        scale_y_continuous(name = plot_unit())
    }
    
    gp <- p %>%
      ggplotly(tooltip = "text") %>%
      layout(legend = list(orientation = 'h'))
    
    # shiny::validate(
    #  need(nrow(temp_dat1) > 1, "Plot not shown as there is only one data point.")
    # )
    
    if (nrow(temp_dat1) > 1){
      gp <- plotly_build(gp)
      
      for(i in 1:length(gp$x$data)) {
        gp$x$data[1] <- lapply(gp$x$data[1], FUN = function(x){
          x$marker = list(opacity = 0)
          return(x)
        })
      }
    } 
    
    return(gp)
    
  })
  
}

# Run the app
shinyApp(ui, server)