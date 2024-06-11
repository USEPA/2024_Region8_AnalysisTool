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
library(zip)
library(TADA)
library(tigris)
library(tidycensus)

# Load the data
load("DataInput/InputData25.RData")
load("DataInput/SANDS_input22.RData")

# Load the tabs
db_main_body <- source("external/db_main_body.R")$value
db_main_sb <- source("external/db_main_sb.R")$value
# tab_Intro <- source("external/tab_Intro.R")$value
tab_Download <- source("external/tab_Download.R")$value
tab_AU <- source('external/tab_AU.R')$value
tab_Sufficiency <- source("external/tab_Sufficiency.R")$value
tab_Analysis <- source("external/tab_Analysis.R")$value
tab_SANDS <- source("external/tab_SANDS.R")$value

# Load the functions
source("scripts/Main_helper_functions.R")
source("scripts/SANDS_helper_functoins.R")
source("scripts/AU_helper_functions.R")

# Set the upload size to 5MB
mb_limit <- 5000
options(shiny.maxRequestSize = mb_limit * 1024^2)

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

## tabs
db_main_sb                     <- source("external/db_main_sb.R"
                                         , local = TRUE)$value
db_main_body                   <- source("external/db_main_body.R"
                                         , local = TRUE)$value
# tab_Intro                      <- source("external/tab_Intro.R"
#                                          , local = TRUE)$value
tab_Download                   <- source("external/tab_Download.R"
                                         , local = TRUE)$value
tab_Analysis                   <- source("external/tab_Analysis.R"
                                         , local = TRUE)$value

## Data and files for AU

path_results <- "Results"

url_au_table <- "https://github.com/Blocktt/ShinyAppDocuments/raw/main/AUSpatialJoin"
url_au_table2 <- file.path(url_au_table, "MonLoc_to_AU_Crosswalk_20240415.xlsx")
temp_au_table <- tempfile(fileext = ".xlsx")
httr::GET(url_au_table2, httr::write_disk(temp_au_table))

df_ML2AU_orig <- as.data.frame(readxl::read_excel(temp_au_table))

df_ML2AU <- df_ML2AU_orig %>%
  select(MonitoringLocationIdentifier, AU_ID, AU_NAME, DrinkingWater_Use
         , Ecological_Use, FishConsumption_Use, Recreational_Use, Other_Use)

## AU Shapefiles ####
load(file = "data/GISlayer_streams.rda")
GISlayer_streams_transformed <- streams_shp %>% 
  sf::st_transform(2818) %>%
  select(AU_ID, AU_NAME, drinkingwa, ecological, fishconsum, recreation
         , other_use) %>%  # trim unneccessary columns
  rename(DrinkingWater_Use = drinkingwa
         , Ecological_Use = ecological
         , FishConsumption_Use = fishconsum
         , Recreational_Use = recreation
         , Other_Use = other_use)


load(file = "data/GISlayer_streams_simp.rda")

load(file = "data/GISlayer_lakes.rda")
GISlayer_lakes_transformed <- lakes_shp %>% 
  sf::st_transform(2818) %>%
  select(AU_ID, AU_NAME, drinkingwa, ecological, fishconsum, recreation
         , other_use) %>%  # trim unneccessary columns
  rename(DrinkingWater_Use = drinkingwa
         , Ecological_Use = ecological
         , FishConsumption_Use = fishconsum
         , Recreational_Use = recreation
         , Other_Use = other_use)

## Waterbody types ####
Lake_types <- c("Lake, Reservoir, Impoundment", "Reservoir", "Lake")

Stream_types <- c("Stream", "Stream: Canal", "River/Stream", "Channelized Stream"
                  , "River/Stream Perennial", "River/Stream Intermittent"
                  , "Canal Irrigation", "Canal Drainage")

### UI
ui <- function(request){
  dashboardPage(
    # The header panel
    header = dashboardHeader(title = "WQP Data Tool"),
    # The sidebar panel
    sidebar = dashboardSidebar(db_main_sb("leftsidebarmenu")
    ),
    body = dashboardBody(
      # A call to the use shinyJS package
      useShinyjs(),
      # Set up the contents in the main panel as tabs
      db_main_body("dbBody")
    )
  )
}

### Server
server <- function(input, output, session){
  
  ### The Download tab
  
  observe({
    if(!is.null(reVal$WQP_dat2)){
      shinyjs::show(id = "data_download")
      shinyjs::show(id = "data_download_QC")
    } else {
      shinyjs::hide(id = "data_download")
      shinyjs::hide(id = "data_download_QC")
    }
  })
  
  observe({
    if(input$biological_check &
       !is.null(reVal$bio_dat)){
      shinyjs::show(id = "data_download_bio")
    } else {
      shinyjs::hide(id = "data_download_bio")
    }
  })
  
  observe({
    if(!is.null(input$Par_Select_rows_selected)){
      shinyjs::show(id = "data_download2")
    } else {
      shinyjs::hide(id = "data_download2")
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
    
    if (!input$state_out_bound){
      return(HUC8_WY_MT_simple)
    } else if (input$state_out_bound){
      return(HUC8_WY_MT_out_simple)
    }
  })
  
  # Update the HUC8 list
  observe({
    if (!input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_label)
    } else if (input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_out_label)
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
      variable <- parameter_names$Standard_Name
    } else {
      group_par <- parameter_names %>%
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
  })
  
  # Download the data for individual parameters
  observeEvent(input$par_download, {
    req(input$par_date, input$sitetype_se, location_ind())
    
    if (input$download_se %in% c("Characteristic names", "Organic characteristic group")){
      showModal(modalDialog(title = "Estimating the sample size...", footer = NULL))
      
      if (input$download_se %in% c("Characteristic names")){
        req(input$par_se)
        # Get the parameter
        char_par <- get_par(dat = parameter_reference_table, par = input$par_se)
      } else {
        char_par <- NULL
      }
      
      if (input$download_se %in% c("Organic characteristic group")){
        char_group <- input$CharGroup_se
      } else {
        char_group <- NULL
      }
      
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
      
      # Filter by char_par or CharacteristicType
      if (input$download_se %in% "Characteristic names"){
        dat_summary_filter <- dat_summary %>%
          fsubset(CharacteristicName %in% char_par)
      } else if (input$download_se %in% "Organic characteristic group"){
        dat_summary_filter <- dat_summary %>%
          fsubset(CharacteristicType %in% char_group)
      }
      
      # Use dat_summary_filter to identify HUC8 that needed to be downloaded
      dat_summary2 <- dat_summary %>%
        fsubset(HUCEightDigitCode %in% unique(dat_summary_filter$HUCEightDigitCode))
      
      # Download the biological profile if biological_check is TRUE
      if (input$biological_check){
        dat_summary_filter_b <- dat_summary %>% fsubset(CharacteristicName %in% "Count")
        
        dat_summary2_b <- dat_summary %>%
          fsubset(HUCEightDigitCode 
                  %in% unique(dat_summary_filter_b$HUCEightDigitCode))
        
      } else {
        dat_summary2_b <- dat_summary %>% slice(0)
      }
      
      removeModal()
      
      # nrow(dat_summary2) > 0, proceed
      if (nrow(dat_summary2) > 0){
        dat_summary3 <- dat_summary2 %>%
          fsubset(YearSummarized >= year(ymd(input$par_date[1])) & 
                    YearSummarized <= year(ymd(input$par_date[2])))
        
        if (input$biological_check & nrow(dat_summary2_b) > 0){
          dat_summary3_b <- dat_summary2_b %>%
            fsubset(YearSummarized >= year(ymd(input$par_date[1])) & 
                      YearSummarized <= year(ymd(input$par_date[2])))
        } else {
          dat_summary3_b <- dat_summary2_b %>% slice(0)
        }
        
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
        
        if (input$biological_check & nrow(dat_summary3_b) > 0){
          dat_summary3_b_sf <- dat_summary3_b %>% 
            distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                     MonitoringLocationLongitude, HUCEightDigitCode) %>%
            drop_na(MonitoringLocationLongitude) %>%
            drop_na(MonitoringLocationLatitude) %>%
            st_as_sf(coords = c("MonitoringLocationLongitude",
                                "MonitoringLocationLatitude"),
                     crs = 4326) %>%
            st_filter(HUC8_dat())
        } else {
          dat_summary3_b_sf <- dat_summary3_b %>% slice(0)
        }
        
        # Filter by HUCEightDigitCode
        site_all <- dat_summary3 %>% 
          fsubset(HUCEightDigitCode 
                  %in% unique(dat_summary3_sf$HUCEightDigitCode)) %>%
          group_by(HUCEightDigitCode, YearSummarized) %>%
          fsummarize(ResultCount = fsum(ResultCount)) %>%
          fungroup() %>%
          arrange(HUCEightDigitCode, YearSummarized) 
        
        if (input$biological_check & 
            nrow(dat_summary3_b) > 0 & 
            nrow(dat_summary3_b_sf) > 0){
          site_all_b <- dat_summary3_b %>% 
            fsubset(HUCEightDigitCode 
                    %in% unique(dat_summary3_b_sf$HUCEightDigitCode)) %>%
            group_by(HUCEightDigitCode, YearSummarized) %>%
            fsummarize(ResultCount = fsum(ResultCount)) %>%
            fungroup() %>%
            arrange(HUCEightDigitCode, YearSummarized) 
        } else{
          site_all_b <- dat_summary3_b %>% slice(0)
        }
        
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
          
          # Download resultPhysChem, project, and site profile
          
          for (i in 1:nrow(site_all3)){
            
            updateProgressBar(
              session = session,
              id = "myprogress",
              value = round(i/nrow(site_all3) * 100, 2)
            )
            
            HUC_temp <- site_all3$HUCEightDigitCode[i]
            Start_temp <- site_all3$Start[i]
            End_temp <- site_all3$End[i]
            
            if (input$download_se %in% "Characteristic names"){
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
            } else if (input$download_se %in% "Organic characteristic group"){
              args_temp <- args_create(
                huc = HUC_temp,
                characteristicType = char_group,
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
          
          # Filter the data with char_par 
          if (input$download_se %in% "Characteristic names"){
            dat_temp_com1 <- dat_temp_com %>%
              fsubset(CharacteristicName %in% char_par)
          } else if (input$download_se %in% "Organic characteristic group"){
            dat_temp_com1 <- dat_temp_com 
          }
          
          dat_temp_all <- dat_temp_com1 %>%
            # Filter with the site ID in the state and dates
            fsubset(MonitoringLocationIdentifier %in% dat_summary3_sf$MonitoringLocationIdentifier) %>%
            fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
          
          # If nrow(site_all) == 0, return an empty data frame with the header as dat_temp_all
        } else {
          
          dat_temp_all <- TADA_download_temp
          
        }
        
        # If nrow(site_all_b) > 0, download the data
        if (nrow(site_all_b) > 0){
          
          site_all_b_list <- site_all_b %>% split(.$HUCEightDigitCode)
          site_all_b2 <- map_dfr(site_all_b_list, cumsum_group, col = "ResultCount", threshold = 25000)
          site_all_b3 <- site_all_b2 %>% summarized_year()
          
          
          progressSweetAlert(
            session = session, id = "myprogress_bio",
            title = "Download Progress (macroinvertbrate count data)",
            display_pct = TRUE, value = 0
          )
          
          biological_temp_all_list <- list()
          
          # Download biological_temp profile
          
          for (j in 1:nrow(site_all_b3)){
            
            updateProgressBar(
              session = session,
              id = "myprogress_bio",
              value = round(j/nrow(site_all_b3) * 100, 2)
            )
            
            HUC_temp <- site_all_b3$HUCEightDigitCode[j]
            Start_temp <- site_all_b3$Start[j]
            End_temp <- site_all_b3$End[j]
            
            args_temp <- args_create(
              huc = HUC_temp,
              characteristicName = "Count",
              startDateLo = Start_temp,
              startDateHi = End_temp,
              siteType = input$sitetype_se
            )
            
            biological_temp <- biological_download(args_temp, ref = biological_template)
            
            if (is.null(biological_temp)){
              biological_temp <- biological_template
            }
            
            biological_temp_all_list[[j]] <- biological_temp
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
            
            dat_summary3_b_sf <- dat_summary3_b_sf %>%
              st_filter(bBox_sf)
          }
          
          biological_temp_com <- rbindlist(biological_temp_all_list, fill = TRUE)
          
          biological_temp_all <- biological_temp_com %>%
            # Filter with the site ID in the state and dates
            fsubset(MonitoringLocationIdentifier %in% dat_summary3_sf$MonitoringLocationIdentifier) %>%
            fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
          
          # If nrow(site_all) == 0, return an empty data frame with the header as dat_temp_all
        } else {
          
          biological_temp_all <- biological_template
          
        }
        
        # If nrow(dat_summary) == 0, return an empty data frame with the header as dat_temp_all
      } else {
        
        dat_temp_all <- TADA_download_temp
        biological_temp_all <- biological_template
      }
      
      reVal$WQP_dat <- dat_temp_all
      reVal$bio_dat <- biological_temp_all
      reVal$download <- TRUE
      reVal$upload <- FALSE
    }
    
  })
  
  ### Clean up the bio_dat
  observe({
    req(reVal$bio_dat)
    bio_temp <- reVal$bio_dat %>%
      # Remove duplicates
      distinct() %>%
      # Remove records without scientific names
      fsubset(!is.na(SubjectTaxonomicName)) %>%
      # Filter the data with macroinvertebrate
      fsubset(AssemblageSampledName %in% "Benthic Macroinvertebrates") %>%
      # Filter for ActivityMediaName as Biological
      fsubset(ActivityMediaName %in% "Biological") %>%
      # Filter the BiologicalIntentName that is not "Tissue"
      fsubset(!BiologicalIntentName %in% "Tissue")
    
    reVal$bio_dat2 <- bio_temp
  })
  
  
  ### Update the UI for data summary
  output$summary_box <- renderUI({
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
              ),
              if (input$biological_check){
                fluidRow(
                  column(
                    width = 12,
                    h2("Macroinvertebrate Count Data Summary"),
                    fluidRow(
                      column(
                        width = 6,
                        verbatimTextOutput(outputId = "Date_Info_bio", placeholder = TRUE)
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        selectInput(inputId = "bio_summary", label = "Data attribute to summarize",
                                    choices = c("Taxon" = "SubjectTaxonomicName",
                                                "Biological Intent Name" = "BiologicalIntentName",
                                                "Sample Status" = "ResultStatusIdentifier"),
                                    multiple = TRUE)
                      ),
                      column(
                        width = 9,
                        DTOutput(outputId = "raw_summary_bio_table")
                      )
                    )
                  )
                )
              }
            )
          )
        )
      )
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
  
  ### Get the macroinvertebrate count data summary
  observe({
    req(reVal$bio_dat2)
    
    # Store the download date
    download_date <- format(Sys.time())
    
    # Site number and sample size
    site_number <- length(unique(reVal$bio_dat2$MonitoringLocationIdentifier))
    sample_size <- nrow(reVal$bio_dat2)
    
    # Min and Max Date
    if (sample_size == 0){
      min_date <- ""
      max_date <- ""
    } else {
      min_date <- min(reVal$bio_dat2$ActivityStartDate, na.rm = TRUE)
      max_date <- max(reVal$bio_dat2$ActivityStartDate, na.rm = TRUE) 
    }
    
    # Save the value
    reVal$bio_download_date <- download_date
    reVal$bio_min_date <- min_date
    reVal$bio_max_date <- max_date
    reVal$bio_site_number <- site_number
    reVal$bio_sample_size <- sample_size
  })
  
  observe({
    req(reVal$bio_dat2, reVal$bio_sample_size)
    output$Date_Info_bio <- renderText({
      req(reVal$bio_dat2, reVal$bio_sample_size)
      if (reVal$bio_sample_size == 0){
        paste0("Data are downloaded at ", reVal$bio_download_date, ".\n",
               "There are ", reVal$bio_sample_size, " records from ", reVal$bio_site_number, " sites.\n") 
      } else {
        paste0("Data are downloaded at ", reVal$bio_download_date, ".\n",
               "The date range is from ", reVal$bio_min_date, " to ", reVal$bio_max_date, ".\n",
               "There are ", reVal$bio_sample_size, " records from ", reVal$bio_site_number, " sites.\n") 
      }
    }) 
  })
  
  ### Summarize the macroinvertebrate count data
  
  observe({
    req(reVal$bio_dat2, input$bio_summary)
    output$raw_summary_bio_table <- renderDT({
      tt <- count_fun(reVal$bio_dat2, input$bio_summary)
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
    
    reVal$WQP_AutoClean <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_SimpleCensoredMethods <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagMethod <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagSpeciation <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagResultUnit <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagFraction <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagCoordinates <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FindQCActivities <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagMeasureQualifierCode <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FindPotentialDuplicatesSingleOrg <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FindQAPPApproval <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FindQAPPDoc <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FindContinuousData <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagAboveThreshold <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_FlagBelowThreshold <- NULL
    
    removeModal()
    
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
    
    reVal$WQP_HarmonizeSynonyms <- NULL
    
    removeModal()
    
  })
  
  # TADA_join_tribal
  observe({
    req(reVal$WQP_depth_combine)
    
    showModal(modalDialog(title = "Join Tribal Information.", 
                          footer = NULL))
    
    temp_dat <- TADA_join_tribal_poss(
      TADA_dat = reVal$WQP_depth_combine,
      tribal_sf = MT_tribal
    )
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_join_tribal_temp
    }
    
    reVal$WQP_join_tribal <- temp_dat
    
    reVal$WQP_depth_combine <- NULL
    
    removeModal()
    
  })
  
  # Select the correct columns to create WQP_dat2
  observe({
    req(reVal$WQP_join_tribal)
    
    temp_dat <- reVal$WQP_join_tribal %>%
      TADA_selector_poss()
    
    if (is.null(temp_dat)){
      temp_dat <- TADA_selector_temp
    }
    
    reVal$WQP_join_tribal <- NULL
    
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
    
    # Prepare a data frame for the Join AU tab
    observe({
      req(reVal$WQP_dat2, nrow(reVal$WQP_dat2) > 0)
      reVal$WQP_AU <- reVal$WQP_dat2 %>% TADA_site_simplify()
      reVal$WQP_dat2_time <- Sys.time()
    })
    
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
  
  # Filter the data for the QC data
  observe({
    req(reVal$WQP_dat3)
    
    reVal$WQP_QC <- reVal$WQP_dat3
    
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
  
  output$data_download_QC <- downloadHandler(
    filename = function() {
      paste0("WQP_data_", Sys.Date(), '_QC.csv', sep='')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(reVal$WQP_QC, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  output$data_download_bio <- downloadHandler(
    filename = function() {
      paste0("WQP_data_", Sys.Date(), '_macro_count.csv', sep='')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(reVal$bio_dat2, file = con, na = "", bom = TRUE)
      
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
  
  observe({
    reVal$download <- FALSE 
    reVal$upload <- FALSE
    reVal$WQP_dat <- NULL
    reVal$bio_dat <- NULL
    reVal$bio_dat2 <- NULL
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
    reVal$WQP_join_tribal <- NULL
    reVal$WQP_dat2 <- NULL
    reVal$WQP_dat2_time <- NULL
    reVal$WQP_AU <- NULL
    reVal$WQP_dat3 <- NULL
    reVal$WQP_QC <- NULL
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
    
    AU_Val$df_import <- NULL
    AU_Val$df_import_time <- NULL
    AU_Val$df_import2 <- NULL
    AU_Val$download <- FALSE
    AU_Val$upload <- FALSE
    AU_Val$WQP_dat <- NULL
    
    SUS_Val$upload <- FALSE
    
  }) %>%
    bindEvent(input$Reset)
  
  observeEvent(input$Reset,{
    reset()
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
    reVal$bio_dat <- NULL
    reVal$bio_dat2 <- NULL
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
    reVal$WQP_join_tribal <- NULL
    reVal$WQP_dat2 <- NULL
    reVal$WQP_dat2_time <- NULL
    reVal$WQP_AU <- NULL
    reVal$WQP_dat3 <- NULL
    reVal$WQP_QC <- NULL
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
    
    AU_Val$df_import <- NULL
    AU_Val$df_import_time <- NULL
    AU_Val$df_import2 <- NULL
    AU_Val$download <- FALSE
    AU_Val$upload <- FALSE
    AU_Val$WQP_dat <- NULL
    
    SUS_Val$upload <- FALSE
    
  }) %>%
    bindEvent(input$Reset2)
  
  observeEvent(input$Reset2,{
    reset()
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
  
  ### Server functions for the AU Join tab
  
  AU_Val <- reactiveValues()

  # # Data 
  # output$AU_file_indicator <- renderText({
  #   if (!AU_Val$download & !AU_Val$upload){
  #     temp_text <- "Require downloading data or uploading a CSV file with the data."
  #   } else if (AU_Val$download & ! AU_Val$upload){
  #     temp_text <- "Downloaded data available."
  #   } else if (AU_Val$upload){
  #     validate(
  #       need(str_detect(input$fn_input$name, "\\.csv$"), "The uploaded file needs to be a CSV file."),
  #       need(identical(names(df_import()), AU_cols), "The uploaded file needs to have the specific columns."),
  #       need(nrow(df_import()) > 0, "The uploaded file need to have at least one row.")
  #     )
  #     temp_text <- "Data uploaded by users."
  #   } else {
  #     temp_text <- ""
  #   }
  #   return(temp_text)
  # })
  
  # Misc Names----
  output$fn_input_display <- renderText({
    inFile <- input$fn_input
    
    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END
    
    return(paste0("'", inFile$name, "'"))
  }) ## renderText~END
  
  # Import ####
  
  # ## File Watch ----
  # file_watch <- reactive({
  #   # trigger for df_import()
  #   input$fn_input
  # })## file_watch
  
  ## Import, df_import ####
  observe({
    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    req(input$fn_input)
    
    inFile <- input$fn_input
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      sep_user <- input$sep
      
      # Define file
      fn_inFile <- inFile$datapath
      
      #message(getwd())
      message(paste0("Import, separator: '", input$sep,"'"))
      message(paste0("Import, file name: ", input$fn_input$name))
      
      # Read user imported file
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      
      required_columns <- c("MonitoringLocationIdentifier", "LatitudeMeasure"
                            , "LongitudeMeasure")
      
      # QC Check for column names
      column_names <- colnames(df_input)
      col_req_match <- required_columns %in% column_names
      col_missing <- required_columns[!col_req_match]
      
      shiny::validate(
        need(all(required_columns %in% column_names)
             , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                      , paste("Required columns missing from the data:\n")
                      , paste("* ", col_missing, collapse = "\n")))
      )##END ~ validate() code      # Add "Results" folder if missing
      
      AU_Val$df_import <- df_input
      AU_Val$df_import_time <- Sys.time()
      
    }
    
  })## df_import
  
  observe({
    if (is.null(AU_Val$df_import) & is.null(reVal$WQP_AU)) {
      AU_Val$df_import2 <- NULL
      AU_Val$download <- FALSE
      AU_Val$upload <- FALSE
    } else if (is.null(reVal$WQP_AU) & !is.null(AU_Val$df_import)) {
      AU_Val$df_import2 <- AU_Val$df_import
      AU_Val$download <- FALSE
      AU_Val$upload <- TRUE
    } else if (!is.null(reVal$WQP_AU) & is.null(AU_Val$df_import)) {
      AU_Val$df_import2 <- reVal$WQP_AU
      AU_Val$download <- TRUE
      AU_Val$upload <- FALSE
    } else {
      if (reVal$WQP_dat2_time > AU_Val$df_import_time) {
        AU_Val$df_import2 <- reVal$WQP_AU
        AU_Val$download <- TRUE
        AU_Val$upload <- FALSE
      } else {
        AU_Val$df_import2 <- AU_Val$df_import
        AU_Val$download <- FALSE
        AU_Val$upload <- TRUE
      }
    }
  })
  
  ## Import table ####
  output$df_import_DT <- DT::renderDT({
    req(AU_Val$df_import2)
    
    boo_Results <- dir.exists(file.path(path_results))
    if (boo_Results == FALSE) {
      dir.create(file.path(path_results))
    }
    
    # Remove all files in "Results" folder
    fn_results <- list.files(file.path(path_results), full.names = TRUE)
    file.remove(fn_results)
    
    df_data <- AU_Val$df_import2
    
  }##expression~END
  , filter = "top"
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100)
                   , autoWidth = TRUE)
  
  )##output$df_import_DT~END
  
  # b_Calc, AUjoin ####
  shiny::observeEvent(input$b_Calc, {
    shiny::withProgress({
      
      # Number of increments
      n_inc <- 10
      
      # sink output
      file_sink <- file(file.path(".", "Results", "results_Log.txt")
                        , open = "wt")
      sink(file_sink, type = "output", append = TRUE)
      sink(file_sink, type = "message", append = TRUE)
      
      # Log
      message("Results Log from AU Spatial Join App")
      message(Sys.time())
      
      if (AU_Val$upload & !AU_Val$download){
        inFile <- input$fn_input
        AU_filename <- inFile$name
      } else if (!AU_Val$upload & AU_Val$download){
        AU_filename <- "WQP"
      }
      
      message(paste0("Imported file name: ", AU_filename))
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Data, Initialize")
      Sys.sleep(0.25)
      
      # Import data
      # data
      df_MonLocData <- AU_Val$df_import2
        
      # QC, FAIL if TRUE
      if (is.null(df_MonLocData)) {
        return(NULL)
      }
      
      ## 1. Join crosswalk ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Join, crosswalk")
      Sys.sleep(0.25)
      
      df_joinedMonLocAU <- dplyr::left_join(df_MonLocData, df_ML2AU
                                            , by = "MonitoringLocationIdentifier")
      
      # 2. Filter matched MonLoc ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Data, Filter")
      Sys.sleep(0.25)
      
      df_MonLocMatched <- df_joinedMonLocAU %>%
        filter(!is.na(AU_ID))
      
      # 3. Unmatched MonLoc by Type ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Data, Prepare Unmatched")
      Sys.sleep(0.25)
      
      df_Lake_Unmatched <- df_joinedMonLocAU %>%
        filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Lake_types)) %>% 
        select(-c(AU_ID, AU_NAME, DrinkingWater_Use, Ecological_Use
                  , FishConsumption_Use, Recreational_Use, Other_Use))
      
      df_Stream_Unmatched <- df_joinedMonLocAU %>%
        filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Stream_types)) %>% 
        select(-c(AU_ID, AU_NAME, DrinkingWater_Use, Ecological_Use
                  , FishConsumption_Use, Recreational_Use, Other_Use))
      
      # 4. Spatial Joins ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Spatial Join, Lakes")
      Sys.sleep(0.25)
      
      ### 4a. Lake Spatial Join ####
      
      # QC check
      num_sites <- nrow(df_Lake_Unmatched)
      
      # only run spatial join if there are sites
      if(num_sites == 0){
        message(paste("There are NO lake monitoring locations missing AU data."))
        
        df_Lake_SpatJoin_Final <- NULL
        
      } else {
        message(paste("There are", num_sites, "lake monitoring locations missing AU data."))
        
        ### convert to geospatial layer (sf object)
        lakes_pts <- sf::st_as_sf(x = df_Lake_Unmatched, coords = c("LongitudeMeasure"
                                                                    ,"LatitudeMeasure")
                                  , crs = "+proj=longlat +datum=WGS84")%>%
          sf::st_transform(st_crs(GISlayer_lakes_transformed))
        
        ### spatial join
        lake_SpatJoin <- sf::st_join(lakes_pts, GISlayer_lakes_transformed
                                     , join = st_nearest_feature) # join points and AUs
        
        ### determine distance (m) between points and nearest feature
        near_feat <- sf::st_nearest_feature(lakes_pts, GISlayer_lakes_transformed)
        Dist_to_AU_m <- sf::st_distance(lakes_pts, GISlayer_lakes_transformed[near_feat,]
                                        , by_element = TRUE)
        
        ### join distance measurements to join results
        lake_SpatJoin2 <- cbind(lake_SpatJoin, Dist_to_AU_m)
        
        ### results and export data
        df_Lake_SpatJoin_Final <- lake_SpatJoin2 %>%
          sf::st_transform(4326) %>%
          mutate(LongitudeMeasure = unlist(map(geometry,1)),
                 LatitudeMeasure = unlist(map(geometry,2))) %>%
          sf::st_drop_geometry()
        
      }# end if/else statement
      
      
      ### 4b. Stream Spatial Join ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Spatial Join, Streams")
      Sys.sleep(0.25)
      
      # QC check
      num_sites <- nrow(df_Stream_Unmatched)
      
      # only run spatial join if there are sites
      if(num_sites == 0){
        message(paste("There are NO stream monitoring locations missing AU data."))
        
        df_Stream_SpatJoin_Final <- NULL
        
      } else {
        message(paste("There are", num_sites, "stream monitoring locations missing AU data."))
        
        ### convert to geospatial layer (sf object)
        streams_pts <- sf::st_as_sf(x = df_Stream_Unmatched, coords = c("LongitudeMeasure"
                                                                        ,"LatitudeMeasure")
                                    , crs = "+proj=longlat +datum=WGS84")%>%
          sf::st_transform(st_crs(GISlayer_streams_transformed))
        
        ### spatial join
        stream_SpatJoin <- sf::st_join(streams_pts, GISlayer_streams_transformed
                                       , join = st_nearest_feature) # join points and AUs
        # select(MonitoringLocationIdentifier, MonitoringLocationName
        #        , MonitoringLocationTypeName, State, AU_ID, AU_NAME) # trim unneccessary columns
        
        ### determine distance (m) between points and nearest feature
        near_feat <- sf::st_nearest_feature(streams_pts, GISlayer_streams_transformed)
        Dist_to_AU_m <- sf::st_distance(streams_pts, GISlayer_streams_transformed[near_feat,]
                                        , by_element = TRUE)
        
        ### join distance measurements to join results
        stream_SpatJoin2 <- cbind(stream_SpatJoin, Dist_to_AU_m)
        
        ### results and export data
        df_Stream_SpatJoin_Final <- stream_SpatJoin2 %>%
          sf::st_transform(4326) %>%
          mutate(LongitudeMeasure = unlist(map(geometry,1)),
                 LatitudeMeasure = unlist(map(geometry,2))) %>%
          sf::st_drop_geometry()
        
      }# end if/else statement
      
      ### 4c. Join data ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Spatial Join, Combine")
      Sys.sleep(0.25)
      
      if (is.null(df_Lake_SpatJoin_Final) & is.null(df_Stream_SpatJoin_Final)){
        df_SpatJoin_Final <- NULL # both datasets are NULL
      } else if (!is.null(df_Lake_SpatJoin_Final) 
                 & is.null(df_Stream_SpatJoin_Final)) {
        df_SpatJoin_Final <- df_Lake_SpatJoin_Final # only streams dataset is NULL
      } else if (is.null(df_Lake_SpatJoin_Final) 
                 & !is.null(df_Stream_SpatJoin_Final)) {
        df_SpatJoin_Final <- df_Stream_SpatJoin_Final # only lakes dataset is NULL
      } else {
        df_SpatJoin_Final <- rbind(df_Lake_SpatJoin_Final, df_Stream_SpatJoin_Final)
        # both datasets have data
      }# END ~ IF/ELSE
      
      ### 4d. Join to matched data ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Data, Combine all")
      Sys.sleep(0.25)
      
      df_MonLocMatched$Dist_to_AU_m <- 0
      
      if (is.null(df_SpatJoin_Final)){
        df_MonLocAU_4Map <- df_MonLocMatched %>% 
          mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                        , TRUE ~ "Matched")) %>%
          select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
                    , Recreational_Use, Other_Use))
      } else {
        df_MonLocAU_4Map <- rbind(df_MonLocMatched, df_SpatJoin_Final)%>% 
          mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                        , TRUE ~ "Matched")) %>%
          select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
                    , Recreational_Use, Other_Use))
      } # END ~ if/else
      
      ## 5. Display data ####
      ### 5a. Table ####
      output$df_results_DT <- DT::renderDT({
        
        if (is.null(df_MonLocAU_4Map)) {
          return(NULL)
        }##IF~is.null~END
        
        # Filter df_MonLocAU_4Map based on input_site_choice
        filtered_df <- df_MonLocAU_4Map
        
        if (input$input_site_choice != "") {
          mySite <- input$input_site_choice
          
          filtered_df <- df_MonLocAU_4Map %>% 
            filter(MonitoringLocationIdentifier == mySite)
        }##IF~END
        
        # Return the filtered dataframe
        return(filtered_df)
        
        # # datatable(df_MonLocAU_4Map, selection = "single")
        # return(df_MonLocAU_4Map)
        
      }##expression~END
      , selection = "single"
      , filter = "top"
      , options = list(scrollX = TRUE
                       , pageLength = 5
                       , lengthMenu = c(5, 10, 25, 50, 100)
                       , autoWidth = TRUE) 
      
      )##output$df_import_DT~END
      
      # browser()
      
      ### 5b. Counts ####
      # Define a reactive value for storing the row count
      row_count_matched <- reactiveVal(0)
      row_count_unmatched_G50 <- reactiveVal(0)
      row_count_unmatched_L50 <- reactiveVal(0)
      
      # Observe the changes in df_MonLocAU_4Map and update the row count
      observe({
        req(df_MonLocAU_4Map)
        nMatched <- sum(df_MonLocAU_4Map$MatchGroup == "Matched")
        nUnmatched_G50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                              & df_MonLocAU_4Map$Dist_to_AU_m >= 50)
        nUnmatched_L50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                              & df_MonLocAU_4Map$Dist_to_AU_m < 50)
        row_count_matched(nMatched)
        row_count_unmatched_G50(nUnmatched_G50)
        row_count_unmatched_L50(nUnmatched_L50)
      })# END ~ observe
      
      # Render the valueBox based on the row count
      output$MatchCount <- renderValueBox({
        valueBox(value = row_count_matched()
                 , subtitle = "Number of Monitoring Locations matched to AU crosswalk table"
                 , icon = icon("hashtag")
                 , color = "aqua"
        )
      })# END ~ renderValueBox
      
      output$UnMatchCount_G50 <- renderValueBox({
        valueBox(value = row_count_unmatched_G50()
                 , subtitle = "Number of Monitoring Locations with AU spatial join >= 50m"
                 , icon = icon("hashtag")
                 , color = "red"
        )
      })# END ~ renderValueBox
      
      output$UnMatchCount_L50 <- renderValueBox({
        valueBox(value = row_count_unmatched_L50()
                 , subtitle = "Number of Monitoring Locations with AU spatial join < 50m"
                 , icon = icon("hashtag")
                 , color = "yellow"
        )
      })# END ~ renderValueBox
      
      ## 6. Map ####
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Display results, ~15 sec")
      Sys.sleep(0.25)
      
      output$mymap <- renderLeaflet({
        req(df_MonLocAU_4Map)
        
        # Subset data by Match group
        data_match <- df_MonLocAU_4Map %>% 
          filter(MatchGroup == "Matched")
        
        data_unmatch <- df_MonLocAU_4Map %>% 
          filter(MatchGroup == "UnMatched")
        
        leaflet("mymap") %>%
          addTiles() %>%
          addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>% 
          addProviderTiles(providers$Esri.WorldImagery, group = "Esri Ortho") %>% 
          addCircleMarkers(data = data_match, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                           , group = "Matched Sites"
                           , popup = paste("SiteID:", data_match$MonitoringLocationIdentifier, "<br>"
                                           ,"Site Name:", data_match$MonitoringLocationName, "<br>"
                                           ,"Site Type:", data_match$MonitoringLocationTypeName, "<br>"
                                           ,"State:", data_match$State, "<br>"
                                           ,"AU_ID:", data_match$AU_ID, "<br>"
                                           ,"AU NAME:", data_match$AU_NAME)
                           , color = "black", fillColor = "blue", fillOpacity = 1, stroke = TRUE
                           # , clusterOptions = markerClusterOptions()
          ) %>% 
          addCircleMarkers(data = data_unmatch, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                           , group = "Unmatched Sites"
                           , popup = paste("SiteID:", data_unmatch$MonitoringLocationIdentifier, "<br>"
                                           ,"Site Name:", data_unmatch$MonitoringLocationName, "<br>"
                                           ,"Site Type:", data_unmatch$MonitoringLocationTypeName, "<br>"
                                           ,"State:", data_unmatch$State, "<br>"
                                           ,"AU_ID:", data_unmatch$AU_ID, "<br>"
                                           ,"AU NAME:", data_unmatch$AU_NAME)
                           , color = "black", fillColor = "red", fillOpacity = 1, stroke = TRUE
                           # , clusterOptions = markerClusterOptions()
          ) %>%
          addPolylines(data = streams_simp_shp, color = "blue", weight = 3
                       , label = streams_simp_shp$AU_ID, group = "River AUs"
                       , popup = paste("<b> AU_ID:</b>", streams_simp_shp$AU_ID, "<br>"
                                       ,"<b> AU_Name:</b>", streams_simp_shp$AU_NAME)) %>%
          addPolygons(data = lakes_shp, color = "black", weight = 2, opacity = 1
                      , fillColor = "#df65b0", fillOpacity = 0.25, group = "Lake AUs"
                      , popup = paste("<b> AU_ID:</b>", lakes_shp$AU_ID, "<br>"
                                      ,"<b> AU_Name:</b>", lakes_shp$AU_NAME)) %>%
          addLayersControl(overlayGroups = c("Matched Sites", "Unmatched Sites"
                                             , "River AUs", "Lake AUs")
                           ,baseGroups = c("Esri WSM", "Esri Ortho")
                           ,options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Matched Sites", "Unmatched Sites"
                      , "River AUs", "Lake AUs")) %>%
          addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                     , position = "bottomright")
      }) # END ~ renderLeaflet
      
      ## Map Zoom ####
      # Map that filters output data to a single location
      observeEvent(input$input_site_choice, {
        req(input$input_site_choice != "")
        
        mySite <- input$input_site_choice
        
        df_MonLocAU_select <- df_MonLocAU_4Map %>% 
          filter(MonitoringLocationIdentifier == mySite)
        
        site_long <- df_MonLocAU_select$Longitude # longitude
        site_lat <- df_MonLocAU_select$Latitude # latitude
        
        # modfiy map
        leafletProxy("mymap") %>%
          # setView(lng = site_long, lat = site_lat, zoom = 12)
          flyTo(lng = site_long, lat = site_lat, zoom = 16)
      })#observeEvent ~ END
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Process completed")
      Sys.sleep(0.25)
      
      # 7. Save results ####
      # Save, Matched
      fn_results <- "MonitoringLocations_AU_Matched.csv"
      dn_results <- path_results
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_MonLocMatched, pn_results, row.names = FALSE)
      
      # Save, SpatJoin
      fn_results <- "MonitoringLocations_AU_SpatialJoin.csv"
      dn_results <- path_results
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_SpatJoin_Final, pn_results, row.names = FALSE)
      
      # Save, Import Data
      if (AU_Val$upload & !AU_Val$download){
        inFile <- input$fn_input
        AU_filename <- inFile$name
      } else if (!AU_Val$upload & AU_Val$download){
        AU_filename <- paste0("WQP_", Sys.Date(), ".csv")
      }
      
      fn_results <- AU_filename
      dn_results <- path_results
      pn_results <- file.path(dn_results, fn_results)
      write.csv(AU_Val$df_import2, pn_results, row.names = FALSE)
      
      # Create zip file of results
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)
      
      # button, enable, download
      # shinyjs::enable("b_download")
      
      # Pop up ####
      n_match <- sum(df_MonLocAU_4Map$MatchGroup == "Matched")
      n_joinG50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                       & df_MonLocAU_4Map$Dist_to_AU_m >= 50)
      n_joinL50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                       & df_MonLocAU_4Map$Dist_to_AU_m < 50)
      msg <- paste0("The AU join is complete, but results take ~30 seconds to render in the app!"
                    , " Please be patient and wait for the map to render to use in QC review of spatial joins.", "\n\n"
                    , "Number of sites matched to AUs = ", n_match, "\n\n"
                    , "Number of sites spatially joined to AUs (<50m distance) = ", n_joinL50, "\n\n"
                    , "Number of sites spatially joined to AUs (>=50m distance) = ", n_joinG50, "\n\n")
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE
                             , size = "m")
      # validate(msg)
      
      # Join the AU assessment data to the reVal$WQP_dat2
      observe({
        req(reVal$WQP_dat2)
        
        # df_AU_join <- bind_rows(df_MonLocMatched, df_SpatJoin_Final)
        
        WQP_AU_join <- reVal$WQP_dat2 %>%
          left_join(df_MonLocAU_4Map %>%
                      fselect(-LatitudeMeasure, -LongitudeMeasure), 
                    by = c("MonitoringLocationIdentifier",
                                             "StateName" = "State",
                                             "MonitoringLocationName",
                                             "MonitoringLocationTypeName"))
        
        AU_Val$WQP_dat <- WQP_AU_join
        
      })
      
    }) #END ~ withProgress
  }) # END ~ observeEvent
  
  ## Download data ####
  output$b_download <- downloadHandler(
    
    filename = function() {
      
      if (AU_Val$upload & !AU_Val$download){
        inFile <- input$fn_input
        AU_filename <- inFile$name
      } else if (!AU_Val$upload & AU_Val$download){
        AU_filename <- "WQP"
      }
      
      fn_input_base <- tools::file_path_sans_ext(AU_filename)
      paste0(fn_input_base
             , "_results_"
             , format(Sys.Date(), "%Y%m%d")
             , ".zip")
    } ,
    content = function(fname) {##content~START
      
      file.copy(file.path(path_results, "results.zip"), fname)
      
    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans
  
  output$WQP_AU_Download <- downloadHandler(
    filename = function() {
      paste0("WQP_data_AU_", Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(AU_Val$WQP_dat, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  ### Data Sufficiency
  
  # Display the upload table
  
  SUS_Val <- reactiveValues()
  
  observe({
    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    req(input$sus_fn_input)
    
    inFile <- input$sus_fn_input
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      # Define file
      sus_fn_inFile <- inFile$datapath
      
      # Read user imported file
      sus_df_input <- read_csv(sus_fn_inFile)
      
      required_columns <- names(data_sus)
      
      # QC Check for column names
      column_names <- colnames(sus_df_input)
      col_req_match <- required_columns %in% column_names
      col_missing <- required_columns[!col_req_match]
      
      shiny::validate(
        need(all(required_columns %in% column_names)
             , paste0("You may have missing required columns\n"
                      , paste("Required columns missing from the data:\n")
                      , paste("* ", col_missing, collapse = "\n")))
      )##END ~ validate() code      # Add "Results" folder if missing
      
      SUS_Val$upload <- TRUE
      SUS_Val$sus_df_input <- sus_df_input 
      
    }
    
    # Determine the Data Sufficiency Table
    observe({
      if (SUS_Val$upload){
        SUS_Val$sus_dat <- SUS_Val$sus_df_input
      } else {
        SUS_Val$sus_dat <- data_sus
      }
    })
    
    # Display the Data Sufficiency Table
    output$Sus_DT <- renderDT({
      req(SUS_Val$sus_dat)
      datatable(SUS_Val$sus_dat,
                filter = "top",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthMenu = c(5, 10, 25, 50, 100), autoWidth = TRUE)) %>%
        formatStyle(names(SUS_Val$sus_dat), lineHeight = '70%')
    })
    
    # Convert Characteristic Names, Fraction
    observe({
      req(AU_Val$WQP_dat)
      
      temp_dat <- AU_Val$WQP_dat
      
      temp_dat2 <- temp_dat 
        
      
    })
    
    # Join the data sufficiency table to the WQP data
    
  })
}

# Run the app
shinyApp(ui, server)