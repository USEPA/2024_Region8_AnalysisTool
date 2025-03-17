# R/mod_data_download.R

#' Data Download Module
#'
#' @description A module to download the data
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_dataDownloadUI <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      title = "Download Data from WQP",
      status = "primary", solidHeader = TRUE,
      width = NULL,
      actionButton(ns("par_download"), "Download the data")
    )
  )
}

#' Data Download Server Function
#'
#' @noRd

mod_dataDownloadServer <- function(id, area_info, area_valid, HUC8_data, param_info, param_valid,
                               parameter_reference_table, get_par_fn,
                               WQP_summary_template, TADA_download_temp) {
  moduleServer(id, function(input, output, session) {
    # Create reactive values to store downloaded data and metadata
    download_results <- reactiveValues(
      WQP_dat = NULL,
      download_date = NULL,
      min_date = NULL,
      max_date = NULL,
      site_number = NULL,
      sample_size = NULL
    )

    # Download action
    observeEvent(input$par_download, {
      req(param_info()$date_range, area_valid(), param_valid())

      shinyCatch({
        download_type <- param_info()$download_type

        # Get the date range from param_info instead of directly from input
        date_range <- param_info()$date_range

        if (download_type %in% c("Characteristic names", "Organic characteristic group", "All data")) {
          # Get parameters based on download type
          if (download_type == "Characteristic names") {
            req(param_info()$parameters)
            # Add error handling for empty parameters
            if (length(param_info()$parameters) == 0) {
              shinyalert(
                title = "Error",
                text = "Please select at least one parameter",
                type = "error"
              )
              return()
            }
            char_par <- get_par_fn(parameter_reference_table, param_info()$parameters)
          } else {
            char_par <- NULL
          }

          if (download_type == "Organic characteristic group") {
            req(param_info()$char_group)
            # Add error handling for empty char_group
            if (length(param_info()$char_group) == 0) {
              shinyalert(
                title = "Error",
                text = "Please select at least one characteristic group",
                type = "error"
              )
              return()
            }
            char_group <- param_info()$char_group
          } else {
            char_group <- NULL
          }

          showModal(modalDialog(title = "Estimating the sample size...", footer = NULL))

          # Calculate year summary for WQP query
          YearSum <- year(Sys.Date()) - year(ymd(date_range[1])) + 1

          if (YearSum <= 1) {
            YearSum2 <- 1
          } else if (YearSum > 1 & YearSum <= 5) {
            YearSum2 <- 5
          } else {
            YearSum2 <- "all"
          }

          # Get data summary based on area type
          area_type <- area_info()$type

          if (area_type == "State") {
            req(area_info()$value)

            if (download_type %in% c("Characteristic names", "Organic characteristic group")) {
              site_type <- param_info()$site_type
              # Add check for empty site_type
              if (length(site_type) == 0) {
                site_type <- NULL
              }

              dat_summary_list <- list(readWQPsummary(
                statecode = area_info()$value,
                summaryYears = YearSum2,
                siteType = site_type
              ))
            } else if (download_type == "All data") {
              dat_summary_list <- list(readWQPsummary(
                statecode = area_info()$value,
                summaryYears = YearSum2
              ))
            }
          } else if (area_type == "HUC8") {
            req(area_info()$value)

            if (download_type %in% c("Characteristic names", "Organic characteristic group")) {
              site_type <- param_info()$site_type
              # Add check for empty site_type
              if (length(site_type) == 0) {
                site_type <- NULL
              }

              dat_summary_list <- purrr::map(area_info()$value, ~readWQPsummary(
                huc = .x,
                summaryYears = YearSum2,
                siteType = site_type
              ))
            } else if (download_type == "All data") {
              dat_summary_list <- purrr::map(area_info()$value, ~readWQPsummary(
                huc = .x,
                summaryYears = YearSum2
              ))
            }
          } else if (area_type == "BBox") {
            bounds <- area_info()$value
            req(bounds$west, bounds$south, bounds$east, bounds$north)
            bBox <- c(bounds$west, bounds$south, bounds$east, bounds$north)

            if (download_type %in% c("Characteristic names", "Organic characteristic group")) {
              site_type <- param_info()$site_type
              # Add check for empty site_type
              if (length(site_type) == 0) {
                site_type <- NULL
              }

              dat_summary_list <- list(readWQPsummary(
                bBox = bBox,
                summaryYears = YearSum2,
                siteType = site_type
              ))
            } else if (download_type == "All data") {
              dat_summary_list <- list(readWQPsummary(
                bBox = bBox,
                summaryYears = YearSum2
              ))
            }
          }

          # Process data summary - Add error handling
          if (length(dat_summary_list) == 0) {
            shinyalert(
              title = "Error",
              text = "No data found for the selected parameters and area",
              type = "error"
            )
            removeModal()
            return()
          }

          dat_summary_list <- purrr::keep(dat_summary_list, .p = function(x) nrow(x) > 0)

          if (length(dat_summary_list) == 0) {
            dat_summary <- WQP_summary_template
          } else {
            dat_summary <- rbindlist(dat_summary_list, fill = TRUE)

            if (nrow(dat_summary) == 0 & ncol(dat_summary) == 0) {
              dat_summary <- WQP_summary_template
            }
          }

          # Filter summary based on parameters
          if (download_type == "Characteristic names") {
            # Check if char_par is not empty
            if (length(char_par) == 0) {
              shinyalert(
                title = "Error",
                text = "No characteristic parameters found",
                type = "error"
              )
              removeModal()
              return()
            }

            dat_summary_filter <- dat_summary %>%
              fsubset(CharacteristicName %in% char_par)
          } else if (download_type == "Organic characteristic group") {
            # Check if char_group is not empty
            if (length(char_group) == 0) {
              shinyalert(
                title = "Error",
                text = "No characteristic group found",
                type = "error"
              )
              removeModal()
              return()
            }

            dat_summary_filter <- dat_summary %>%
              fsubset(CharacteristicType %in% char_group)
          } else if (download_type == "All data") {
            dat_summary_filter <- dat_summary
          }

          # Check if dat_summary_filter has any rows
          if (nrow(dat_summary_filter) == 0) {
            shinyalert(
              title = "No Data Found",
              text = "No data found for the selected parameters and area",
              type = "warning"
            )
            removeModal()
            return()
          }

          # Add check for HUCEightDigitCode column
          if (!"HUCEightDigitCode" %in% names(dat_summary_filter)) {
            shinyalert(
              title = "Error",
              text = "HUC8 code information is missing in the data summary",
              type = "error"
            )
            removeModal()
            return()
          }

          # Add check for unique HUCEightDigitCode
          if (length(unique(dat_summary_filter$HUCEightDigitCode)) == 0) {
            shinyalert(
              title = "Error",
              text = "No HUC8 codes found in the data summary",
              type = "error"
            )
            removeModal()
            return()
          }

          # Use dat_summary_filter to identify HUC8 that needed to be downloaded
          dat_summary2 <- dat_summary %>%
            fsubset(HUCEightDigitCode %in% unique(dat_summary_filter$HUCEightDigitCode))

          # nrow(dat_summary2) > 0, proceed
          if (nrow(dat_summary2) > 0){
            dat_summary3 <- dat_summary2 %>%
              fsubset(YearSummarized >= year(ymd(date_range[1])) &
                        YearSummarized <= year(ymd(date_range[2])))

            # Check if the filtered data has any rows
            if (nrow(dat_summary3) == 0) {
              shinyalert(
                title = "No Data Found",
                text = "No data found for the selected date range",
                type = "warning"
              )
              removeModal()
              return()
            }

            sample_size <- nrow(dat_summary3)

            if (sample_size > 100000){
              shinyalert(
                title = "Process Started",
                text = paste0("The estimated sample size is ",
                              comma(sample_size),
                              ", which is above 100,000 and could be above the memory limit. Please select a smaller geographic area or a shorter period with multiple downloads."),
                type = "warning"
              )
              removeModal()
              return()
            }

            # Check if HUC8_data() is not NULL
            req(HUC8_data())

            # Filter the dat_summary3 by HUC8_data()
            # Add error handling for spatial operations
            tryCatch({
              dat_summary3_sf <- dat_summary3 %>%
                distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                         MonitoringLocationLongitude, HUCEightDigitCode) %>%
                drop_na(MonitoringLocationLongitude) %>%
                drop_na(MonitoringLocationLatitude)

              # Check if there are any rows left after dropping NA values
              if (nrow(dat_summary3_sf) == 0) {
                shinyalert(
                  title = "Error",
                  text = "No valid location data found",
                  type = "error"
                )
                removeModal()
                return()
              }

              dat_summary3_sf <- dat_summary3_sf %>%
                st_as_sf(coords = c("MonitoringLocationLongitude",
                                    "MonitoringLocationLatitude"),
                         crs = 4326) %>%
                st_transform(crs = st_crs(HUC8_data())) %>%
                st_filter(HUC8_data())

              # Check if any points remain after spatial filtering
              if (nrow(dat_summary3_sf) == 0) {
                shinyalert(
                  title = "No Data in Selected Area",
                  text = "No monitoring locations found in the selected HUC8 area",
                  type = "warning"
                )
                removeModal()
                return()
              }
            }, error = function(e) {
              shinyalert(
                title = "Error in Spatial Processing",
                text = paste("Error processing spatial data:", e$message),
                type = "error"
              )
              removeModal()
            })

            removeModal()

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
              site_all2 <- purrr::map_dfr(site_all_list, cumsum_group, col = "ResultCount", threshold = 25000)
              site_all3 <- site_all2 %>% summarized_year()

              progressSweetAlert(
                session = session, id = "myprogress",
                title = "Download Progress",
                display_pct = TRUE, value = 0
              )

              dat_temp_all_list <- list()

              # Download the data as TADA data frame
              for (i in 1:nrow(site_all3)){

                updateProgressBar(
                  session = session,
                  id = "myprogress",
                  value = round(i/nrow(site_all3) * 100, 2)
                )

                HUC_temp <- site_all3$HUCEightDigitCode[i]
                Start_temp <- site_all3$Start[i]
                End_temp <- site_all3$End[i]

                # Use download_type instead of input$download_se
                if (download_type == "Characteristic names"){
                  site_type <- param_info()$site_type
                  # Check for empty site_type
                  if (length(site_type) == 0) {
                    site_type <- NULL
                  }

                  if (length(char_par) <= 5){
                    args_temp <- TADA_args_create(
                      huc = HUC_temp,
                      characteristicName = char_par,
                      startDate = Start_temp,
                      endDate = End_temp,
                      siteType = site_type
                    )
                  } else {
                    args_temp <- TADA_args_create(
                      huc = HUC_temp,
                      startDate = Start_temp,
                      endDate = End_temp,
                      siteType = site_type
                    )
                  }
                } else if (download_type == "Organic characteristic group"){
                  site_type <- param_info()$site_type
                  # Check for empty site_type
                  if (length(site_type) == 0) {
                    site_type <- NULL
                  }

                  args_temp <- TADA_args_create(
                    huc = HUC_temp,
                    characteristicType = char_group,
                    startDate = Start_temp,
                    endDate = End_temp,
                    siteType = site_type
                  )
                } else if (download_type == "All data"){
                  args_temp <- TADA_args_create(
                    huc = HUC_temp,
                    startDate = Start_temp,
                    endDate = End_temp
                  )
                }

                # Add error handling for TADA_DataRetrieval
                tryCatch({
                  output_temp <- TADA_DataRetrieval(
                    startDate = as.character(args_temp[["startDate"]]),
                    endDate =  as.character(args_temp[["endDate"]]),
                    countrycode = args_temp[["countrycode"]],
                    countycode = args_temp[["countycode"]],
                    huc = args_temp[["huc"]],
                    siteid = args_temp[["siteid"]],
                    siteType = args_temp[["siteType"]],
                    characteristicName = args_temp[["characteristicName"]],
                    characteristicType = args_temp[["characteristicType"]],
                    sampleMedia = args_temp[["sampleMedia"]],
                    statecode = args_temp[["statecode"]],
                    organization = args_temp[["organization"]],
                    project = args_temp[["project"]],
                    providers = args_temp[["providers"]],
                    applyautoclean = FALSE
                  )

                  if (is.null(output_temp)){
                    output_temp <- TADA_download_temp
                  }

                  dat_temp_all_list[[i]] <- output_temp
                }, error = function(e) {
                  # Log the error but continue with the loop
                  warning(paste("Error in TADA_DataRetrieval for HUC", HUC_temp, ":", e$message))
                  dat_temp_all_list[[i]] <- TADA_download_temp
                })
              }

              closeSweetAlert(session = session)
              sendSweetAlert(
                session = session,
                title = "Download completed !",
                type = "success"
              )

              # Subset the site data by the bBox
              if (area_type == "BBox"){
                bounds <- area_info()$value
                bBox_sf <- st_bbox(
                  c(
                    xmin = bounds$west,
                    xmax = bounds$east,
                    ymax = bounds$north,
                    ymin = bounds$south
                  ),
                  crs = st_crs(4326)
                ) %>%
                  st_as_sfc()

                dat_summary3_sf <- dat_summary3_sf %>%
                  st_filter(bBox_sf)
              }

              # Check if dat_temp_all_list has any elements
              if (length(dat_temp_all_list) == 0) {
                dat_temp_com <- TADA_download_temp
              } else {
                # Combine all data
                dat_temp_com <- rbindlist(dat_temp_all_list, fill = TRUE)
              }

              # Filter the data with char_par
              if (download_type == "Characteristic names"){
                # Check if CharacteristicName column exists
                if ("CharacteristicName" %in% names(dat_temp_com) && length(char_par) > 0) {
                  dat_temp_com1 <- dat_temp_com %>%
                    fsubset(CharacteristicName %in% char_par)
                } else {
                  dat_temp_com1 <- dat_temp_com
                }
              } else {
                dat_temp_com1 <- dat_temp_com
              }

              # Check if MonitoringLocationIdentifier and ActivityStartDate columns exist
              if (all(c("MonitoringLocationIdentifier", "ActivityStartDate") %in% names(dat_temp_com1)) &&
                  nrow(dat_temp_com1) > 0 &&
                  nrow(dat_summary3_sf) > 0) {

                dat_temp_all <- dat_temp_com1 %>%
                  # Filter with the site ID in the state and dates
                  fsubset(MonitoringLocationIdentifier %in% dat_summary3_sf$MonitoringLocationIdentifier) %>%
                  fsubset(ActivityStartDate >= date_range[1] & ActivityStartDate <= date_range[2])
              } else {
                dat_temp_all <- TADA_download_temp
              }

              # If nrow(site_all) == 0, return an empty data frame with the header as dat_temp_all
            } else {
              dat_temp_all <- TADA_download_temp
            }

            # If nrow(dat_summary) == 0, return an empty data frame with the header as dat_temp_all
          } else {
            dat_temp_all <- TADA_download_temp
          }

          # Store the initial data
          download_results$WQP_dat <- dat_temp_all

          # Update metadata
          download_results$download_date <- format(Sys.time())

          if (nrow(dat_temp_all) > 0) {
            download_results$min_date <- min(dat_temp_all$ActivityStartDate, na.rm = TRUE)
            download_results$max_date <- max(dat_temp_all$ActivityStartDate, na.rm = TRUE)
            download_results$site_number <- length(unique(dat_temp_all$MonitoringLocationIdentifier))
            download_results$sample_size <- nrow(dat_temp_all)
          } else {
            download_results$min_date <- NA
            download_results$max_date <- NA
            download_results$site_number <- 0
            download_results$sample_size <- 0
          }
        }
      }, blocking_level = "error")
    })

    return(download_results)
  })
}
