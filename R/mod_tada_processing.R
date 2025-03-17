# R/mod_tada_processing.R

#' TADA Processing Module
#'
#' @description A module to select parameters
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

tadaProcessingServer <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to hold processed data at different stages
    processed_data <- reactiveValues(
      WQP_dat2 = NULL,   # Final processed data
      WQP_dat3 = NULL,   # QA filtered data
      WQP_QC = NULL      # QC data
    )

    # TADA processing chain
    # Each step is observed and triggered when the previous step completes

    # 1. TADA_AutoClean
    observe({
      req(raw_data$WQP_dat)

      showModal(modalDialog(title = "Run TADA_AutoClean", footer = NULL))

      temp_dat <- TADA_AutoClean_poss(.data = raw_data$WQP_dat)

      if (is.null(temp_dat)) {
        temp_dat <- TADA_AutoClean_temp
      }

      processed_data$WQP_AutoClean <- temp_dat
      raw_data$WQP_dat <- NULL  # Clean up to save memory

      removeModal()
    })

    # 2. TADA_SimpleCensoredMethods
    observe({
      req(processed_data$WQP_AutoClean)

      showModal(modalDialog(title = "Run TADA_SimpleCensoredMethods", footer = NULL))

      temp_dat <- TADA_SimpleCensoredMethods_poss(
        .data = processed_data$WQP_AutoClean,
        nd_multiplier = 1
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_SimpleCensoredMethods_temp
      }

      processed_data$WQP_SimpleCensoredMethods <- temp_dat
      processed_data$WQP_AutoClean <- NULL

      removeModal()
    })

    # Continue with other TADA processing steps...
    # Each step follows the same pattern as above
    # For brevity, I'm not including all steps, but you would add them here

    # Example of another step:
    observe({
      req(processed_data$WQP_SimpleCensoredMethods)

      showModal(modalDialog(title = "Run TADA_FlagMethod", footer = NULL))

      temp_dat <- TADA_FlagMethod_poss(
        .data = processed_data$WQP_SimpleCensoredMethods,
        clean = FALSE
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagMethod_temp
      }

      processed_data$WQP_FlagMethod <- temp_dat
      processed_data$WQP_SimpleCensoredMethods <- NULL

      removeModal()
    })

    # Additional steps would follow

    # tada_processing_module.R (continued)

    # 4. TADA_FlagSpeciation
    observe({
      req(processed_data$WQP_FlagMethod)

      showModal(modalDialog(title = "Run TADA_FlagSpeciation", footer = NULL))

      temp_dat <- TADA_FlagSpeciation_poss(
        .data = processed_data$WQP_FlagMethod,
        clean = "none"
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagSpeciation_temp
      }

      processed_data$WQP_FlagSpeciation <- temp_dat
      processed_data$WQP_FlagMethod <- NULL

      removeModal()
    })

    # 5. TADA_FlagResultUnit
    observe({
      req(processed_data$WQP_FlagSpeciation)

      showModal(modalDialog(title = "Run TADA_FlagResultUnit", footer = NULL))

      temp_dat <- TADA_FlagResultUnit_poss(
        .data = processed_data$WQP_FlagSpeciation,
        clean = "none"
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagResultUnit_temp
      }

      processed_data$WQP_FlagResultUnit <- temp_dat
      processed_data$WQP_FlagSpeciation <- NULL

      removeModal()
    })

    # 6. TADA_FlagFraction
    observe({
      req(processed_data$WQP_FlagResultUnit)

      showModal(modalDialog(title = "Run TADA_FlagFraction", footer = NULL))

      temp_dat <- TADA_FlagFraction_poss(
        .data = processed_data$WQP_FlagResultUnit,
        clean = FALSE
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagFraction_temp
      }

      processed_data$WQP_FlagFraction <- temp_dat
      processed_data$WQP_FlagResultUnit <- NULL

      removeModal()
    })

    # 7. TADA_FlagCoordinates
    observe({
      req(processed_data$WQP_FlagFraction)

      showModal(modalDialog(title = "Run TADA_FlagCoordinates", footer = NULL))

      temp_dat <- TADA_FlagCoordinates_poss(
        .data = processed_data$WQP_FlagFraction
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagCoordinates_temp
      }

      processed_data$WQP_FlagCoordinates <- temp_dat
      processed_data$WQP_FlagFraction <- NULL

      removeModal()
    })

    # 8. TADA_FindQCActivities
    observe({
      req(processed_data$WQP_FlagCoordinates)

      showModal(modalDialog(title = "Run TADA_FindQCActivities", footer = NULL))

      temp_dat <- TADA_FindQCActivities_poss(
        .data = processed_data$WQP_FlagCoordinates
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FindQCActivities_temp
      }

      processed_data$WQP_FindQCActivities <- temp_dat
      processed_data$WQP_FlagCoordinates <- NULL

      removeModal()
    })

    # 9. TADA_FlagMeasureQualifierCode
    observe({
      req(processed_data$WQP_FindQCActivities)

      showModal(modalDialog(title = "Run TADA_FlagMeasureQualifierCode", footer = NULL))

      temp_dat <- TADA_FlagMeasureQualifierCode_poss(
        .data = processed_data$WQP_FindQCActivities
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagMeasureQualifierCode_temp
      }

      processed_data$WQP_FlagMeasureQualifierCode <- temp_dat
      processed_data$WQP_FindQCActivities <- NULL

      removeModal()
    })

    # 10. TADA_FindPotentialDuplicatesSingleOrg
    observe({
      req(processed_data$WQP_FlagMeasureQualifierCode)

      showModal(modalDialog(title = "Run TADA_FindPotentialDuplicatesSingleOrg", footer = NULL))

      temp_dat <- TADA_FindPotentialDuplicatesSingleOrg_poss(
        .data = processed_data$WQP_FlagMeasureQualifierCode
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FindPotentialDuplicatesSingleOrg_temp
      }

      processed_data$WQP_FindPotentialDuplicatesSingleOrg <- temp_dat
      processed_data$WQP_FlagMeasureQualifierCode <- NULL

      removeModal()
    })

    # 11. TADA_FindPotentialDuplicatesMultipleOrgs
    observe({
      req(processed_data$WQP_FindPotentialDuplicatesSingleOrg)

      showModal(modalDialog(title = "Run TADA_FindPotentialDuplicatesMultipleOrgs", footer = NULL))

      temp_dat <- TADA_FindPotentialDuplicatesMultipleOrgs_poss(
        .data = processed_data$WQP_FindPotentialDuplicatesSingleOrg
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FindPotentialDuplicatesMultipleOrgs_temp
      }

      processed_data$WQP_FindPotentialDuplicatesMultipleOrgs <- temp_dat
      processed_data$WQP_FindPotentialDuplicatesSingleOrg <- NULL

      removeModal()
    })

    # 12. TADA_FindQAPPApproval
    observe({
      req(processed_data$WQP_FindPotentialDuplicatesMultipleOrgs)

      showModal(modalDialog(title = "Run TADA_FindQAPPApproval", footer = NULL))

      temp_dat <- TADA_FindQAPPApproval_poss(
        .data = processed_data$WQP_FindPotentialDuplicatesMultipleOrgs
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FindQAPPApproval_temp
      }

      processed_data$WQP_FindQAPPApproval <- temp_dat
      processed_data$WQP_FindPotentialDuplicatesMultipleOrgs <- NULL

      removeModal()
    })

    # 13. TADA_FindQAPPDoc
    observe({
      req(processed_data$WQP_FindQAPPApproval)

      showModal(modalDialog(title = "Run TADA_FindQAPPDoc", footer = NULL))

      temp_dat <- TADA_FindQAPPDoc_poss(
        .data = processed_data$WQP_FindQAPPApproval
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FindQAPPDoc_temp
      }

      processed_data$WQP_FindQAPPDoc <- temp_dat
      processed_data$WQP_FindQAPPApproval <- NULL

      removeModal()
    })

    # 14. TADA_FlagContinuousData
    observe({
      req(processed_data$WQP_FindQAPPDoc)

      showModal(modalDialog(title = "Run TADA_FlagContinuousData", footer = NULL))

      temp_dat <- TADA_FlagContinuousData_poss(
        .data = processed_data$WQP_FindQAPPDoc
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagContinuousData_temp
      }

      processed_data$WQP_FlagContinuousData <- temp_dat
      processed_data$WQP_FindQAPPDoc <- NULL

      removeModal()
    })

    # 15. TADA_FlagAboveThreshold
    observe({
      req(processed_data$WQP_FlagContinuousData)

      showModal(modalDialog(title = "Run TADA_FlagAboveThreshold", footer = NULL))

      temp_dat <- TADA_FlagAboveThreshold_poss(
        .data = processed_data$WQP_FlagContinuousData
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagAboveThreshold_temp
      }

      processed_data$WQP_FlagAboveThreshold <- temp_dat
      processed_data$WQP_FlagContinuousData <- NULL

      removeModal()
    })

    # 16. TADA_FlagBelowThreshold
    observe({
      req(processed_data$WQP_FlagAboveThreshold)

      showModal(modalDialog(title = "Run TADA_FlagBelowThreshold", footer = NULL))

      temp_dat <- TADA_FlagBelowThreshold_poss(
        .data = processed_data$WQP_FlagAboveThreshold
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_FlagBelowThreshold_temp
      }

      processed_data$WQP_FlagBelowThreshold <- temp_dat
      processed_data$WQP_FlagAboveThreshold <- NULL

      removeModal()
    })

    # 17. TADA_HarmonizeSynonyms
    observe({
      req(processed_data$WQP_FlagBelowThreshold)

      showModal(modalDialog(title = "Run TADA_HarmonizeSynonyms", footer = NULL))

      temp_dat <- TADA_HarmonizeSynonyms_poss(
        .data = processed_data$WQP_FlagBelowThreshold
      )

      if (is.null(temp_dat)) {
        temp_dat <- TADA_HarmonizeSynonyms_temp
      }

      processed_data$WQP_HarmonizeSynonyms <- temp_dat
      processed_data$WQP_FlagBelowThreshold <- NULL

      removeModal()
    })

    # Final step: Select correct columns to create WQP_dat2
    observe({
      req(processed_data$WQP_HarmonizeSynonyms)

      temp_dat <- processed_data$WQP_HarmonizeSynonyms %>%
        TADA_selector_poss()

      if (is.null(temp_dat)) {
        temp_dat <- TADA_Final_temp
      }

      processed_data$WQP_dat2 <- temp_dat
      processed_data$WQP_HarmonizeSynonyms <- NULL
    })

    # Filter data for QA criteria
    observe({
      req(processed_data$WQP_dat2)

      # Save the download information
      processed_data$download_date <- round(Sys.time())
      processed_data$min_date <- min(processed_data$WQP_dat2$ActivityStartDate)
      processed_data$max_date <- max(processed_data$WQP_dat2$ActivityStartDate)
      processed_data$site_number <- length(unique(processed_data$WQP_dat2$MonitoringLocationIdentifier))
      processed_data$sample_size <- nrow(processed_data$WQP_dat2)

      processed_data$WQP_dat3 <- processed_data$WQP_dat2 %>% QA_filter()

      # Calculate metadata for filtered data
      if (nrow(processed_data$WQP_dat3) > 0) {
        processed_data$min_date2 <- min(processed_data$WQP_dat3$ActivityStartDate)
        processed_data$max_date2 <- max(processed_data$WQP_dat3$ActivityStartDate)
        processed_data$site_number2 <- length(unique(processed_data$WQP_dat3$MonitoringLocationIdentifier))
        processed_data$sample_size2 <- nrow(processed_data$WQP_dat3)
      }
    })

    # Store QC data
    observe({
      req(processed_data$WQP_dat3)
      processed_data$WQP_QC <- processed_data$WQP_dat3
    })

    return(processed_data)
  })
}

