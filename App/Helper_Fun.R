### Helper functions

# Load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(collapse)
library(dataRetrieval)
library(tigris)
library(TADA)

### A helper function to count the data
count_fun <- function(dat, ...){
  dat2 <- dat %>%
    count(across(all_of(...))) %>%
    arrange(desc(n)) %>%
    mutate(`Percent (%)` = n/sum(n))
  
  # Replace NA with ""
  dat2[is.na(dat2)] <- ""
  
  return(dat2)
}

### A helper function to get the parameter names
get_par <- function(dat, par){
  dat2 <- dat %>%
    filter(Standard_Name %in% par)
  variable <- dat2$Original_Name
  return(variable)
}

### A function to download data from the Water Quality Portal

# Create a possible version of the readWQPdata
readWQPdata_poss <- possibly(readWQPdata)

# A function to construct the argument list
args_create <- function(statecode = NULL, huc = NULL, 
                        bBox = NULL,
                        project = NULL,
                        startDateLo = NULL, startDateHi = NULL,
                        siteType = NULL,
                        siteid = NULL,
                        characteristicName = NULL,
                        characteristicType = NULL){
  # Construct the arguments for downloads
  args <- list(
    "startDateLo" = startDateLo,
    "startDateHi" = startDateHi,
    "characteristicName" = characteristicName,
    "characteristicType" = characteristicType,
    "statecode" = statecode,
    "siteType" = siteType,
    "huc" = huc,
    "bBox" = bBox,
    "siteid" = siteid,
    "project" = project 
  )
  
  # Remove NULL attribute
  args <- args[map_lgl(args, function(x) !is.null(x))]
  
  return(args)
}

# A function to pull site data
site_download <- function(args, ref){
  site <- readWQPdata_poss(args, service = "Station", ignore_attributes = TRUE)
  
  if (is.null(site)|nrow(site) == 0L){
    site <- ref
  }
  
  return(site)
}

# A function to download the result
result_download <- function(args, ref){
  result <- readWQPdata_poss(args, ignore_attributes = TRUE)
  
  if (is.null(result)|nrow(result) == 0L){
    result <- ref
  }
  
  return(result)
}

# A function to download the resultPhysChem
resultPhysChem_download <- function(args, ref){
  resultPhysChem <- readWQPdata_poss(args, dataProfile = "resultPhysChem",
                             ignore_attributes = TRUE)
  
  if (is.null(resultPhysChem)|nrow(resultPhysChem) == 0L){
    resultPhysChem <- ref
  }
  
  return(resultPhysChem)
}

# A function to download the Narrow data
narrow_download <- function(args, ref){
  narrow <- readWQPdata_poss(args, dataProfile = "narrowResult", ignore_attributes = TRUE)
  
  if (is.null(narrow)|nrow(narrow) == 0L){
    narrow <- ref
  }
  
  return(narrow)
}

# A function to download the project data
project_download <- function(args, ref){
  project <- readWQPdata_poss(args, service = "Project", ignore_attributes = TRUE)

  if (is.null(project)){
    project <- ref
  }

  return(project)
}

# A function to create a TADA data frame based on site, result, and project
TADA_join3 <- function(site, resultphyschem, projects, ref){
  TADAprofile <- TADA_JoinWQPProfiles(
    FullPhysChem = resultphyschem,
    Sites = site,
    Projects = projects
  )

  if (nrow(TADAprofile) == 0){
    TADAprofile <- ref
  }
  
  TADAprofile2 <- TADAprofile %>%
    fmutate(across(.cols = NULL, .fns = as.character))
  
  return(TADAprofile2)
}

# A function to create a TADA data frame based on site, result, and project
TADA_join <- function(site, resultphyschem, ref){
  TADAprofile <- TADA_JoinWQPProfiles(
    FullPhysChem = resultphyschem,
    Sites = site
  )
 
  if (nrow(TADAprofile) == 0){
    TADAprofile2 <- ref
  } else {
    TADAprofile2 <- TADAprofile %>%
      TADA_ConvertDepthUnits() 
  }
}

# # Create a possible version of the TADA_DataRetrieval
# TADA_DataRetrieval_Fun <- function(huc, startDate, endDate, siteType, 
#                                    applyautoclean = FALSE){
#   TADA_DataRetrieval_poss <- possibly(TADA_DataRetrieval)
#   dat <- TADA_DataRetrieval_poss(huc = HUC_temp,
#                                  startDate = startDate,
#                                  endDate = endDate,
#                                  siteType = siteType ,
#                                  applyautoclean = applyautoclean)
#   return(dat)
# }

# Combine the TADA Depth column
# The order is "ResultDepthHeightMeasure", 
# "ActivityDepthHeightMeasure", "ActivityTopDepthHeightMeasure", "ActivityBottomDepthHeightMeasure"
TADA_depth_combine <- function(TADA_dat){
    TADA_dat2 <- TADA_dat %>%
      ungroup() %>%
      fmutate(Depth = coalesce(
        TADA.ResultDepthHeightMeasure.MeasureValue,
        TADA.ActivityDepthHeightMeasure.MeasureValue,
        TADA.ActivityTopDepthHeightMeasure.MeasureValue,
        TADA.ActivityBottomDepthHeightMeasure.MeasureValue
      )) %>%
      relocate(Depth, .before = "TADA.ResultDepthHeightMeasure.MeasureValue")

  return(TADA_dat2)
}

# A function to select the targert columns
TADA_selector <- function(TADA_dat){
  
  TADA_dat2 <- TADA_dat %>%
    # fselect(ActivityTypeCode:TADA.ActivityType.Flag,
    #         TADA.ActivityMediaName:TADA.CharacteristicNameAssumptions,
    #         MethodSpeciationName:ActivityStartDateTime,
    #         ResultMeasureValue:TADA.CensoredMethod,
    #         Depth, 
    #         TADA.ResultDepthHeightMeasure.MeasureValue,
    #         TADA.ActivityDepthHeightMeasure.MeasureValue,
    #         TADA.ActivityTopDepthHeightMeasure.MeasureValue,
    #         TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
    #         StatisticalBaseCode,
    #         TADA.AggregatedContinuousData.Flag,
    #         ResultAnalyticalMethod.MethodName:TADA.MeasureQualifierCode.Def) %>%
    fmutate(ActivityStartDate = ymd(ActivityStartDate)) %>%
    # Add the state name
    left_join(dataRetrieval::stateCd %>% dplyr::select(STATE_NAME, STATE),
              by = c("StateCode" = "STATE")) %>%
    rename(StateName = STATE_NAME) %>%
    relocate(StateName, .before = StateCode)
  
  return(TADA_dat2)
}

### TADA function transformation to possibly
TADA_AutoClean_poss <- possibly(TADA_AutoClean)
TADA_SimpleCensoredMethods_poss <- possibly(TADA_SimpleCensoredMethods)
TADA_FlagMethod_poss <- possibly(TADA_FlagMethod)
TADA_FlagSpeciation_poss <- possibly(TADA_FlagSpeciation)
TADA_FlagResultUnit_poss <- possibly(TADA_FlagResultUnit)
TADA_FlagFraction_poss <- possibly(TADA_FlagFraction)
TADA_FlagCoordinates_poss <- possibly(TADA_FlagCoordinates)
TADA_FindQCActivities_poss <- possibly(TADA_FindQCActivities)
TADA_FlagMeasureQualifierCode_poss <- possibly(TADA_FlagMeasureQualifierCode)
TADA_FindPotentialDuplicatesSingleOrg_poss <- possibly(TADA_FindPotentialDuplicatesSingleOrg)
TADA_FindPotentialDuplicatesMultipleOrgs_poss <- possibly(TADA_FindPotentialDuplicatesMultipleOrgs)
TADA_FindQAPPApproval_poss <- possibly(TADA_FindQAPPApproval)
TADA_FindQAPPDoc_poss <- possibly(TADA_FindQAPPDoc)
TADA_FindContinuousData_poss <- possibly(TADA_FindContinuousData)
TADA_FlagAboveThreshold_poss <- possibly(TADA_FlagAboveThreshold)
TADA_FlagBelowThreshold_poss <- possibly(TADA_FlagBelowThreshold)
TADA_HarmonizeSynonyms_poss <- possibly(TADA_HarmonizeSynonyms)

TADA_depth_combine_poss <- possibly(TADA_depth_combine)

TADA_selector_poss <- possibly(TADA_selector)

# A function to conduct data clean up
dat_cleanup <- function(dat, TADA_dat, USGS_ref, char_ref, fraction_ref, unit_ref, invalid_unit, detect_ref){
  dat2 <- dat %>%
    USGSPcode_fill(ref = USGS_ref) %>%
    char_convert(ref = char_ref) %>%
    fraction_convert(ref = fraction_ref) %>%
    unit_convert(ref = unit_ref, invalid_unit = invalid_unit) %>%
    detect_convert(ref = detect_ref) %>%
    unit_detect_clean() 
  
  # Adjust column order
  dat3 <- dat2 %>%
    relocate(MonitoringLocationName:VerticalMeasure.MeasureUnitCode, .after = MonitoringLocationIdentifier) %>%
    frename(Unit = Original_Unit) 
  
  # Column for join
  join_col <- c("OrganizationIdentifier", "ActivityIdentifier", "ActivityTypeCode", "ActivityMediaName", 
                "ActivityMediaSubdivisionName", "ActivityStartDate", "ActivityStartTime.Time", 
                "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "HUCEightDigitCode",                       
                "LatitudeMeasure", "LongitudeMeasure", "HorizontalCoordinateReferenceSystemDatumName")
  
  # Join the TADA data frame
  TADA_dat2 <- TADA_dat %>% 
    # Convert Depth unit
    TADA_depth_combine() %>%
    dplyr::select(all_of(c(join_col, "Depth"))) %>%
    distinct()
  
  # Column for ordering
  col_order <- c(
    "OrganizationIdentifier", "ActivityIdentifier", "ActivityTypeCode", "ActivityMediaName", 
    "ActivityMediaSubdivisionName", "ActivityStartDate", "ActivityStartTime.Time", 
    "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "HUCEightDigitCode",                       
    "LatitudeMeasure", "LongitudeMeasure", "StateName", "HorizontalCoordinateReferenceSystemDatumName", 
    "VerticalMeasure.MeasureValue", "VerticalMeasure.MeasureUnitCode", "Depth", "ResultDetectionConditionText", 
    "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",                    
    "MeasureQualifierCode", "ResultStatusIdentifier", "ResultValueTypeName", "ResultWeightBasisText", 
    "ResultCommentText", "USGSPCode", "ResultAnalyticalMethod.MethodIdentifier", 
    "ResultAnalyticalMethod.MethodIdentifierContext", "ResultAnalyticalMethod.MethodName", "AnalysisStartDate",                                
    "DetectionQuantitationLimitTypeName", "DetectionQuantitationLimitMeasure.MeasureValue", 
    "DetectionQuantitationLimitMeasure.MeasureUnitCode", "MethodSpecificationName", "Standard_Name", 
    "Standard_Fraction", "Standard_Unit", "Original_Unit", "Conversion_Factor", "Updated_ResultMeasureValue",                       
    "Updated_DetectionLimit", "Updated_ResultDetectionConditionText", "Char_Label", "Fraction_Label", 
    "Invalid_Unit_Label", "Unit_Label", "Value_Label", "Detection_Label"
  )
  
  dat4 <- dat3 %>%
    left_join(TADA_dat2, by = join_col) %>%
    get_vars(col_order) %>%
    lonlat_num()

  return(dat4)
}

# A function to calculate the cumsum grouping
cumsum_group <- function(dat, col, threshold = 25000){
  num <- nrow(dat)
  x <- dat[[col]]
  y <- numeric(num)
  z <- integer(num)
  group_num <- 1L
  current_sum <- 0
  for (i in 1:num){
    if (x[i] >= threshold){
      y[i] <- x[i]
      group_num <- group_num + 1L
      z[i] <- group_num
      current_sum <- 0
      group_num <- group_num + 1L
    } else if (current_sum + x[i] >= threshold){
      current_sum <- x[i]
      group_num <- group_num + 1L
      y[i] <- current_sum
      z[i] <- group_num
    } else {
      current_sum <- current_sum + x[i]
      y[i] <- current_sum
      z[i] <- group_num
    }
  }
  dat2 <- dat %>% fmutate(Cumsum = y, CumGroup = z)
  return(dat2)
}

### Functions to calculate the criteria

dateTime_create <- function(x){
  x2 <- x %>%
    fmutate(DateTime = case_when(
      is.na(ActivityStartTime.Time)  ~
        ymd_hms(paste0(as.character(ActivityStartDate),
                       " ",
                       "00:00:00")),
      TRUE                           ~
        ymd_hms(paste0(as.character(ActivityStartDate),
                                      " ",
                                      as.character(ActivityStartTime.Time)))
      ))
}

column_name_change <- function(x){
  x2 <- x %>%
    rename(Standard_Name = TADA.CharacteristicName, 
           Standard_Fraction = TADA.ResultSampleFractionText,
           Standard_Unit = TADA.ResultMeasure.MeasureUnitCode) %>%
    fselect(-LongitudeMeasure, -LatitudeMeasure) %>%
    rename(LongitudeMeasure = TADA.LongitudeMeasure,
           LatitudeMeasure = TADA.LatitudeMeasure)
  return(x2)
}

# Helper functions to get the pH data
pH_filter <- function(x){
  x2 <- x %>%
    fsubset(Standard_Name %in% "PH") %>%
    fselect(DateTime,
            MonitoringLocationIdentifier, MonitoringLocationTypeName,
            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
            StateName, pH = TADA.ResultMeasureValue) %>%
    # Calculate average if multiple samples exist
    group_by(across(-pH)) %>%
    summarize(pH = mean(pH, na.rm = TRUE)) %>%
    ungroup() %>%
    # Create the upper and lower bound
    mutate(DateTime_upper = DateTime + days(1),
           DateTime_lower = DateTime - days(1))
  
  return(x2)
}

pH_join <- function(x, y){
  
  by <- join_by(MonitoringLocationIdentifier, MonitoringLocationTypeName,
                HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
                StateName, closest(DateTime >= DateTime_lower), 
                closest(DateTime <= DateTime_upper))
  
  x2 <- x %>%
    left_join(y, by = by) %>%
    frename(DateTime = DateTime.x, DateTime_pH = DateTime.y) %>%
    fselect(-DateTime_lower, -DateTime_upper)
    
  return(x2)
}

pH_fun <- function(x){
  pH_dat <- x %>% 
    pH_filter()
  x2 <- x %>%
    pH_join(pH_dat)
  return(x2)
}

# Helper functions to get the temperature data
temp_filter <- function(x){
  x2 <- x %>%
    fsubset(Standard_Name %in% "TEMPERATURE") %>%
    fselect(DateTime,
            MonitoringLocationIdentifier, MonitoringLocationTypeName,
            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
            StateName, Temperature = TADA.ResultMeasureValue) %>%
    # Calculate average if multiple samples exist
    group_by(across(-Temperature)) %>%
    summarize(Temperature = mean(Temperature, na.rm = TRUE)) %>%
    ungroup() %>%
    # Create the upper and lower bound
    mutate(DateTime_upper = DateTime + days(1),
           DateTime_lower = DateTime - days(1))
  
  return(x2)
}

temp_join <- function(x, y){
  
  by <- join_by(MonitoringLocationIdentifier, MonitoringLocationTypeName,
                HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
                StateName, closest(DateTime >= DateTime_lower), 
                closest(DateTime <= DateTime_upper))
  
  x2 <- x %>%
    left_join(y, by = by) %>%
    frename(DateTime = DateTime.x, DateTime_Temperature = DateTime.y) %>%
    fselect(-DateTime_lower, -DateTime_upper)
    
  return(x2)
}

temp_fun <- function(x){
  temp_dat <- x %>% 
    temp_filter()
  x2 <- x %>%
    temp_join(temp_dat)
  return(x2)
}

# Helper functions to get the hardness data
hardness_filter <- function(x){
  x2 <- x %>%
    fsubset(Standard_Name %in% "HARDNESS, CA, MG") %>%
    fselect(ActivityStartDate, `ActivityStartTime.Time`,
            MonitoringLocationIdentifier, MonitoringLocationTypeName,
            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
            StateName, Hardness = TADA.ResultMeasureValue) %>%
    # Calculate average if multiple samples exist
    group_by(across(-Hardness)) %>%
    summarize(Hardness = mean(Hardness, na.rm = TRUE)) %>%
    ungroup() 
  return(x2)
}

hardness_join <- function(x, y){
  x2 <- x %>%
    left_join(y, by = c(
      "ActivityStartDate", "ActivityStartTime.Time",
      "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
      "HUCEightDigitCode", "LatitudeMeasure", "LongitudeMeasure",
      "StateName"
    ))
  return(x2)
}

hardness_fun <- function(x){
  hard_dat <- x %>% 
    hardness_filter()
  x2 <- x %>%
    hardness_join(hard_dat) %>%
    # Limit the hardness
    fmutate(Hardness = ifelse(Hardness > 400, 400, Hardness))
  return(x2)
}

# A function to calculate the hardness criteria
hardness_criteria_fun <- function(hardness, E_A, E_B, CF_A, CF_B, CF_C){
  if (is.na(CF_A) & is.na(CF_B)){
    CF2 <- CF_C
  } else if (!is.na(CF_A) & !is.na(CF_B)){
    CF <- CF_A - (log(hardness) * CF_B)
    
    CF2 <- CF
    CF2[CF2 > 1] <- 1
  }
  result <- exp(E_A * log(hardness) + E_B) * CF2
  
  return(result)
}

# A function to create flags for pH and temperature join
temperature_pH_flag <- function(x){
  x2 <- x %>%
    fmutate(pH_join_flag = as.integer(DateTime == DateTime_pH)) %>%
    fmutate(Temperature_join_flag = as.integer(DateTime == DateTime_Temperature))
  return(x2)
}

# A function to join the criteria
criteria_join <- function(x, y){
  x2 <- x %>%
    left_join(y, by = c("Standard_Name" = "Constituent",
                        # "Standard_Fraction" = "Fraction",
                        "Standard_Unit" = "MagUnits",
                        "StateName" = "State"),
              relationship = "many-to-many")
  return(x2)
}

# A function to calculate the crtieria based on pH and temperature
CMC_criteria_fun <- function(pH, salmonid = TRUE){
  if (salmonid){
    a <- 0.275
    b <- 39
  } else {
    a <- 0.411
    b <- 58.4
  }
  result <- a/(1 + 10^(7.204 - pH)) + b/(1 + 10^(pH - 7.204))
  return(result)
}
  
CCC_criteria_fun <- function(pH, temperature, earlylife = TRUE){
  result_temp <- 0.0577/(1 + 10^(7.688 - pH)) + 2.487/(1 + 10^(pH - 7.688)) 
  
  if (earlylife){
    result <- result_temp * min(c(2.85, 1.45 * 10^(0.028 * (25 - temperature))))
  } else {
    result <- result_temp * 1.45 * 10^(0.028 * (25 - max(c(temperature, 7))))
  }
  
  return(result)
}

# A function to get the summarized year for each group
summarized_year <- function(dat){
  dat2 <- dat %>%
    group_by(HUCEightDigitCode, CumGroup) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(Type = rep(c("Start", "End"), times = n()/2)) %>%
    fselect(HUCEightDigitCode, YearSummarized, CumGroup, Type) %>%
    pivot_wider(names_from = "Type", values_from = "YearSummarized") %>%
    fmutate(Start = ymd(paste0(Start, "-01", "-01")),
            End = ymd(paste0(End, "12", "-31")))
  return(dat2)
}

# A function to return NA if all values are NA, otherwise
# DO sum(x, na.rm = TRUE)
modSum <- function(x){
  if(all(is.na(x))){
    y <- NA
  } else {
    y <- sum(x, na.rm = TRUE)
  }
  return(y)
}

# A function to calculate exceedance
exceedance_fun <- function(x){
  x2 <- x %>%
    mutate(Exceedance = case_when(
      is.na(Criteria_Lower) & !is.na(Criteria_Upper) & 
        TADA.ResultMeasureValue > Criteria_Upper    ~   TRUE,
      !is.na(Criteria_Lower) & is.na(Criteria_Upper) & 
        TADA.ResultMeasureValue < Criteria_Lower    ~   TRUE,
      !is.na(Criteria_Lower) & !is.na(Criteria_Upper) & 
        (TADA.ResultMeasureValue < Criteria_Lower | 
           TADA.ResultMeasureValue > Criteria_Upper)   ~   TRUE,
      TRUE                                             ~  FALSE
    ))
  return(x2)
}

# A function to calculate the exceedance percentage data with criteria
exceedance_cal <- function(x){
  
  x2 <- x %>%
    group_by(MonitoringLocationIdentifier, MonitoringLocationName, 
             LongitudeMeasure, LatitudeMeasure,
             Standard_Name, Standard_Fraction, Standard_Unit, Use, Details, Type,
             Frequency, Duration) %>%
    summarize(Size = n(), 
              Exceedance_Size = modSum(Exceedance)) %>%
    ungroup() %>%
    fmutate(Percentage = Exceedance_Size/Size)
  
  return(x2)
}

# A function to summarize the criteria
criteria_summary <- function(x){
  x2 <- x %>%
    group_by(Standard_Name, Standard_Fraction, Standard_Unit, Details, Use, Type,
             Frequency, Duration) %>%
    summarize(Site_Num = n_distinct(MonitoringLocationIdentifier),
              Site_Exceedance = sum(Percentage > 0, na.rm = TRUE)) %>%
    ungroup() %>%
    fmutate(Percentage = Site_Exceedance/Site_Num)
  return(x2)
}

# A function to summarize the data without criteria
criteria_no_summary <- function(x){
  x2 <- x %>%
    group_by(Standard_Name, Standard_Fraction, Standard_Unit, Details, Use, Type,
             Frequency, Duration) %>%
    summarize(Site_Num = n_distinct(MonitoringLocationIdentifier))
  return(x2)
}
