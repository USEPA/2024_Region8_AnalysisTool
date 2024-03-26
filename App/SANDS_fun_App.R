### This script summarized the needed function in SANDS format for the app

# Load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(collapse)
library(dataRetrieval)
library(TADA)
library(tigris)
library(tidycensus)

### Data Download

# # A function to construct the argument list
# SANDS_args_create <- function(huc = NULL, 
#                         bBox = NULL,
#                         project = NULL,
#                         startDateLo = NULL, startDateHi = NULL){
#   # Construct the arguments for downloads
#   args <- list(
#     "startDateLo" = startDateLo,
#     "startDateHi" = startDateHi,
#     "huc" = huc,
#     "bBox" = bBox,
#     "project" = project 
#   )
#   
#   # Remove NULL attribute
#   args <- args[map_lgl(args, function(x) !is.null(x))]
#   
#   return(args)
# }

# Create a possible version of the readWQPdata
readWQPdata_poss <- possibly(readWQPdata)

### STATIONS

# A function to pull site data
SANDS_site_fun <- function(args, ref){
  site <- readWQPdata_poss(args, service = "Station", ignore_attributes = TRUE)
  
  if (is.null(site) | nrow(site) == 0L){
    site <- ref
  }
  
  return(site)
}

SANDS_site_join <- function(x, tribal_sf, AU_sf){
  ### Combine AU and tribal
  
  # Create an sf object for x
  x2 <- x %>%
    st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"),
             crs = 4326, remove = FALSE) %>%
    st_join(tribal_sf) %>%
    st_join(AU_sf) %>%
    st_set_geometry(NULL)
  
  return(x2)
}

# A function to format the site to be consistent with the SANDS format
SANDS_clean_site <- function(x, ref, state_county){
  
  if (nrow(x) > 0){
    
    x2 <- x %>%
      fselect(-ContributingDrainageAreaMeasure.MeasureValue,
              -ContributingDrainageAreaMeasure.MeasureUnitCode,
              -LocalAqfrName, -ProviderName) %>%
      frename(`DrainageAreaMeasure.MeasureValue` = MeasureValue,
              `HorizontalAccuracyMeasure.MeasureValue` = MeasureValue2,
              `VerticalMeasure.MeasureValue` = MeasureValue4,
              `VerticalAccuracyMeasure.MeasureValue` = MeasureValue6,
              `WellDepthMeasure.MeasureValue` = MeasureValue8,
              `WellHoleDepthMeasure.MeasureValue` = MeasureValue10,
              `DrainageAreaMeasure.MeasureUnitCode` = MeasureUnitCode,
              `HorizontalAccuracyMeasure.MeasureUnitCode` = MeasureUnitCode3,
              `VerticalMeasure.MeasureUnitCode` = MeasureUnitCode5,
              `VerticalAccuracyMeasure.MeasureUnitCode` = MeasureUnitCode7,
              `WellDepthMeasure.MeasureUnitCode` = MeasureUnitCode9,
              `WellHoleDepthMeasure.MeasureUnitCode` = MeasureUnitCode11) %>%
      # Join state and county name
      left_join(state_county, by = c("StateCode" = "state_code",
                                     "CountyCode" = "county_code")) %>%
      fselect(-state) %>%
      frename(state_name = "StateName", county = "CountyName") %>%
      frename(\(x) paste0("ns2:", x)) %>%
      # Reorder the columns
      get_vars(names(ref))
  } else {
    x2 <- ref
  }
  
  return(x2)
} 

### RESULTS

# A function to download the result
SANDS_result_fun <- function(args, ref){
  result <- readWQPdata_poss(args, ignore_attributes = TRUE)
  
  if (is.null(result) | nrow(result) == 0L){
    result <- ref
  }
  
  return(result)
}

# A function to format the result data to be consistent as RESULTS
SANDS_clean_result <- function(x, ref){
  if (nrow(x) > 0){
    x2 <- x %>%
      frename(function(x) str_replace(x, fixed("."), fixed("/"))) %>%
      fselect(-timeZoneStart, -timeZoneEnd, -ActivityStartDateTime, -ActivityEndDateTime) %>%
      # Reorder the columns
      get_vars(names(ref))
  } else {
    x2 <- ref
  }
  
  return(x2)
}

### RESULTS Phys Chem

# A function to download the result
SANDS_resultphyschem_fun <- function(args, ref){
  resultphyschem <- readWQPdata_poss(args, dataProfile = "resultPhysChem",
                                     ignore_attributes = TRUE)
  
  if (is.null(resultphyschem) | nrow(resultphyschem) == 0L){
    resultphyschem <- ref
  }
  
  return(resultphyschem)
}

### Narrow

# A function to download the Narrow data
SANDS_narrow_fun <- function(args, ref){
  narrow <- readWQPdata_poss(args, dataProfile = "narrowResult", ignore_attributes = TRUE)
  
  if (is.null(narrow) | nrow(narrow) == 0L){
    narrow <- ref
  }
  
  return(narrow)
}

# A function to format the narrow data to be consistent as Narrow
SANDS_clean_narrow <- function(x, ref){
  if (nrow(x) > 0){
    x2 <- x %>%
      frename(function(x) str_replace(x, fixed("."), fixed("/"))) %>%
      fselect(-timeZoneStart, -ActivityStartDateTime) %>%
      # Reorder the columns
      get_vars(names(ref))
  } else {
    x2 <- ref
  }
  
  return(x2)
}

### SUMMARY

# A function to create the SUMMARY table
SANDS_create_summary_part1 <- function(sites, results, narrows, ref, state_county){
  
  if (nrow(sites) > 0 & nrow(results) > 0 & nrow(narrows) > 0){
    # Select the columns in sites
    sites2 <- sites %>%
      semi_join(results, 
                by = c("OrganizationIdentifier", "MonitoringLocationIdentifier")) %>%
      # Join state and county name
      left_join(state_county, by = c("StateCode" = "state_code",
                                     "CountyCode" = "county_code")) %>%
      fselect(-state) %>%
      frename(state_name = "StateName", county = "CountyName") %>%
      fselect(
        "ORG ID" = "OrganizationIdentifier",
        "SITE ID" = "MonitoringLocationIdentifier",
        "SITE NAME" = "MonitoringLocationName",
        "STATE NAME" = "StateName",
        "COUNTY NAME" = "CountyName",
        "LATITUDE" = "LatitudeMeasure",
        "LONGITUDE" = "LongitudeMeasure",
        "COORDINATE REF" = "HorizontalCoordinateReferenceSystemDatumName",
        "SITE TYPE" = "MonitoringLocationTypeName",
        "MT RES NAME" = "MT_Res_Name",	
        "ASSESSMENTUNITIDENTIFIER" = "AssessmentUnitIdentifier",	
        "ASSESSMENTUNITNAME" = "AssessmentUnitName"
      )
    # Select the columns in narrows
    narrows2 <- narrows %>%
      # Create a within group ID for merging
      # Some rows differs only in ResultIdentifier and AnalysisStartDateTime
      # But ResultIdentifier and AnalysisStartDateTime is not in resuls
      ftransform(
        ID = rowid(
          OrganizationIdentifier,
          MonitoringLocationIdentifier,
          ActivityStartDate,
          ActivityIdentifier,
          `ActivityStartTime.Time`,
          CharacteristicName,
          ResultMeasureValue,
          ResultMeasure.MeasureUnitCode,
          ResultStatusIdentifier,
          ResultValueTypeName,
          ResultSampleFractionText,
          ResultCommentText,
          SubjectTaxonomicName,
          AnalysisStartDate,
          ResultDetectionConditionText
        )
      ) %>%
      fselect(
        "ORG ID" = "OrganizationIdentifier",
        "SITE ID" = "MonitoringLocationIdentifier",
        "ACTIVITY DATE" = "ActivityStartDate",
        "ACTIVITY ID" = "ActivityIdentifier",
        "ACTIVITY START TIME" = "ActivityStartTime.Time",
        "LAB ANALYSIS DATE" = "AnalysisStartDate",
        "CHARACTERISTIC NAME" = "CharacteristicName",
        "RESULT VALUE" = "ResultMeasureValue",
        "RESULT UNIT" = "ResultMeasure.MeasureUnitCode",
        "RESULT STATUS" = "ResultStatusIdentifier",
        "RESULT VALUE TYPE" = "ResultValueTypeName",
        "SAMPLE FRACTION" = "ResultSampleFractionText",
        "ResultCommentText",
        "ResultDetectionConditionText",
        "MethodSpecificationName",
        "SubjectTaxonomicName",
        "ID"
      )
    # Select the columns in results
    results2 <- results %>%
      ftransform(
        ID = rowid(
          OrganizationIdentifier,
          MonitoringLocationIdentifier,
          ActivityStartDate,
          ActivityIdentifier,
          `ActivityStartTime.Time`,
          CharacteristicName,
          ResultMeasureValue,
          ResultMeasure.MeasureUnitCode,
          ResultStatusIdentifier,
          ResultValueTypeName,
          ResultSampleFractionText,
          ResultCommentText,
          SubjectTaxonomicName,
          AnalysisStartDate,
          ResultDetectionConditionText
        )
      ) %>%
      fselect(
        "ORG ID" = "OrganizationIdentifier",
        "SITE ID" = "MonitoringLocationIdentifier",
        "ACTIVITY DATE" = "ActivityStartDate",
        "ACTIVITY ID" = "ActivityIdentifier",
        "ACTIVITY TYPE" = "ActivityTypeCode",
        "ACTIVITY MEDIA" = "ActivityMediaName",
        "ACTIVITY SUBMEDIA" = "ActivityMediaSubdivisionName",
        "CHARACTERISTIC NAME" = "CharacteristicName",
        "RESULT VALUE" = "ResultMeasureValue",
        "RESULT UNIT" = "ResultMeasure.MeasureUnitCode",
        "QUALIFIER CODE" = "MeasureQualifierCode",
        "RESULT STATUS" = "ResultStatusIdentifier",
        "RESULT VALUE TYPE" = "ResultValueTypeName",
        "SAMPLE FRACTION" = "ResultSampleFractionText",
        "LAB ANALYSIS DATE" = "AnalysisStartDate",
        "DETECTION LIMIT TYPE" = "DetectionQuantitationLimitTypeName",
        "DETECTION LIMIT VALUE" = "DetectionQuantitationLimitMeasure.MeasureValue",
        "DETECTION LIMIT UNIT" = "DetectionQuantitationLimitMeasure.MeasureUnitCode",
        "WQX SPECIATION" = "ResultWeightBasisText",
        "ACTIVITY START TIME" = "ActivityStartTime.Time",
        "SubjectTaxonomicName",
        "ResultAnalyticalMethod.MethodIdentifier",
        "ResultAnalyticalMethod.MethodName",
        "ResultAnalyticalMethod.MethodIdentifierContext",
        "ResultCommentText",
        "ResultDetectionConditionText",
        "ID"
      ) %>%
      # Join the sites2 table
      left_join(sites2, by = c("ORG ID", "SITE ID")) %>%
      # Join the narrow2 table
      left_join(narrows2, by = c(
        "ORG ID","SITE ID", "ACTIVITY DATE", "ACTIVITY ID", "ACTIVITY START TIME",
        "CHARACTERISTIC NAME", "RESULT VALUE", "RESULT UNIT", "RESULT STATUS",
        "RESULT VALUE TYPE", "SAMPLE FRACTION", "LAB ANALYSIS DATE", 
        "ResultCommentText", "SubjectTaxonomicName",
        "ResultDetectionConditionText", "ID"
      )) %>%
      # Prepare the COMMENT column
      fmutate(
        Analyte = 
          pmap_chr(list(`CHARACTERISTIC NAME`, `RESULT VALUE`, `RESULT UNIT`, `SAMPLE FRACTION`, 
                        MethodSpecificationName
          ),
          function(x, y, z, u, w){
            x2 <- str_replace_na(x, "")
            y2 <- str_replace_na(y, "")
            z2 <- str_replace_na(z, "")
            u2 <- str_replace_na(u, "")
            w2 <- str_replace_na(w, "")
            
            out <- str_c("Analyte = ", x2, " ", 
                         w2, 
                         " ", y2, " ", 
                         z2, ", ", u2)
            
            return(out)
          })
      ) %>%
      fmutate(
        COMMENTS =
          pmap_chr(list(Analyte, `ResultAnalyticalMethod.MethodIdentifier`,
                        `ResultAnalyticalMethod.MethodName`, ResultCommentText, 
                        `ResultAnalyticalMethod.MethodIdentifierContext`),
                   function(x, y, z, u, w, v){
                     x2 <- str_replace_na(x, "")
                     y2 <- str_replace_na(y, "")
                     z2 <- str_replace_na(z, "")
                     u2 <- str_replace_na(u, "")
                     w2 <- str_replace_na(w, "")
                     
                     out <- str_c(x2,
                                  "\r\r\nMethod ID = ", y2, " ", w2,
                                  "\r\r\nMethod Name = ", z2,
                                  "\r\r\nResult Comment = ", u2) 
                     
                     # Remove the last white spaces
                     out <- str_trim(out)
                     
                     return(out)
                   }
          )) %>%
      # Reorder the columns
      get_vars(c(names(ref), "ResultCommentText")) %>%
      # Arrange the columns
      roworderv(c("SITE ID", "ACTIVITY DATE", "ACTIVITY ID"))
  } else {
    results2 <- ref
  }
  
  return(results2)
}

SANDS_create_summary_part2 <- function(x, ref){
  if (nrow(x) > 0){
    x2 <- x %>% fselect(-ResultCommentText)
  } else {
    x2 <- ref
  }
  return(x2)
}

### nutrients tab

# This function takes the summary tab as the input argument
nutrients_tab <- function(x, address_dat, lookup_main, lookup2_spec, narrow_dat,
                          assign_fraction, unit_dat, fraction_dat, header, TADA_file){
  par_name <- unique(c(unique(lookup_main$Parameter), 
                       unique(lookup2_spec$Parameter)))
  
  if (nrow(x) > 0){
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      #fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      # Separate the x2 with lookup_main and lookup2_spec
      x2_main <- x2 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% unique(lookup_main$Parameter))
      
      x2_spec <- x2 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% unique(lookup2_spec$Parameter))
      
      # Join x2_main with lookup_main
      x3_main <- x2_main %>%
        # Impute fraction for those special parameters
        left_join(assign_fraction, by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fmutate(`SAMPLE FRACTION` = as.character(ifelse(is.na(`SAMPLE FRACTION`) & !is.na(Assign_Fraction),
                                                        Assign_Fraction, `SAMPLE FRACTION`))) %>%
        fselect(-Assign_Fraction) %>%
        left_join(lookup_main %>% select(-PCode), 
                  by = c("CHARACTERISTIC NAME" = "Parameter",
                         "SAMPLE FRACTION" = "Fraction")) %>%
        fselect(-Calculated_Address, -Other_Fraction_Address, -n)
      
      # Join x2_spec with narrow_dat and lookup2_spec
      narrow_dat2 <- narrow_dat %>% 
        fselect("ORG ID" = "OrganizationIdentifier",
                "SITE ID" = "MonitoringLocationIdentifier",
                "ACTIVITY DATE" = "ActivityStartDate",
                "ACTIVITY ID" = "ActivityIdentifier",
                "CHARACTERISTIC NAME" = "CharacteristicName",
                "SAMPLE FRACTION" = "ResultSampleFractionText",
                "RESULT VALUE" = "ResultMeasureValue",
                "RESULT UNIT" = "ResultMeasure/MeasureUnitCode",
                "MethodSpecificationName") %>%
        mutate(MethodSpecificationName = str_to_upper(MethodSpecificationName))
      
      x3_spec <- x2_spec %>%
        left_join(narrow_dat2, by = c(
          "ORG ID", "SITE ID", "ACTIVITY DATE",
          "ACTIVITY ID", "CHARACTERISTIC NAME", "SAMPLE FRACTION",
          "RESULT VALUE", "RESULT UNIT"
        ), multiple = "first") %>%
        left_join(fraction_dat,
                  by = c("SAMPLE FRACTION" = "Original_Fraction")) %>%
        left_join(lookup2_spec %>% select(-PCode), 
                  by = c("CHARACTERISTIC NAME" = "Parameter",
                         "Standard_Fraction" = "Fraction",
                         "MethodSpecificationName" = "Speciation")) %>%
        fselect(-Calculated_Address, -Other_Fraction_Address, -n, -`SAMPLE FRACTION`) %>%
        rename("SAMPLE FRACTION" = "Standard_Fraction")
      
      x3 <- bind_rows(x3_main, x3_spec)
      
      # Conduct unit conversion
      x4 <- x3 %>% SANDS_unit(unit_dat = unit_dat)
      
      # Get records without address
      x4_noAd <- x4 %>% 
        fsubset(is.na(Address)) #%>%
      # # Create the columns with fraction and values
      # fmutate(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
      #           str_c(reNA(`SAMPLE FRACTION`), "? ", as.character(Num_Value))) %>%
      # fselect("Org ID" = "ORG ID", 
      #         "Station (Site) Name" = "SITE NAME",
      #         "Site ID" = "SITE ID",
      #         "Activity Date" = "ACTIVITY DATE",
      #         "Activity ID" = "ACTIVITY ID",
      #         "Latitude" = "LATITUDE",
      #         "Longitude" = "LONGITUDE",
      #         "Waterbody Type" = "SITE TYPE",
      #         "Medium" = "ACTIVITY MEDIA",
      #         "Medium Detail" = "ACTIVITY SUBMEDIA",
      #         "DATUM" = "COORDINATE REF",
      #         "Activity \r\nTime" = "ACTIVITY START TIME",
      #         "Other Fraction \r\nConcentration\r\n(U find analyte)") %>%
      # group_by(across(-`Other Fraction \r\nConcentration\r\n(U find analyte)`)) %>%
      # slice(1)
      
      # Get records with address
      x4_Ad <- x4 %>% fsubset(!is.na(Address))
      
      # Based on the rank of address, determine the records to keep
      x5 <- x4_Ad %>%
        group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                 `ACTIVITY ID`, Address) %>%
        drop_na(Value) %>%
        filter(Rank == min(Rank, na.rm = TRUE)) %>%
        # If multiple values exist, only keep the one that `RESULT UNIT` == %in% Units
        mutate(Rank2 = ifelse(`RESULT UNIT` %in% Units, 1, 2)) %>%
        slice(1) %>%
        ungroup()
      
      # Prepare the main dataset
      x6 <- x5 %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium Detail" = "ACTIVITY SUBMEDIA",
                "Replica" = "ACTIVITY TYPE",
                "DATUM" = "COORDINATE REF",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address",
                "ResultCommentText") %>%
        # Join with table header names
        left_join(address_dat, by = "Address") 
      
      x7 <- x6 %>%
        drop_na(Name) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
        pivot_wider(names_from = "Name", values_from = "Value",
                    values_fn = \(x) paste0(x, collapse = " ")) %>%
        # left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
        #                           "Activity ID", "Latitude", "Longitude", "Waterbody Type",
        #                           "Medium", "Medium Detail", "DATUM",
        #                           "Activity \r\nTime")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
        mutate(across(everything(), .fns = as.character))
      
      x7_comment <- x6 %>%
        drop_na(Name) %>%
        fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
        fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
        fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                           paste0(head_copy, ": ", ResultCommentText),
                                           NA)) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                -Value, -head_copy, -Name) %>%
        group_by(across(-ResultCommentText)) %>%
        summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                    paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
        mutate(across(everything(), .fns = as.character)) %>%
        fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                Latitude, Longitude, `Waterbody Type`, Medium,  `Medium Detail`, DATUM,
                `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
      
      x7_final <- x7 %>%
        left_join(x7_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                     "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium Detail", "DATUM",
                                     "Activity \r\nTime"))
      
      # Join the TADA_file
      
      if (nrow(TADA_file) == 0){
        x7_final2 <- x7_final
      } else {
        TADA_file2 <- TADA_file %>%
          fselect("Org ID" = "OrganizationIdentifier",
                  "Station (Site) Name" = "MonitoringLocationName",
                  "Site ID" = "MonitoringLocationIdentifier",
                  "Activity Date" = "ActivityStartDate",
                  "Activity ID" = "ActivityIdentifier",
                  "Activity \r\nTime" = "ActivityStartTime.Time",
                  "Latitude" = "LatitudeMeasure",
                  "Longitude" = "LongitudeMeasure",
                  "Depth") %>%
          distinct() %>%
          mutate(across(everything(), .fns = as.character)) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`))
        
        x7_final2 <- x7_final %>%
          left_join(TADA_file2, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity \r\nTime",
                                       "Activity ID", "Latitude", "Longitude"))
      }
      
      # Join to the header
      x8 <- header %>%
        mutate(across(everything(), .fns = as.character)) %>%
        bind_rows(x7_final2) %>%
        arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      
    } else {
      x8 <- header
    }
    
  } else {
    x8 <- header
  }
  
  return(x8)
}

# A function to prepare the GIS output 
GIS_output_nutrient <- function(x){
  x2 <- x %>% mutate(across(c(`Flow (cfs)`:`DO\r\n(%)`, 
                              `Filamentous Algae Cover`:`Macrophage Cover`), 
                            .fns = number_clean))
  return(x2)
}

### H20 tab

# This function takes the summary tab as the input argument
H2O_tab <- function(x, address_dat, lookup, assign_fraction, unit_dat, header,
                    TADA_file){
  
  if (nrow(x) > 0){
    par_name <- unique(lookup$Parameter)
    
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      # Join x2 with lookup
      x3 <- x2 %>%
        # Impute fraction for those special parameters
        left_join(assign_fraction, by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fmutate(`SAMPLE FRACTION` = as.character(ifelse(is.na(`SAMPLE FRACTION`) & !is.na(Assign_Fraction),
                                                        Assign_Fraction, `SAMPLE FRACTION`))) %>%
        fselect(-Assign_Fraction) %>%
        left_join(lookup %>% select(-PCode), 
                  by = c("CHARACTERISTIC NAME" = "Parameter",
                         "SAMPLE FRACTION" = "Fraction")) 
      
      # Conduct unit conversion
      x4 <- x3 %>% SANDS_unit(unit_dat = unit_dat)
      
      # # Get records without address
      # x4_noAd <- x4 %>% 
      #   fsubset(is.na(Address)) %>%
      #   # Create the columns with fraction and values
      #   fmutate(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
      #             str_c(reNA(`SAMPLE FRACTION`), "? ", as.character(Num_Value))) %>%
      #   fselect("Org ID" = "ORG ID", 
      #           "Station (Site) Name" = "SITE NAME",
      #           "Site ID" = "SITE ID",
      #           "Activity Date" = "ACTIVITY DATE",
      #           "Activity ID" = "ACTIVITY ID",
      #           "Latitude" = "LATITUDE",
      #           "Longitude" = "LONGITUDE",
      #           "Waterbody Type" = "SITE TYPE",
      #           "Medium" = "ACTIVITY MEDIA",
      #           "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
      #           "Activity\r\nType" = "ACTIVITY TYPE",
      #           "DATUM" = "COORDINATE REF",
      #           "Activity \r\nTime" = "ACTIVITY START TIME",
      #           "Other Fraction \r\nConcentration\r\n(U find analyte)") %>%
      #   group_by(across(-`Other Fraction \r\nConcentration\r\n(U find analyte)`)) %>%
      #   slice(1)
      
      # Get records with address
      x4_Ad <- x4 %>% fsubset(!is.na(Address))
      
      # Based on the rank of address, determine the records to keep
      x5 <- x4_Ad %>%
        group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                 `ACTIVITY ID`, Address) %>%
        drop_na(Value) %>%
        filter(Rank == min(Rank, na.rm = TRUE)) %>%
        # If multiple values exist, only keep the one that `RESULT UNIT` == %in% Units
        mutate(Rank2 = ifelse(`RESULT UNIT` %in% Units, 1, 2)) %>%
        slice(1) %>%
        ungroup()
      
      # Get the hardness dataset
      hardness_dat <- x5 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% 
                  c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3")) %>%
        # Add the final notes
        fmutate(Value = case_when(
          is.na(Num_Value)                         ~str_c("<  ", as.character(Num_Det_Value)),
          `RESULT VALUE TYPE` %in% "Calculated"    ~str_c("C ", as.character(Num_Value)),
          TRUE                                     ~as.character(Num_Value)
        )) %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                "Activity\r\nType" = "ACTIVITY TYPE",
                "DATUM" = "COORDINATE REF",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address") %>%
        # Join with table header names
        left_join(address_dat, by = "Address")
      
      x6 <- x5 %>%
        fsubset(!`CHARACTERISTIC NAME` %in% 
                  c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3"))
      
      # Prepare the main dataset
      x7 <- x6 %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                "Activity\r\nType" = "ACTIVITY TYPE",
                "DATUM" = "COORDINATE REF",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address",
                "ResultCommentText") %>%
        # Join with table header names
        left_join(address_dat, by = "Address") 
      
      # x8 <- x7 %>%
      #   bind_rows(hardness_dat) %>%
      #   drop_na(Name) %>%
      #   fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`) %>%
      #   pivot_wider(names_from = "Name", values_from = "Value",
      #               values_fn = \(x) paste0(x, collapse = " ")) %>%
      #   left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
      #                             "Activity ID", "Latitude", "Longitude", "Waterbody Type",
      #                             "Medium", "Medium\r\nDetail", "Activity\r\nType", "DATUM",
      #                             "Activity \r\nTime")) %>%
      #   fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
      #   mutate(across(everything(), .fns = as.character))
      
      x8 <- x7 %>%
        bind_rows(hardness_dat) %>%
        drop_na(Name) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
        pivot_wider(names_from = "Name", values_from = "Value",
                    values_fn = \(x) paste0(x, collapse = " ")) %>%
        # left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
        #                           "Activity ID", "Latitude", "Longitude", "Waterbody Type",
        #                           "Medium", "Medium Detail", "DATUM",
        #                           "Activity \r\nTime")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
        mutate(across(everything(), .fns = as.character))
      
      x8_comment <- x7 %>%
        bind_rows(hardness_dat) %>%
        drop_na(Name) %>%
        fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
        fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
        fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                           paste0(head_copy, ": ", ResultCommentText),
                                           NA)) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                -Value, -head_copy, -Name) %>%
        group_by(across(-ResultCommentText)) %>%
        summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                    paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
        mutate(across(everything(), .fns = as.character)) %>%
        fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                Latitude, Longitude, `Waterbody Type`, Medium,  `Medium\r\nDetail`, DATUM,
                `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
      
      x8_final <- x8 %>%
        left_join(x8_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                     "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium\r\nDetail", "DATUM",
                                     "Activity \r\nTime"))
      
      # Join the TADA_file
      
      if (nrow(TADA_file) == 0){
        x8_final2 <- x8_final
      } else {
        TADA_file2 <- TADA_file %>%
          fselect("Org ID" = "OrganizationIdentifier",
                  "Station (Site) Name" = "MonitoringLocationName",
                  "Site ID" = "MonitoringLocationIdentifier",
                  "Activity Date" = "ActivityStartDate",
                  "Activity ID" = "ActivityIdentifier",
                  "Activity \r\nTime" = "ActivityStartTime.Time",
                  "Latitude" = "LatitudeMeasure",
                  "Longitude" = "LongitudeMeasure",
                  "Depth") %>%
          distinct() %>%
          mutate(across(everything(), .fns = as.character)) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`))
        
        x8_final2 <- x8_final %>%
          left_join(TADA_file2, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity \r\nTime",
                                       "Activity ID", "Latitude", "Longitude"))
      }
      
      # Join to the header
      x9 <- header %>%
        mutate(across(everything(), .fns = as.character)) %>%
        bind_rows(x8_final2) %>%
        arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      
    } else {
      x9 <- header
    }
    
  } else {
    x9 <- header
  }

  return(x9)
}

# A function to prepare the GIS output 
GIS_output_metal_h2o <- function(x){
  x2 <- x %>% mutate(across(c(`Depth`:`Tl \r\n(ug/L) \r\nD`,
                              `Hardness (mg/L)\r\nDissolved`), 
                            .fns = number_clean))
  return(x2)
}

### Sed tab

# This function takes the summary tab as the input argument
sed_tab <- function(x, address_dat, lookup, unit_dat, header){
  if (nrow(x) > 0){
    par_name <- unique(lookup$Parameter)
    
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Sediment") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      # Join x2 with lookup
      x3 <- x2 %>%
        # # Impute fraction for those special parameters
        # left_join(assign_fraction, by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        # fmutate(`SAMPLE FRACTION` = ifelse(is.na(`SAMPLE FRACTION`) & !is.na(Assign_Fraction),
        #                                    Assign_Fraction, `SAMPLE FRACTION`)) %>%
        # fselect(-Assign_Fraction) %>%
        left_join(lookup %>% select(-PCode), 
                  by = c("CHARACTERISTIC NAME" = "Parameter",
                         "SAMPLE FRACTION" = "Fraction")) 
      
      # Conduct unit conversion
      x4 <- x3 %>% SANDS_unit(unit_dat = unit_dat)
      
      # # Get records without address
      # x4_noAd <- x4 %>% 
      #   fsubset(is.na(Address)) %>%
      #   # Create the columns with fraction and values
      #   fmutate(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
      #             str_c(reNA(`SAMPLE FRACTION`), "? ", as.character(Num_Value))) %>%
      #   fselect("Org ID" = "ORG ID", 
      #           "Station (Site) Name" = "SITE NAME",
      #           "Site ID" = "SITE ID",
      #           "Activity Date" = "ACTIVITY DATE",
      #           "Activity ID" = "ACTIVITY ID",
      #           "Latitude" = "LATITUDE",
      #           "Longitude" = "LONGITUDE",
      #           "Waterbody Type" = "SITE TYPE",
      #           "Medium" = "ACTIVITY MEDIA",
      #           "Medium Detail" = "ACTIVITY SUBMEDIA",
      #           "Activity Type" = "ACTIVITY TYPE",
      #           "Activity \r\nTime" = "ACTIVITY START TIME",
      #           "Other Fraction \r\nConcentration\r\n(U find analyte)") %>%
      #   group_by(across(-`Other Fraction \r\nConcentration\r\n(U find analyte)`)) %>%
      #   slice(1)
      
      # Get records with address
      x4_Ad <- x4 %>% fsubset(!is.na(Address))
      
      # Based on the rank of address, determine the records to keep
      x5 <- x4_Ad %>%
        group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                 `ACTIVITY ID`, Address) %>%
        drop_na(Value) %>%
        filter(Rank == min(Rank, na.rm = TRUE)) %>%
        # If multiple values exist, only keep the one that `RESULT UNIT` == %in% Units
        mutate(Rank2 = ifelse(`RESULT UNIT` %in% Units, 1, 2)) %>%
        slice(1) %>%
        ungroup()
      
      # Prepare the main dataset
      x6 <- x5 %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium Detail" = "ACTIVITY SUBMEDIA",
                "Activity Type" = "ACTIVITY TYPE",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address",
                "ResultCommentText") %>%
        # Join with table header names
        left_join(address_dat, by = "Address") 
      
      # x7 <- x6 %>%
      #   drop_na(Name) %>%
      #   fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`) %>%
      #   pivot_wider(names_from = "Name", values_from = "Value",
      #               values_fn = \(x) paste0(x, collapse = " ")) %>%
      #   left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
      #                             "Activity ID", "Latitude", "Longitude", "Waterbody Type",
      #                             "Medium", "Medium Detail", "Activity Type", 
      #                             "Activity \r\nTime")) %>%
      #   fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
      #   mutate(across(everything(), .fns = as.character))
      
      x7 <- x6 %>%
        drop_na(Name) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
        pivot_wider(names_from = "Name", values_from = "Value",
                    values_fn = \(x) paste0(x, collapse = " ")) %>%
        # left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
        #                           "Activity ID", "Latitude", "Longitude", "Waterbody Type",
        #                           "Medium", "Medium Detail", "DATUM",
        #                           "Activity \r\nTime")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
        mutate(across(everything(), .fns = as.character))
      
      x7_comment <- x6 %>%
        drop_na(Name) %>%
        fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
        fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
        fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                           paste0(head_copy, ": ", ResultCommentText),
                                           NA)) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                -Value, -head_copy, -Name) %>%
        group_by(across(-ResultCommentText)) %>%
        summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                    paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
        mutate(across(everything(), .fns = as.character)) %>%
        fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                Latitude, Longitude, `Waterbody Type`, Medium,  `Medium Detail`,
                `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
      
      x7_final <- x7 %>%
        left_join(x7_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                     "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium Detail", 
                                     "Activity \r\nTime"))
      
      # Join to the header
      x8 <- header %>%
        mutate(across(everything(), .fns = as.character)) %>%
        bind_rows(x7_final) %>%
        arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      
    } else {
      
      x8 <- header
      
    }
    
  } else {
    x8 <- header
  }
  return(x8)
}

# A function to prepare the GIS output 
GIS_output_metal_sed <- function(x){
  x2 <- x %>% mutate(across(`Al \r\n(ug/g) \r\nT`:`Tl  \r\n(ug/g) \r\n<63um`, .fns = number_clean))
  return(x2)
}

### salinity tab

# This function takes the summary tab as the input argument
salinity_tab <- function(x, address_dat, lookup, assign_fraction, unit_dat, header, TADA_file){
  if (nrow(x) > 0){
    par_name <- unique(lookup$Parameter)
    
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      # Join x2 with lookup
      x3 <- x2 %>%
        # Impute fraction for those special parameters
        left_join(assign_fraction, by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fmutate(`SAMPLE FRACTION` = as.character(ifelse(is.na(`SAMPLE FRACTION`) & !is.na(Assign_Fraction),
                                                        Assign_Fraction, `SAMPLE FRACTION`))) %>%
        fselect(-Assign_Fraction) %>%
        left_join(lookup %>% select(-PCode), 
                  by = c("CHARACTERISTIC NAME" = "Parameter",
                         "SAMPLE FRACTION" = "Fraction")) 
      
      # Conduct unit conversion
      x4 <- x3 %>% SANDS_unit(unit_dat = unit_dat)
      
      # # Get records without address
      # x4_noAd <- x4 %>% 
      #   fsubset(is.na(Address)) %>%
      #   # Create the columns with fraction and values
      #   fmutate(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
      #             str_c(reNA(`SAMPLE FRACTION`), "? ", as.character(Num_Value))) %>%
      #   fselect("Org ID" = "ORG ID", 
      #           "Station (Site) Name" = "SITE NAME",
      #           "Site ID" = "SITE ID",
      #           "Activity Date" = "ACTIVITY DATE",
      #           "Activity ID" = "ACTIVITY ID",
      #           "Latitude" = "LATITUDE",
      #           "Longitude" = "LONGITUDE",
      #           "Waterbody Type" = "SITE TYPE",
      #           "Medium" = "ACTIVITY MEDIA",
      #           "Medium Detail" = "ACTIVITY SUBMEDIA",
      #           "Activity Type" = "ACTIVITY TYPE",
      #           "Activity \r\nTime" = "ACTIVITY START TIME",
      #           "Other Fraction \r\nConcentration\r\n(U find analyte)") %>%
      #   group_by(across(-`Other Fraction \r\nConcentration\r\n(U find analyte)`)) %>%
      #   slice(1)
      
      # Get records with address
      x4_Ad <- x4 %>% fsubset(!is.na(Address))
      
      # Based on the rank of address, determine the records to keep
      x5 <- x4_Ad %>%
        group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                 `ACTIVITY ID`, Address) %>%
        drop_na(Value) %>%
        filter(Rank == min(Rank, na.rm = TRUE)) %>%
        # If multiple values exist, only keep the one that `RESULT UNIT` == %in% Units
        mutate(Rank2 = ifelse(`RESULT UNIT` %in% Units, 1, 2)) %>%
        slice(1) %>%
        ungroup()
      
      # Get the hardness dataset
      hardness_dat <- x5 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% 
                  c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3")) %>%
        # Add the final notes
        fmutate(Value = case_when(
          is.na(Num_Value)                         ~str_c("<  ", as.character(Num_Det_Value)),
          `RESULT VALUE TYPE` %in% "Calculated"    ~str_c("C ", as.character(Num_Value)),
          TRUE                                     ~as.character(Num_Value)
        )) %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium Detail" = "ACTIVITY SUBMEDIA",
                "Activity Type" = "ACTIVITY TYPE",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address") %>%
        # Join with table header names
        left_join(address_dat, by = "Address")
      
      x6 <- x5 %>%
        fsubset(!`CHARACTERISTIC NAME` %in% 
                  c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3"))
      
      # Prepare the main dataset
      x7 <- x6 %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium Detail" = "ACTIVITY SUBMEDIA",
                "Activity Type" = "ACTIVITY TYPE",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address",
                "ResultCommentText") %>%
        # Join with table header names
        left_join(address_dat, by = "Address") 
      
      # x8 <- x7 %>%
      #   bind_rows(hardness_dat) %>%
      #   drop_na(Name) %>%
      #   fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`) %>%
      #   pivot_wider(names_from = "Name", values_from = "Value",
      #               values_fn = \(x) paste0(x, collapse = " ")) %>%
      #   left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
      #                             "Activity ID", "Latitude", "Longitude", "Waterbody Type",
      #                             "Medium", "Medium Detail", "Activity Type", 
      #                             "Activity \r\nTime")) %>%
      #   fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
      #   mutate(across(everything(), .fns = as.character))
      
      x8 <- x7 %>%
        bind_rows(hardness_dat) %>%
        drop_na(Name) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
        pivot_wider(names_from = "Name", values_from = "Value",
                    values_fn = \(x) paste0(x, collapse = " ")) %>%
        # left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
        #                           "Activity ID", "Latitude", "Longitude", "Waterbody Type",
        #                           "Medium", "Medium Detail", "DATUM",
        #                           "Activity \r\nTime")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
        mutate(across(everything(), .fns = as.character))
      
      x8_comment <- x7 %>%
        bind_rows(hardness_dat) %>%
        drop_na(Name) %>%
        fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
        fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
        fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                           paste0(head_copy, ": ", ResultCommentText),
                                           NA)) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                -Value, -head_copy, -Name) %>%
        group_by(across(-ResultCommentText)) %>%
        summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                    paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
        mutate(across(everything(), .fns = as.character)) %>%
        fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                Latitude, Longitude, `Waterbody Type`, Medium,  `Medium Detail`,
                `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
      
      x8_final <- x8 %>%
        left_join(x8_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                     "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium Detail",
                                     "Activity \r\nTime"))
      
      # Join the TADA_file
      
      if (nrow(TADA_file) == 0){
        x8_final2 <- x8_final
      } else {
        TADA_file2 <- TADA_file %>%
          fselect("Org ID" = "OrganizationIdentifier",
                  "Station (Site) Name" = "MonitoringLocationName",
                  "Site ID" = "MonitoringLocationIdentifier",
                  "Activity Date" = "ActivityStartDate",
                  "Activity ID" = "ActivityIdentifier",
                  "Activity \r\nTime" = "ActivityStartTime.Time",
                  "Latitude" = "LatitudeMeasure",
                  "Longitude" = "LongitudeMeasure",
                  "Depth") %>%
          distinct() %>%
          mutate(across(everything(), .fns = as.character)) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`))
        
        x8_final2 <- x8_final %>%
          left_join(TADA_file2, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity \r\nTime",
                                       "Activity ID", "Latitude", "Longitude"))
      }
      
      # Join to the header
      x9 <- header %>%
        mutate(across(everything(), .fns = as.character)) %>%
        bind_rows(x8_final2) %>%
        arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
    } else {
      x9 <- header
    }
    
  } else {
    x9 <- header
  }
  return(x9)
}

# A function to prepare the GIS output 
GIS_output_salinity <- function(x){
  x2 <- x %>% mutate(across(c(Depth:`SO4 \r\n(mg/L) \r\nD`,
                              `Conductivity\r\n(uS/cm)`:`Hardness, \r\nCa, Mg \r\nas CaCO3\r\nDiss\r\n(mg/L)`), 
                            .fns = number_clean))
  return(x2)
}

### Oil Gas tab

# This function takes the summary tab as the input argument
oil_tab <- function(x, address_dat, lookup, assign_fraction, unit_dat, header){
  
  if (nrow(x) > 0){
    par_name <- unique(lookup$Parameter)
    
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      
      # Join x2 with lookup
      x3 <- x2 %>%
        # Impute fraction for those special parameters
        left_join(assign_fraction, by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fmutate(`SAMPLE FRACTION` = as.character(ifelse(is.na(`SAMPLE FRACTION`) & !is.na(Assign_Fraction),
                                                        Assign_Fraction, `SAMPLE FRACTION`))) %>%
        fselect(-Assign_Fraction) %>%
        left_join(lookup %>% select(-PCode), 
                  by = c("CHARACTERISTIC NAME" = "Parameter",
                         "SAMPLE FRACTION" = "Fraction")) 
      
      # Conduct unit conversion
      x4 <- x3 %>% SANDS_unit(unit_dat = unit_dat)
      
      # # Get records without address
      # x4_noAd <- x4 %>% 
      #   fsubset(is.na(Address)) %>%
      # # Create the columns with fraction and values
      # fmutate(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
      #           str_c(reNA(`SAMPLE FRACTION`), "? ", as.character(Num_Value))) %>%
      # fselect("Org ID" = "ORG ID", 
      #         "Station (Site) Name" = "SITE NAME",
      #         "Site ID" = "SITE ID",
      #         "Activity Date" = "ACTIVITY DATE",
      #         "Activity ID" = "ACTIVITY ID",
      #         "Latitude" = "LATITUDE",
      #         "Longitude" = "LONGITUDE",
      #         "Waterbody Type" = "SITE TYPE",
      #         "Medium" = "ACTIVITY MEDIA",
      #         "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
      #         "Activity\r\nType" = "ACTIVITY TYPE",
      #         "Activity \r\nTime" = "ACTIVITY START TIME",
      #         "Other Fraction \r\nConcentration\r\n(U find analyte)") %>%
      # group_by(across(-`Other Fraction \r\nConcentration\r\n(U find analyte)`)) %>%
      # slice(1)
      
      # Get records with address
      x4_Ad <- x4 %>% fsubset(!is.na(Address))
      
      # Based on the rank of address, determine the records to keep
      x5 <- x4_Ad %>%
        group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                 `ACTIVITY ID`, Address) %>%
        filter(Rank == min(Rank, na.rm = TRUE)) %>%
        # If multiple values exist, only keep the one that `RESULT UNIT` == %in% Units
        mutate(Rank2 = ifelse(`RESULT UNIT` %in% Units, 1, 2)) %>%
        slice(1) %>%
        ungroup()
      
      # Get the hardness dataset
      hardness_dat <- x5 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% 
                  c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3")) %>%
        # Add the final notes
        fmutate(Value = case_when(
          is.na(Num_Value)                         ~str_c("<  ", as.character(Num_Det_Value)),
          `RESULT VALUE TYPE` %in% "Calculated"    ~str_c("C ", as.character(Num_Value)),
          TRUE                                     ~as.character(Num_Value)
        )) %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                "Activity\r\nType" = "ACTIVITY TYPE",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address") %>%
        # Join with table header names
        left_join(address_dat, by = "Address")
      
      x6 <- x5 %>%
        fsubset(!`CHARACTERISTIC NAME` %in% 
                  c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3"))
      
      # Prepare the main dataset
      x7 <- x6 %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                "Activity\r\nType" = "ACTIVITY TYPE",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address",
                "ResultCommentText") %>%
        # Join with table header names
        left_join(address_dat, by = "Address") 
      
      # x8 <- x7 %>%
      #   bind_rows(hardness_dat) %>%
      #   drop_na(Name) %>%
      #   fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`) %>%
      #   pivot_wider(names_from = "Name", values_from = "Value",
      #               values_fn = \(x) paste0(x, collapse = " ")) %>%
      #   # left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
      #   #                           "Activity ID", "Latitude", "Longitude", "Waterbody Type",
      #   #                           "Medium",  Detail", "Activity\r\nType", 
      #   #                           "Activity \r\nTime")) %>%
      #   fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
      #   mutate(across(everything(), .fns = as.character))
      
      x8 <- x7 %>%
        bind_rows(hardness_dat) %>%
        drop_na(Name) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
        pivot_wider(names_from = "Name", values_from = "Value",
                    values_fn = \(x) paste0(x, collapse = " ")) %>%
        # left_join(x4_noAd, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date",
        #                           "Activity ID", "Latitude", "Longitude", "Waterbody Type",
        #                           "Medium", "Medium Detail", "DATUM",
        #                           "Activity \r\nTime")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
        mutate(across(everything(), .fns = as.character))
      
      x8_comment <- x7 %>%
        bind_rows(hardness_dat) %>%
        drop_na(Name) %>%
        fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
        fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
        fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                           paste0(head_copy, ": ", ResultCommentText),
                                           NA)) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                -Value, -head_copy, -Name) %>%
        group_by(across(-ResultCommentText)) %>%
        summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                    paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
        mutate(across(everything(), .fns = as.character)) %>%
        fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                Latitude, Longitude, `Waterbody Type`, Medium,  `Medium\r\nDetail`,
                `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
      
      x8_final <- x8 %>%
        left_join(x8_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                     "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium\r\nDetail",
                                     "Activity \r\nTime"))
      
      # Join to the header
      x9 <- header %>%
        mutate(across(everything(), .fns = as.character)) %>%
        bind_rows(x8_final) %>%
        arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      
    } else {
      
      x9 <- header
    
    }
    
  } else {
    x9 <- header
  }
  
  return(x9)
}

# A function to prepare the GIS output 
GIS_output_oil <- function(x){
  x2 <- x %>% mutate(across(`Organic\r\nCarbon\r\n(ug/l total)`:`Carbonate as CO3\r\n(mg/L)_D`, 
                            .fns = number_clean))
  return(x2)
}

### Helper functions

# A helper function to replace NA with ""
reNA <- function(x){
  x2 <- str_replace_na(x, "")
  return(x2)
}

# A helper function to convert 24 hours to 12 hours AM PM time
convert_time <- function(x){
  y <- format(strptime(x, format = "%H:%M"), "%I:%M %p")
  return(y)
}

# A helper function for unit conversion
SANDS_unit <- function(x, unit_dat){
  x2 <- x %>%
    fmutate(`RESULT UNIT` = str_to_upper(`RESULT UNIT`)) %>%
    fmutate(`DETECTION LIMIT UNIT` = str_to_upper(`DETECTION LIMIT UNIT`)) %>%
    # Create a column summarizing both RESULT UNIT and DETECTION LIMIT UNIT
    fmutate(Original_Unit = coalesce(.$`RESULT UNIT`, .$`DETECTION LIMIT UNIT`)) %>%
    left_join(unit_dat, by = c("Original_Unit" = "Original",
                               "Units" = "Updated")) %>%
    # Fill NA with 1
    fmutate(Conversion_Factor = collapse::replace_na(Conversion_Factor, value = 1),
            Conversion_Coefficient = collapse::replace_na(Conversion_Coefficient, value = 0)) %>%
    # Create a column to documnet the numeric value
    fmutate(Num_Value = as.numeric(`RESULT VALUE`)) %>%
    fmutate(Num_Det_Value = as.numeric(`DETECTION LIMIT VALUE`)) %>%
    # Unit conversion
    fmutate(Num_Value = Num_Value * Conversion_Factor + Conversion_Coefficient) %>%
    fmutate(Num_Det_Value = Num_Det_Value * Conversion_Factor + Conversion_Coefficient) %>%
    # Unit conversion
    fmutate(Value = case_when(
      # If below detection limit: Num_Value = NA & Num_Det_Value not NA
      # Add < and Num_Det_Value
      is.na(Num_Value) & !is.na(Num_Det_Value)  
      ~str_c("<  ", as.character(Num_Det_Value)),
      # If `RESULT VALUE`is NA, add <
      is.na(`RESULT VALUE`) 
      ~str_c("<  ", reNA(`DETECTION LIMIT VALUE`)),
      # If Num_Value is not NA, add the qualifier code
      !is.na(Num_Value) 
      ~str_c(as.character(Num_Value), " ", reNA(`QUALIFIER CODE`)),
      TRUE             ~NA_character_
    ))
  return(x2)
}

# A helper function to remove all symbols and letter except decimal point
# to keep numbers
number_clean <- function(x) {
  result <- str_replace_all(x, "[^0-9. ]", "")
  result <- str_trim(result)
  return(result)
}

# ### Excel format function
# 
# # A function to add data to a blank new sheet
# Excel_paste <- function(wb, sheet_from, sheet_to, dat){
#   wb2 <- wb %>%
#     wb_add_worksheet(sheet = sheet_to) %>%
#     wb_add_data(x = dat, na.strings = NULL) %>%
#     wb_clone_sheet_style(from = sheet_from, to = sheet_to)
# 
#   return(wb2)
# }
# 
# # A function to add data to a new sheet and format as a table
# Excel_table <- function(wb, sheet_from, sheet_to, dat){
#   wb2 <- wb %>%
#     wb_add_worksheet(sheet = sheet_to) %>%
#     wb_add_data_table(x = dat, tableStyle = "TableStyleMedium9", na.strings = NULL) %>%
#     wb_clone_sheet_style(from = sheet_from, to = sheet_to)
# }
# 
# # A function to add data to existing columns
# Excel_paste2 <- function(wb, sheet_from, sheet_to, dat,
#                          startCol, startRow){
#   wb2 <- wb %>%
#     wb_add_data(sheet = sheet_to, startCol = startCol, startRow = startRow,
#                 na.strings = NULL, x = dat, colNames = FALSE) %>%
#     wb_clone_sheet_style(from = sheet_from, to = sheet_to)
#   return(wb2)
# }





