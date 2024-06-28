#Create MTDEQ's summary sheets

#Created by Hannah Ferriby
#Date created: 2024-06-25
#Last updated: 2024-06-28


####Load Libraries####
library(tidyverse) 
library(sf)
library(here)
library(readxl)
library(lubridate)
library(openxlsx)

####Load Data####
samples <- read_csv(here::here('Data', 'WQP_data_AU_2024-06-26.csv'))

er3 <- st_read(here::here('Data', 'MT_ecoregion_lv3.shp')) %>%
  select(US_L3NAME)

criteria_table <- read_xlsx(here::here('Data', 'CriteriaTable_v3_ycw3.xlsx')) %>%
  filter(State == 'Montana')

flow_dates <- read_xlsx(here::here('Data', 'highflowdates.xlsx'))

####Filter data####
parameters <- criteria_table %>%
  select(Constituent) %>%
  unique()

samples_filtered <- samples %>%
  filter(CharacteristicName %in% parameters$Constituent) %>%
  filter(CharacteristicName %in% c('Aluminum',
                                   'Arsenic',
                                   'Cadmium',
                                   'Copper',
                                   'Iron',
                                   'Lead',
                                   'Selenium',
                                   'Silver',
                                   'Zinc',
                                   'Mercury'))


####Spatial Join MLs to ER3####
mls_filtered_spatial <- samples_filtered %>%
  select(MonitoringLocationIdentifier, LongitudeMeasure, LatitudeMeasure) %>%
  unique() %>%
  st_as_sf(coords = c('LongitudeMeasure','LatitudeMeasure'), remove = F) %>%
  st_set_crs(4617) %>% #NAD83 EPSG
  st_transform(st_crs(er3))

mls_w_er3 <- mls_filtered_spatial %>%
  st_intersection(er3)

sample_w_er3 <- samples_filtered %>%
  right_join(mls_w_er3, by = c('MonitoringLocationIdentifier'))


####List AUs####
list_aus <- samples_filtered %>%
  select(AU_ID) %>%
  unique() %>%
  filter(!str_detect(AU_ID, 'WY')) %>% #Remove one WY site
  pull()


####Pull relevant data by AU####

#MAKE INTO LOOP
# test_au <- list_aus[1]

for(i in list_aus[1:5]) {

  print(i)
  
  au_sample_pull <- sample_w_er3 %>%
    filter(AU_ID == i) %>%
    #Convert all caps to title case
    mutate(TADA.ResultSampleFractionText = str_to_title(TADA.ResultSampleFractionText),
           TADA.ResultSampleFractionText = ifelse(TADA.ResultSampleFractionText == 'Total',
                                                  'Total Recoverable',
                                                  TADA.ResultSampleFractionText))
  
  au_er3 <- au_sample_pull %>%
    select(US_L3NAME) %>%
    unique() %>%
    pull()
  
  hf_start <- flow_dates %>%
    filter(US_L3NAME == au_er3) %>%
    select(StartDate) %>%
    pull()
  
  hf_end <- flow_dates %>%
    filter(US_L3NAME == au_er3) %>%
    select(EndDate) %>%
    pull()
  
  au_constituents <- au_sample_pull %>%
    select(CharacteristicName, TADA.ResultSampleFractionText) %>%
    unique() %>%
    #Convert all caps to title case
    mutate(TADA.ResultSampleFractionText = str_to_title(TADA.ResultSampleFractionText))
  
  au_standards <- criteria_table %>%
    filter(Constituent %in% au_constituents$CharacteristicName)
  
  combine_stands_const <- au_constituents %>% 
    right_join(au_standards, by = c('CharacteristicName' = 'Constituent',
                                   'TADA.ResultSampleFractionText' = 'Fraction')) 
  
  combine_samples_stands <- au_sample_pull %>%
    right_join(combine_stands_const, by = c('CharacteristicName',
                                            'TADA.ResultSampleFractionText')) %>%
    mutate(doy = lubridate::yday(ActivityStartDate)) 
  
  au_uses <- au_standards %>%
    select(Use) %>%
    filter(Use != 'Trigger Value') %>%
    unique() %>%
    pull()
  
  #Determine which parameters are missing, create columns for them to 
  #cbind to final table
  parameters <- c('Aluminum','Arsenic', 'Cadmium','Copper', 'Iron',
                  'Lead','Selenium','Silver', 'Zinc','Mercury')
  
  missing_params <- which(!parameters %in% au_constituents$CharacteristicName)
  missing_params_names <- parameters[missing_params]
  
  columns_aq_use <- c(i, unique(au_sample_pull$AU_NAME), 'Aquatic Life', NA, NA, NA, NA,
                      NA, NA, NA, NA, NA)
  
  columns_hh_use <- c(i, unique(au_sample_pull$AU_NAME), 'Human Health', NA, NA, NA, NA, NA)
  
  columns_aq_use_to_add <- columns_aq_use %>%
    as.data.frame() %>%
    rep(each = length(missing_params_names)) %>%
    as.data.frame()
  colnames(columns_aq_use_to_add) <- missing_params_names
  
  
  columns_hh_use_to_add <- columns_hh_use %>%
    as.data.frame() %>%
    rep(each = length(missing_params_names)) %>%
    as.data.frame()
  colnames(columns_hh_use_to_add) <- missing_params_names
  
  
  summary_table_list <- list()
  counter <- 1
  
  for(j in au_uses){
    #MAKE INTO NESTED LOOP
    # test_use <- au_uses[1]
    if(j == 'Aquatic Life') {
    summary_table <- combine_samples_stands %>%
      filter(Use %in% j) %>%
      group_by(CharacteristicName) %>%
      reframe(AU_ID = AU_ID,
              AU_NAME = AU_NAME,
              Use = Use,
              CharacteristicName = CharacteristicName,
              Sample_Years = paste0(min(year(ActivityStartDate)),
                                    ' - ',
                                    max(year(ActivityStartDate))),
              Number_Samples = n(),
              #If location is Lake -> high flow = NA,
              #If location is stream -> check dates for high flow
              high_flow = ifelse(str_detect(MonitoringLocationTypeName, 'Lake') == F,
                                 ifelse(doy >= hf_start & doy < hf_end, 1, 0),
                                 NA),
              Num_High_Flow = sum(high_flow, na.rm = T),
              Percent_High_Flow = Num_High_Flow/Number_Samples*100,
              acute_2x = ifelse(Details == 'Acute', 
                             ifelse(TADA.ResultMeasureValue >= (2*Magnitude_Upper), 1, 0),
                             NA),
              Num_Samples_2x_Acute = sum(acute_2x, na.rm = T),
              acute = ifelse(Details == 'Acute', 
                             ifelse(TADA.ResultMeasureValue >= Magnitude_Upper, 1, 0),
                             NA),
              Num_Acute_Exceedances = sum(acute, na.rm = T),
              chronic = ifelse(Details == 'Chronic', 
                               ifelse(TADA.ResultMeasureValue >= Magnitude_Upper, 1, 0),
                               NA),
              Num_Chronic_Exceedances = sum(chronic, na.rm = T),
              Acute_Exceedance_Rate = Num_Acute_Exceedances/Number_Samples*100,
              Chronic_Exceedance_Rate = Num_Chronic_Exceedances/Number_Samples*100) %>%
      select(!c(high_flow, acute_2x, acute, chronic)) %>%
      unique()
    } else {
      summary_table <- combine_samples_stands %>%
        filter(Use %in% j) %>%
        group_by(CharacteristicName) %>%
        reframe(AU_ID = AU_ID,
                AU_NAME = AU_NAME,
                Use = Use,
                CharacteristicName = CharacteristicName,
                Sample_Years = paste0(min(year(ActivityStartDate)),
                                      ' - ',
                                      max(year(ActivityStartDate))),
                Number_Samples = n(),
                #If location is Lake -> high flow = NA,
                #If location is stream -> check dates for high flow
                high_flow = ifelse(str_detect(MonitoringLocationTypeName, 'Lake') == F,
                                   ifelse(doy >= hf_start & doy < hf_end, 1, 0),
                                   NA),
                Num_High_Flow = sum(high_flow, na.rm = T),
                Percent_High_Flow = Num_High_Flow/Number_Samples*100,
                exceedances = ifelse(TADA.ResultMeasureValue >= Magnitude_Upper, 1, 0),
                Num_HHS_Exceedances = sum(exceedances, na.rm = T)) %>%
        select(!c(high_flow, exceedances)) %>%
        unique()
    }
    
    table_col_names <- colnames(summary_table)
    
    constituent_names <- summary_table %>%
      select(CharacteristicName) %>%
      pull()
    
    summary_table_wide <- as_tibble(t(summary_table)) 
    colnames(summary_table_wide) <- constituent_names
    summary_table_wide <- summary_table_wide %>%
      cbind(table_col_names) %>%
      relocate(table_col_names) %>%
      rename(Variable = table_col_names) %>%
      slice(-1)
    
    #Adding missing columns if necessary
    if(j == 'Aquatic Life' & nrow(columns_aq_use_to_add) > 0) {
      
      summary_table_wide <- summary_table_wide %>%
        cbind(columns_aq_use_to_add) 
      
      vars <- colnames(summary_table_wide)[-1]
      #Make variable columns in alphabetical order
      summary_table_wide <- summary_table_wide %>%
        relocate(all_of(sort(vars)), .after = last_col())
      
    } else if (j == 'Human Health' & nrow(columns_hh_use_to_add) > 0){
      
      if('Iron' %in% colnames(columns_hh_use_to_add)) {
        columns_hh_use_to_add <- columns_hh_use_to_add %>%
          select(!Iron)
      }
      
      if('Aluminum' %in% colnames(columns_hh_use_to_add)) {
        columns_hh_use_to_add <- columns_hh_use_to_add %>%
          select(!Aluminum)
      }
      
      
      summary_table_wide <- summary_table_wide %>%
        cbind(columns_hh_use_to_add)
      
      vars <- colnames(summary_table_wide)[-1]
      #Make variable columns in alphabetical order
      summary_table_wide <- summary_table_wide %>%
        relocate(all_of(sort(vars)), .after = last_col())
    }
    
    summary_table_list[[counter]] <- summary_table_wide
    counter <- counter + 1
    # write_xlsx(summary_table_wide, here::here('output',paste0(i, '_',j, '.xlsx')))
  }
  
  wb <- createWorkbook()
  addWorksheet(wb, "Summary_Metals")
  
  curr_row <- 1
  for(k in seq_along(summary_table_list)) {
    writeData(wb, "Summary_Metals", names(summary_table_list)[k], startCol = 1, startRow = curr_row)
    writeData(wb, "Summary_Metals", summary_table_list[[k]], startCol = 1, startRow = curr_row+1)
    curr_row <- curr_row + nrow(summary_table_list[[k]]) + 5 #Add spacing between tables
  }
  
  saveWorkbook(wb, here::here('output',paste0(i,'.xlsx')))
}
