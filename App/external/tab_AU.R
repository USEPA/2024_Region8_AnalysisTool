# Import Page
function(){
  tabPanel("Join AUs",
           h2("Join Sites to the Assessment Units"),
           fluidRow(
             box(width = 6, status = "primary", solidHeader = TRUE,
                 title = "WQP Data Upload",
                 strong("Data Condition:"),
                 verbatimTextOutput(outputId = "file_indicator_AU"),
                 fileInput(inputId = "file_AU", label = "", accept = ".csv"),
                 verbatimTextOutput(outputId = "Date_Info_AU", placeholder = TRUE)
             ) # END box
            , box(width = 6, status = "primary", solidHeader = TRUE,
                  title = "AU Crosswalk Table Upload",
                  radioButtons(inputId = "crosswalk_upload", 
                               label = "Select the crosswalk table",
                               choices = c("Use the default crosswalk table",
                                           "Upload a new crosswalk table")),
                  strong("Users should ensure they have institutional permissions to make manual changes to the AU crosswalk table"),
                  p(),
                  fileInput(inputId = "file_crosswalk", label = "", accept = ".csv"),
                  verbatimTextOutput(outputId = "file_indicator_crosswalk")
            ) # END box
           ), # END fluidRow
           fluidRow(
             box(width = 4, status = "primary", solidHeader = TRUE,
                 title = "App Control"
                 , p("Use the App Control to correctly assign sites to AUs."
                     , "Sites will be joined to the existing AU crosswalk table."
                     , "Sites not included in the table will be spatially joined"
                     , "to the EPA ATTAINS spatial layers. These sites will need analyst review."
                     , "Results will be shown via the table and map below once the join is complete.")
                 , h4("1. Join Monitoring Locations to AUs")
                 , actionButton("b_Calc", "Join Sites to AUs")
                 , h4("2. Download Results")
                 , p("All input and output files will be available in a single zip file.")
                 # , shinyjs::disabled(downloadButton("b_download"
                 #                                    , "Download Results"))
                 , downloadButton("b_download", "Download Results")
                 , h4("3. Select Monitoring Location")
                 , p("Once map and table are loaded, enter a monitoring location identifier below"
                     , "to zoom into that location.")
                 , textInput(inputId = "input_site_choice"
                             , label = "MonitoringLocationIdentifier:"
                             , value = "")
                 , h4 ("4. Download the WQP data with AU assignments")
                 , downloadButton("WQP_AU_Download", "Download the WQP data with AU")
                 , p ("Download the WQP data with AU assignments if the WQP data was downloaded in the 'Data Download'")
                 , actionButton(inputId = "Reset2", "Reset")
             ) # END box
             # , valueBoxOutput("MatchCount", width = 3)
             # , valueBoxOutput("UnMatchCount_L50", width = 3)
             # , valueBoxOutput("UnMatchCount_G50", width = 3)
             , box(width = 8, status = "primary", solidHeader = TRUE
                   , title = "Results Map"
                   , leafletOutput("mymap")
             ) # END box
           ) # END fluidRow
           , fluidRow(
             box(width = 6, status = "primary", solidHeader = TRUE
                 , title = "Results Table"
                 , DT::dataTableOutput("df_results_DT")
             ) # END box
             , valueBoxOutput("MatchCount", width = 2)
             , valueBoxOutput("UnMatchCount_L50", width = 2)
             , valueBoxOutput("UnMatchCount_G50", width = 2)
             # , box(width = 6, status = "info", solidHeader = TRUE
             #       , title = "Results Map"
             #       , leafletOutput("mymap")
             # ) # END box
           )
           , fluidRow(
             box(width = 12, status = "primary", solidHeader = TRUE,
                 title = "SANDS Format with Spatial Join to AUs", collapsible = TRUE,
                 p(),
                 verbatimTextOutput(outputId = "SANDS_info", placeholder = TRUE),
                 p(),
                 checkboxInput(inputId = "SANDS_check2", label = "Create the SANDS output as CSV files in a ZIP folder"),
                 checkboxGroupButtons(inputId = "MT_par_group2",
                                      label = "Select the filter groups",
                                      choices = c("Metals (Water Column)" = "metal_w", 
                                                  "Metals (Sediment)" = "metal_s", 
                                                  "Salinity" = "salinity", 
                                                  "Oil & Gas" = "oil_gas", 
                                                  "Nutrients" = "nutrients")),
                 downloadButton(outputId = "SANDS_data_download2", label = "Save data")
             ) 
           )
  )##tabPanel ~ END
}## FUNCTION ~ END
