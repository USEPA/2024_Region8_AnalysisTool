# Import Page

mb_limit <- 5

function(){
  tabPanel("Join AUs",
           h2("Join the Assessment Units"),
           fluidRow(
             box(width = 4, status = "primary", solidHeader = TRUE,
                 title = "App Control",
                 h4("Load File")
                 , p("Only comma-separated or tab-separated files.")
                 , h5("Select file parameters")
                 #, checkboxInput('header', 'Header', TRUE)
                 , radioButtons("sep", "Separator",
                                c(Comma = ",",
                                  # Semicolon = ";",
                                  Tab = "\t"),
                                ',')
                 , strong("Data Condition:")
                 , verbatimTextOutput(outputId = "AU_file_indicator")
                 , fileInput("fn_input"
                             , label = "Choose file to upload"
                             , multiple = FALSE
                             , accept = c("text/csv"
                                          , "text/comma-separated-values"
                                          , "text/tab-separated-values"
                                          , "text/plain"
                                          , ".csv"
                                          , ".tsv"
                                          , ".txt")
                 )##fileInput~END
                 , tags$hr()
                 , p("The 'separator' allows the user to upload different file formats
            (e.g., csv, tsv, or txt).")
            , p("Files for all operations will be uploaded through this interface.")
            , p(paste0("File uploads are limited to a maximum of "
                       , mb_limit
                       , " MB in size."))
             ) # END box
            , box(width = 8, status = "primary", solidHeader = TRUE,
                  title = "Preview uploaded data"
              , DT::dataTableOutput("df_import_DT")
            ) # END box
           ), # END fluidRow
           fluidRow(
             box(width = 4, status = "primary", solidHeader = TRUE,
                 title = "App Control"
                 , p("Use the App Control to correctly assign sites to AUs."
                     , "Sites will be joined to the existing "
                     ,a("AU crosswalk table."
                        , href = "https://raw.githubusercontent.com/Blocktt/ShinyAppDocuments/main/AUSpatialJoin/MonLoc_to_AU_Crosswalk_20240415.xlsx"
                        , target="_blank")
                     , "Sites not included in the table will be spatially joined"
                     , "to the EPA ATTAINS spatial layers. These sites will need analyst review."
                     , "Results will be shown via the table and map below once the join is complete.")
                 , h4("1. Join Monitoring Locations to AUs")
                 , actionButton("b_Calc", "Join AUs")
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
                 , p ("Download the WQP data with AU assignments if the WQP data was downloaded in the 'Download Page'")
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
  )##tabPanel ~ END
}## FUNCTION ~ END
