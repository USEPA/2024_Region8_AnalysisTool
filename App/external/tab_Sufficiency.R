# Data Sufficiency
function(){
  tabItem(tabName = "Sufficiency",
          h2("Data Sufficiency"),
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
                    verbatimTextOutput(outputId = "file_indicator_SU"),
                    fileInput(inputId = "file_SU", label = "", accept = ".csv"),
                    verbatimTextOutput(outputId = "Date_Info_SU", placeholder = TRUE),
                    radioButtons(inputId = "sites_or_AU_SU",
                                 label = "Assess site or AU",
                                 choices = c("Site", "AU")),
                    strong("Combined fractions"),
                    switchInput(inputId = "all_fractions_SU", label = "", value = FALSE),
                    sidebarPanel(id = "site_sidebar_SU",
                                 width = 12,
                                 fluidRow(
                                   column(
                                     width = 12,
                                     selectizeInput(inputId = "fraction_combined_SU", label = "Select the fractions to combined",
                                                    choices = NULL, multiple = TRUE,
                                                    width = "100%")
                                   )
                                 )
                    )
                  ),
                  column(
                    width = 8,
                    h2("Parameter Sufficiency Summary Table"),
                    fluidRow(
                      column(
                        width = 12,
                        DTOutput(outputId = "Par_Select_SU")
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
                title = "Monitoring Site or AU Selection Table", 
                status = "warning", solidHeader = TRUE,
                width = 6, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 12,
                    # Slider to filter the exceedance percentage
                    strong("Selected Parameter:"),
                    textOutput(outputId = "parameter_name1_SU"),
                    DTOutput(outputId = "site_table_SU")
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
                    textOutput(outputId = "parameter_name2_SU"),
                    leafletOutput(outputId = "site_map_SU")
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
                width = 6, collapsible = TRUE,
                downloadButton(outputId = "data_download_SU", 
                               label = "Save the WQP data with sufficiency test results"),
                actionButton(inputId = "Reset3", "Reset")
              )
            )
          )
  )##tabItem ~ END
}## FUNCTION ~ END
