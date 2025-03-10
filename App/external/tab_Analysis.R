# Analysis Page
function(){
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
              verbatimTextOutput(outputId = "file_indicator_AN"),
              fileInput(inputId = "file_AN", label = "", accept = ".csv"),
              verbatimTextOutput(outputId = "Date_Info_AN", placeholder = TRUE),
              awesomeRadio(inputId = "SU_filter",
                          label = "Filter the data based on the sufficiency tests results",
                          choices = c("All Data",
                                      "Report site with any sufficient data",
                                      "Report AU with any sufficient data"),
                          selected = "All Data"),
              awesomeRadio(inputId = "sites_or_AU_AN",
                           label = "Assess site or AU",
                           choices = c("Site", "AU")),
              strong("Review all sites or AUs"),
              switchInput(inputId = "all_sites_AU", label = "", value = TRUE),
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
              # strong("Review all fractions"),
              # switchInput(inputId = "all_fractions", label = "", value = TRUE),
              # sidebarPanel(id = "site_sidebar_AN",
              #              width = 12,
              #              fluidRow(
              #                column(
              #                  width = 12,
              #                  selectizeInput(inputId = "fraction_combined_AN", label = "Select the fractions to combined",
              #                                 choices = NULL, multiple = TRUE,
              #                                 width = "100%")
              #                )
              #              )
              # ),
              strong("Criteria Information:"),
              verbatimTextOutput(outputId = "criteria_info_AN")
            ),
            column(
              width = 8,
              h2("Parameter Criteria Summary Table"),
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
          title = "Monitoring Site or AU Selection Table", 
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
          downloadButton(outputId = "data_download_selected_AN", 
                         label = "Save selected data"),
          downloadButton(outputId = "data_download_AN", 
                         label = "Save the WQP data with analytical results"),
          actionButton(inputId = "Reset4", "Reset")
        )
      )
    )
  ) 
}