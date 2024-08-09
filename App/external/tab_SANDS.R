# SANDS page
function(){
  tabItem(
    # The tab name
    tabName = "SANDS",
    h2("Data Summary Export"),
    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "Notes",
          p("The fact sheet creation function focuses on the metal parameters in Montans.")
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Fact Sheet",
          status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          p("The fact sheet creation function focuses on the metal parameters."),
          strong("Create the factsheets"),
          verbatimTextOutput(outputId = "factsheet_info"),
          radioButtons(inputId = "Site_AU_fact_SANDS", label = "Summarize the data by site or AU",
                       choices = c("Site", "AU")),
          strong("convert the fraction 'Total' to 'Total Recoverable:'"),
          switchInput(inputId = "total_convert"),
          p(),
          actionButton(inputId = "create_factsheets", "Create"),
          p(),
          verbatimTextOutput(outputId = "factsheet_info2"),
          downloadButton(outputId = "fact_sheet_data_download",
                         label = "Save data")
        )
      ),
      column(
        width = 6,
        box(
          title = "Reset", 
          status = "warning", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          actionButton(inputId = "Reset5", "Reset")
        )
      )
    )
  )
}