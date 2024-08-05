# SANDS page
function(){
  tabItem(
    # The tab name
    tabName = "SANDS",
    h2("Data Summary Export"),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Fact Sheet",
          status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          strong("Create the factsheets"),
          verbatimTextOutput(outputId = "factsheet_info"),
          p(),
          radioButtons(inputId = "Site_AU_fact_SANDS", label = "Summarize the data by site or AU",
                       choices = c("Site", "AU")),
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