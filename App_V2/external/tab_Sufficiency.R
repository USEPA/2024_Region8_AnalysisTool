# Data Sufficiency
function(){
  tabItem(tabName = "Sufficiency",
           h2("Data Sufficiency"),
           fluidRow(
             box(width = 4, status = "primary", solidHeader = TRUE,
                 title = "App Control",
                 h4("Load File")
                 , p("Load the data sufficiency table. If no uploaded file, the tool will use the default data sufficiency table for the analysis.")
                 , fileInput("sus_fn_input"
                             , label = "Choose file to upload"
                             , multiple = FALSE
                             , accept = c("csv")
                 )##fileInput~END
             ) # END box
             , box(width = 8, status = "primary", solidHeader = TRUE,
                   title = "Preview the data sufficiency table"
                   , DTOutput("Sus_DT")
             ) # END box
           ), # END fluidRow
           fluidRow(
             box(width = 4, status = "primary", solidHeader = TRUE,
                 title = "App Control",
                 p("Use the App Control to calculate the data sufficiency of each site and AU."),
                 h4("1. Calculate the data sufficiency"),
                 p("The tool converts the dataset's characteristic names, fractions, and units before calculating the data sufficiency."),
                 p("Click here to view the characteristic names, fractions, and units."),
                 actionButton("sus_Calc", "Calculate"),
                 h4 ("2. Download the WQP data with data sufficiency calculation."),
                 downloadButton("WQP_SUS_Download", "Download the WQP data with data sufficiency")
             ) # END box
             , box(width = 8, status = "primary", solidHeader = TRUE
                   , title = "Data Sufficiency Results"                   
                   , radioButtons(inputId = "site_AU_select",
                                  label = "View the results as Site or AU",
                                  choices = c("Site", "AU"))
                   , DTOutput(outputId = "Sus_Par_Select")
             ) # END box
          ), # END fluidRow
          fluidRow(
              box(
                title = "Monitoring Site Selection Table",
                status = "warning", solidHeader = TRUE,
                width = 6, collapsible = TRUE,
                  column(
                    width = 12,
                    # Slider to filter the exceedance percentage
                    strong("Selected Parameter:"),
                    textOutput(outputId = "sus_parameter_name1"),
                    DTOutput(outputId = "sus_site_table")
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
                    textOutput(outputId = "sus_parameter_name2"),
                    leafletOutput(outputId = "sus_site_map")
                  )
                )
            )
          )
  )##tabItem ~ END
}## FUNCTION ~ END
