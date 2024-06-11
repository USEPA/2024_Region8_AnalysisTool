# Introduction Page
function(){
  tabItem(
    # The tab name
    tabName = "Intro",
    # The header
    h2("Water Quality Portal Data Tool"),
    # The first row in Tab 1
    fluidRow(
      # Box 1 in Tab 1
      box(
        title = "", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        h2("Introduction"),
        p("The Water Quality Portal Data Tool is a tool to download the data from the Water Quality Portal and analyze the data for exceedance, designed for the State of Montana and Wyoming."),
        h3("Data Download"),
        p("To download the data, go to the 'Download Page' by clicking the 'Download Page' button on the side-bar panel'. 
                 The tool offers two formats to organize the downloaded data:"),
        tags$li("Downloading the data as selected individual parameters and save the data as a CSV file with long format."),
        tags$li("Downloading the data as a group of parameters and save the data as an Excel file following the SANDS format."),
        p("Select one of the formats in 'Select the data format'"),
        p("If 'Individual Parameter' is selected, select the parameter names and site types in the following menu on the left.
                If 'SANDS Format' is selected, Select the filter groups in the following menu on the right."),
        p("In the 'Geographic and Date Input' box, select the date range. Choose one of the three methods to select geographic area, then seelct the area. 
                Click the 'Download the data' button to download the data to work space.
                When downloading the data, the tool displays a progress bar to show the progress. 
                When the download is completed, the tool shows a dialog box. Click 'OK' to continue."),
        fluidRow(
          column(width = 12,
                 img(src = "Download_completed.PNG", height = "30%", width = "30%", align = "left"))
        ),
        p("Once the data download is completed, the tool shows summary information in the 'Data Summary' box."),
        p("Click the 'Save Data' button to download the data as a CSV file or an Excel file with 'Individual Parameter' or 'SANDS Format', respectively."), 
        p("Click 'Reset' to reset the inputs."),
        h3("Data Analysis"),
        p("If data are downloaded with the format as the 'Individual Parameter', 
                the 'Analysis Page' provides options to visualize the exceedance information. 
                The analysis can use the data from the 'Download Page' or upload by the users with the same format. 
                Once data are available, select the parameter name, the fraction, 
                the state criteria, the use of criteria, and the details of criteria. 
                The tool would search for the criteria information and compare the measurement with the criteria."),
        p("A table and a map will display the sample size, number of exceedance and percentage of exceedance. 
                Select the site by clicking each row in the table or dot on the map, 
                the measurement and the criteria will be plotted as a time-series plot and a boxplot."),
        p("For hardness-dependent parameter, hardness data are required to calculate the criteria. 
                The criteria are plotted as orange points, while the measurements are plotted as blue points in the time-series plot.
                For parameters with criteria not depending on hardness, the criteria are plotted as an orange line.")
      )
    )
  )
}