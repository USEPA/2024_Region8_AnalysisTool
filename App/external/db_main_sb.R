# Sidebar

library(shiny)
library(shinybusy)
library(shinyWidgets)

# Sidbar function
function(id) {
  dashboardSidebar(
    
    # Use Shiny Busy
    add_busy_spinner(spin = "circle",
                     position = "top-right"
    ),
    useSweetAlert(),
    
    # The sidebar manual
    sidebarMenu(
      id = "tabs",
      # # Tab 1: Introduction Page
      # menuItem(
      #   text = "Introduction Page",
      #   tabName = "Intro",
      #   icon = icon("map")
      # ),
      # Tab 2: Download Page
      menuItem(
        text = "Data Download",
        tabName = "Download",
        icon = icon("pen")
      ),
      # Tab 3: Join AU
      menuItem(
        text = "Join AU",
        tabName = "AU",
        icon = icon("pen")
      ),
      # Tab 4: Data Sufficiency
      menuItem(
        text = "Data Sufficiency",
        tabName = "Sufficiency",
        icon = icon("poll")
      ),
      # Tab 5: Analysis
      menuItem(
        text = "Data Analysis",
        tabName = "Analysis",
        icon = icon("poll")
      ),
      # Tab 6: SANDS format
      menuItem(
        text = "Data Summary Export",
        tabName = "SANDS",
        icon = icon("poll")
      )
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END