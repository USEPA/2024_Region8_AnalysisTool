# Main body

library(shiny)
library(shinyjs)

# Main body function
function(id){
  tabItems(
    # tabItem(tabName = "Intro",
    #         tab_Intro()),
    tabItem(tabName = "Download",
            tab_Download()),
    tabItem(tabName = "AU",
            tab_AU()),
    tabItem(tabName = "Sufficiency",
            tab_Sufficiency()),
    tabItem(tabName = "Analysis",
            tab_Analysis()),
    tabItem(tabName = "SANDS",
            tab_SANDS())
  )## tabItems
} ## Mian body function ends