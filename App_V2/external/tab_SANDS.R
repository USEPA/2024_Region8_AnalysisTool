# SANDS page
function(){
  tabItem(
    # The tab name
    tabName = "SANDS",
    h2("SANDS Format"),
    fluidRow(
      column(
        width = 12,
        box(
          title = "SANDS Format", 
          status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          checkboxGroupButtons(inputId = "MT_par_group",
                               label = "Select the filter groups",
                               choices = c("Metals (Water Column)" = "metal_w", 
                                           "Metals (Sediment)" = "metal_s", 
                                           "Salinity" = "salinity", 
                                           "Oil & Gas" = "oil_gas", 
                                           "Nutrients" = "nutrients")),
          h4("Save Data"),
          downloadButton(outputId = "SANDS_data_download", label = "Save Data")
        )
      )
    )
  )
}