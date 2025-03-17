# R/mod_map.R

#' Map Module
#'
#' @description A module to create maps
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# UI function
mapUI <- function(id) {
  ns <- NS(id)

  tagList(
    leafletOutput(ns("map"))
  )
}

#' Data download handler module
#'
#' @noRd

# Server function
mapServer <- function(id, map_type, data = NULL, bounds = NULL) {
  moduleServer(id, function(input, output, session) {

    # Initialize map
    output$map <- renderLeaflet({
      if (map_type == "HUC8") {
        map <- leaflet() %>%
          addPolygons(data = data(),
                      fill = TRUE,
                      fillColor = "blue",
                      color = "blue",
                      label = ~huc8,
                      weight = 1,
                      layerId = ~huc8,
                      group = "base_map") %>%
          add_USGS_base()
      } else if (map_type == "BBox") {
        map <- leaflet() %>%
          fitBounds(lng1 = -102.11928, lat1 = 50.58677,
                    lng2 = -117.65217, lat2 = 40.37306) %>%
          addDrawToolbar(polylineOptions = FALSE,
                         circleOptions = FALSE,
                         markerOptions = FALSE,
                         circleMarkerOptions = FALSE,
                         polygonOptions = FALSE,
                         singleFeature = TRUE) %>%
          add_USGS_base()
      }

      return(map)
    })

    # Return reactive values for captured events
    events <- reactiveValues(
      selected = NULL,
      bounds = NULL
    )

    # Handle map click events for HUC8
    if (map_type == "HUC8") {
      observeEvent(input$map_shape_click, {
        click <- input$map_shape_click
        click_id <- click$id

        # Store current selection
        current_selected <- events$selected

        # Toggle selection
        if (isTruthy(!click_id %in% current_selected)) {
          current_selected <- c(current_selected, click_id)
        } else {
          current_selected <- current_selected[!current_selected %in% click_id]
        }

        events$selected <- current_selected
      })

      # Create leaflet proxy for highlighting selected HUCs
      map_proxy <- leafletProxy("map", session)

      # Watch for changes in selection and update map
      observe({
        req(events$selected)
        selected_data <- data() %>%
          filter(huc8 %in% events$selected)

        if (nrow(selected_data) > 0) {
          map_proxy %>% clearGroup(group = "highlighted_polygon")
          map_proxy %>%
            addPolylines(data = selected_data,
                         stroke = TRUE,
                         color = "red",
                         weight = 2,
                         group = "highlighted_polygon")
        } else {
          map_proxy %>% clearGroup(group = "highlighted_polygon")
        }
      })
    }

    # Handle bounding box drawing for BBox
    if (map_type == "BBox") {
      observeEvent(input$map_draw_new_feature, {
        feat <- input$map_draw_new_feature
        coords <- unlist(feat$geometry$coordinates)
        coords_m <- matrix(coords, ncol = 2, byrow = TRUE)
        poly <- st_sf(st_sfc(st_polygon(list(coords_m))), crs = st_crs(27700))
        bbox_temp <- st_bbox(poly)

        # Update the bounding box
        events$bounds <- list(
          west = as.numeric(bbox_temp[1]),
          south = as.numeric(bbox_temp[2]),
          east = as.numeric(bbox_temp[3]),
          north = as.numeric(bbox_temp[4])
        )
      })
    }

    return(events)
  })
}
