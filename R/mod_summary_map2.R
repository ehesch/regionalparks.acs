#' summary_map2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_map2_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<p>This map visualizes the geospatial location of the buffers around the user-selected parks and trails along with the selected demographic data. For the demographic data, darker colors mean higher values and lighter colors mean lower values. Demographic data can be turned off using the layer controls found at the bottom right of the map."),
    
    leafletOutput(ns("buffermap"), height = 700)
  )
  
}
    
#' summary_map2 Server Function
#'
#' @noRd 
mod_summary_map2_server <- function(input, output, session,
                                    summary_util,
                                    selected_vars) {
  ns <- session$ns
  
  #buf map --------
  output$buffermap <- renderLeaflet({ 
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       group = "Stamen Toner"
      ) %>%
      addProviderTiles("CartoDB.Positron",
                       group = "Carto Positron"
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Esri Imagery"
      ) %>%
      
      addMapPane("Agency boundaries", zIndex = 650) %>%
      addPolygons(
        data = agency_boundary,
        group = "Agency boundaries",
        stroke = T,
        color = "black",
        fill = F,
        weight = 2,
        options = pathOptions(pane = "Agency boundaries")
      ) %>%
    
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Parks and Trails",
          "Buffers",
          "Demographic data",
          "Agency boundaries"
        ),
        baseGroups = c(
          "Carto Positron",
          "Stamen Toner",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = F)
      ) %>%
    
      leaflet::addScaleBar(position = c("bottomleft"))
  }) #----
  
  #----
  observe({ 
    # req(nrow(summary_util$map_bg_data)>0)

    leafletProxy("buffermap", data = summary_util$map_bg_data) %>%
      clearGroup("Demographic data") %>%
      # clearControls() %>%
      addPolygons(
        group = "Demographic data",
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ colorNumeric(
          # n = 7,
          palette = "Blues",
          domain = summary_util$map_bg_data[[1]]
        )(summary_util$map_bg_data[[1]])
      )
    event <- input$buffermap_map_shape_click
  })
  
}

    
## To be copied in the UI
# mod_summary_map2_ui("summary_map2_ui_1")
    
## To be copied in the server
# callModule(mod_summary_map2_server, "summary_map2_ui_1")
 
