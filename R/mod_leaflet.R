#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import leaflet
#' @import councilR
#' @import leaflet.extras
#' @import sf
mod_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = "100%", height = 700)
  )
}

#' leaflet Server Function
#'
#' @noRd
#' @import leaflet
#' @import councilR
#' @import leaflet.extras
mod_leaflet_server <- function(input, output, session, tract_data = tract_data,
                               parktrail_util,
                               selected_parktrail) {
  ns <- session$ns

  output$map <- renderLeaflet({
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.22,
        zoom = 10) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Esri Imagery"
      ) %>%
      addProviderTiles("CartoDB.Positron",
        group = "Carto Positron"
      ) %>%
      addMapPane("parks_geo", zIndex = 420) %>%
      addPolygons(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ], #https://cran.r-project.org/web/packages/sf/vignettes/sf4.html
        group = "Regional Parks - existing",
        stroke = TRUE,
        # weight = 0.5,
        color = e_col,
        fill = TRUE,
        fillColor = e_col, 
        fillOpacity =  0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$name, "<br>", "<em>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$agency, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolygons(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ],
        group = "Regional Parks - planned",
        stroke = TRUE,
        # weight = 0.5,
        color = p_col, 
        fill = TRUE,
        fillColor = p_col, # ouncilR::colors$suppGray,
        fillOpacity = 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$agency, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addCircles(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ],
        group = "Regional Parks - search",
        stroke = TRUE,
        radius = 2000,
        # weight = 0.5,
        color = s_col, 
        fill = TRUE,
        fillColor = s_col, 
        fillOpacity =  0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ]$status, "</b>", "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ]$name, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolygons(
        data = agency_boundary,
        group = "Agency boundaries",
        fill = FALSE,
        stroke = TRUE,
        weight = 2, # 0.75,
        color = councilR::colors$suppGray
      ) %>%
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ],
        group = "Regional Trails - existing",
        stroke = TRUE,
        weight = 3, # 3,
        color = e_col, 
        smoothFactor = 0.3,
        opacity = .8, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          
          color = "black", # "white",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ],
        group = "Regional Trails - search",
        stroke = TRUE,
        weight = 3, # 3,
        color = s_col, 
        smoothFactor = 0.3,
        opacity = .8, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$agency, "<em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ],
        group = "Regional Trails - planned",
        stroke = TRUE,
        weight = 3, # 3,
        color = p_col, 
        smoothFactor = 0.3,
        opacity = .8, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      # addPolylines(
      #   data = regionalparks.acs::trans_routes,
      #   group = "Transit",
      #   stroke = TRUE,
      #   weight = 2, # 0.75,
      #   color = councilR::colors$transitRed
      # ) %>%
      addCircles(
        data = regionalparks.acs::trans_stops,
        group = "Transit",
        radius = 3,
        fill = T,
        stroke = TRUE,
        weight = 2, # 0.75,
        color = councilR::colors$transitRed,
        fillColor = councilR::colors$transitRed
      ) %>%
      hideGroup(
        c(
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search",
          "Transit"
        )
      ) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Regional Parks - existing",
          "Regional Trails - existing",
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search",
          "Agency boundaries",
          "Census Tracts",
          "Transit"
        ),
        baseGroups = c(
          "Carto Positron",
          # "Carto DarkMatter",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })

  observeEvent(tract_data$tract_data, {
    pal <- if (tract_data$selected_var == "Income, Median Household Income") {
      colorQuantile(
        palette = tract_data$color_pal,
        n = 5,
        # reverse = TRUE,
        domain = tract_data$tract_data[[1]]
      )
    } else {
      colorNumeric(
        palette = tract_data$color_pal,
        domain = tract_data$tract_data[[1]]
      )
    }

        leafletProxy("map") %>%
      clearGroup("Census Tracts") %>%
      # clearControls() %>%
      addPolygons(
        data = tract_data$tract_data,
        group = "Census Tracts",
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ pal(tract_data$tract_data[[1]]),

        popup = if (tract_data$selected_var == "Income, Median Household Income") {
          ~ paste0(tags$strong(tract_data$selected_var), " $", format(tract_data$tract_data[[1]], big.mark = ","))
        } else {
          ~ paste0(
            tags$strong(tract_data$selected_var),
            " ",
            tract_data$tract_data[[1]], "%"
          )
        },
        options = list(zIndex = 0),
        popupOptions = popupOptions(closeOnClick = TRUE)
      ) %>%
      addLegend(
        title = if (tract_data$selected_var == "Income, Median Household Income") {
          "Income by Percentile"
        } else {
          "% of pop."
        },
        position = "bottomleft",
        group = "Census Tracts",
        layerId = "Census Tracts",
        pal = pal,
        values = tract_data$tract_data[[1]],
        labFormat = if (tract_data$selected_var == "Income, Median Household Income") {
          labelFormat()
        } else {
          labelFormat(suffix = "%")
        }
      )
  })
  
  
  
  
  observeEvent(
    c(selected_parktrail$input_agency,
      selected_parktrail$input_type,
      selected_parktrail$input_status),
    {
      leafletProxy("map") %>%
        clearGroup("Parks and trails") %>%
        addMapPane("Parks and trails", zIndex = 700) %>%
        addPolylines(
          group = "Parks and trails",
          data = parktrail_util$parktrail_data %>% filter(Type == "Trail"),
          color = case_when(
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Trail"] == "Existing" ~ e_col,
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Trail"] == "Planned" ~ p_col,
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Trail"] == "Search" ~ s_col
          ),
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~ paste0(
            "<b>",
            parktrail_util$parktrail_data$status[parktrail_util$parktrail_data$Type == "Trail"],
            "</b>",
            "<br>",
            parktrail_util$parktrail_data$name[parktrail_util$parktrail_data$Type == "Trail"],
            "<br>",
            "<em>",
            parktrail_util$parktrail_data$agency[parktrail_util$parktrail_data$Type == "Trail"],
            "</em>"
          ),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          ),
          options = list(zIndex = 700)
        ) %>%
        
        addPolygons(
          group = "Parks and trails",
          data = parktrail_util$parktrail_data %>% filter(Type == "Park"),
          color = case_when(
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Existing" ~ e_col,
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Planned" ~ p_col,
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Search" ~ s_col
          ),
          fillColor = case_when(
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Existing" ~ e_col,
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Planned" ~ p_col,
            parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Search" ~ s_col
          ),
          fillOpacity = 1,
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~ paste0(
            "<b>",
            parktrail_util$parktrail_data$status[parktrail_util$parktrail_data$Type == "Park"],
            "</b>",
            "<br>",
            parktrail_util$parktrail_data$name[parktrail_util$parktrail_data$Type == "Park"],
            "<br>",
            "<em>",
            parktrail_util$parktrail_data$agency[parktrail_util$parktrail_data$Type == "Park"],
            "</em>"
          ),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )#,
          # options = list(zIndex = 710)
        ) 
    })
  
  # 
  observeEvent( #add buffers -------
                c(selected_parktrail$input_agency,
                  selected_parktrail$input_type,
                  selected_parktrail$input_status,
                  selected_parktrail$input_distance), {
                    leafletProxy("map") %>%
                      clearGroup("Buffers") %>%
                      # clearControls()
                      addPolygons(
                        data = parktrail_util$buffer_data,
                        group = "Buffers",
                        stroke = TRUE,
                        weight = 2,
                        color = "#616161",
                        fill = T,
                        fillColor = "transparent",
                        opacity = .4,
                        fillOpacity = .005,
                        highlightOptions = highlightOptions(
                          stroke = TRUE,
                          color = "black",
                          weight = 6,
                          bringToFront = TRUE,
                          sendToBack = TRUE,
                          opacity = 1
                        ),
                        popup = ~ paste0(
                          "<b>",
                          "Buffer: ",
                          parktrail_util$buffer_data$status,
                          ", ",
                          parktrail_util$buffer_data$type,
                          "</b>",
                          "<br>",
                          parktrail_util$buffer_data$name,
                          "<br>",
                          "<em>",
                          parktrail_util$buffer_data$agency,
                          "</em>"
                        ),
                        popupOptions = popupOptions(
                          closeButton = FALSE,
                          style = list(
                            "font-size" = "18px",
                            "font-family" = "Arial"
                          )
                        ),
                        options = list(zIndex = 750),
                      )
                  })
  
  
}



#   
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
