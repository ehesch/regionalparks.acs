#' map_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_base_ui <- function(id) {
  ns <- NS(id)
  tagList()
  # use_waiter()
}

#' map_base Server Function
#'
#' @noRd
mod_map_base_server <- function(input, output, session) {
  ns <- session$ns
  # w <- Waiter$new()#, html="Please wait")#, hide_on_render=T)


  output$ns <- renderLeaflet(quoted = TRUE, {
    # w$show()
    # waiter_show()
    
    # ###  msp area territories 
    # native_land_json <- jsonlite::fromJSON("https://native-land.ca/api/index.php?maps=territories&name=oceti-sakowin-sioux,anishinabek,mdewakanton,wahpekute,/indigenousTerritories.json")
    #   # "https://native-land.ca/api/index.php?maps=territories&position=44.96,-93.16/indigenousTerritories.json") #2 territories, central msp area
    #  # "https://native-land.ca/api/index.php?maps=territories&position=45.458124028703274,-94.05888484444051/indigenousTerritories.json" # st. cloud area, 4 territories
    # # sauk-and-meskwaki #not inside boundary
    # # wahpeton #not inside boundary
    # # mdewakanton #not inside boundary; but maybe important becuase of Shakopee Mdewakanton Sioux community
    # # ho-chunk-winnebago,#not insiden boundary
    # #  #yes! boundary
    # #wahpekute,anishinabek, #yes, boundary!
    # geoms = native_land_json$geometry
    # # table(geoms$type)       # all geometries are polygons
    # # str(head(geoms)) 
    # # geoms$coordinates[[1]]
    # types = geoms$type
    # geoms = geoms$coordinates
    # plgs = lapply(1:length(geoms), function(x) {
    #   iter_type = glue::glue("{toupper(types[x])}")
    #   iter_geom = geoms[[x]]
    #   iter_geom = glue::glue("{iter_geom[,,1]} {iter_geom[,,2]}")
    #   iter_geom = paste(iter_geom, collapse = ', ')
    #   iter_out = as.character(glue::glue("{iter_type}(({iter_geom}))"))
    #   iter_out
    # })
    # # length(plgs)
    # properties = native_land_json$properties
    # properties$geometry = plgs
    # # str(properties)
    # df_properties = sf::st_as_sf(properties, wkt = "geometry")
    # leaflet() %>%
    #   addTiles() %>%
    #   addPolygons(data = df_properties,
    #               color = df_properties$color,
    #               opacity = 1.0,
    #               popup = df_properties$Name) %>%
    #   addPolygons(
    #     data = regionalparks.acs::agency_boundary,
    #     group = "Agency boundaries",
    #     stroke = T,
    #     color = "black",
    #     fill = F,
    #     weight = 2)
   
    
    ###  oceti 
    oceti <- jsonlite::fromJSON("https://native-land.ca/api/index.php?maps=territories&name=oceti-sakowin-sioux,/indigenousTerritories.json")
    oceti_geoms = oceti$geometry
    oceti_types = oceti_geoms$type
    oceti_geoms = oceti_geoms$coordinates
    oceti_plgs = lapply(1:length(oceti_geoms), function(x) {
      iter_type = glue::glue("{toupper(oceti_types[x])}")
      iter_geom = oceti_geoms[[x]]
      iter_geom = glue::glue("{iter_geom[,,1]} {iter_geom[,,2]}")
      iter_geom = paste(iter_geom, collapse = ', ')
      iter_out = as.character(glue::glue("{iter_type}(({iter_geom}))"))
      iter_out
    })
    oceti_properties = oceti$properties
    oceti_properties$geometry = oceti_plgs
    oceti_properties = sf::st_as_sf(oceti_properties, wkt = "geometry")
    
    ###  anishinabek 
    anishinabek <- jsonlite::fromJSON("https://native-land.ca/api/index.php?maps=territories&name=anishinabek,/indigenousTerritories.json")
    anishinabek_geoms = anishinabek$geometry
    anishinabek_types = anishinabek_geoms$type
    anishinabek_geoms = anishinabek_geoms$coordinates
    anishinabek_plgs = lapply(1:length(anishinabek_geoms), function(x) {
      iter_type = glue::glue("{toupper(anishinabek_types[x])}")
      iter_geom = anishinabek_geoms[[x]]
      iter_geom = glue::glue("{iter_geom[,,1]} {iter_geom[,,2]}")
      iter_geom = paste(iter_geom, collapse = ', ')
      iter_out = as.character(glue::glue("{iter_type}(({iter_geom}))"))
      iter_out
    })
    anishinabek_properties = anishinabek$properties
    anishinabek_properties$geometry = anishinabek_plgs
    anishinabek_properties = sf::st_as_sf(anishinabek_properties, wkt = "geometry")
    
    ###  mdewakanton 
    mdewakanton <- jsonlite::fromJSON("https://native-land.ca/api/index.php?maps=territories&name=mdewakanton,/indigenousTerritories.json")
    mdewakanton_geoms = mdewakanton$geometry
    mdewakanton_types = mdewakanton_geoms$type
    mdewakanton_geoms = mdewakanton_geoms$coordinates
    mdewakanton_plgs = lapply(1:length(mdewakanton_geoms), function(x) {
      iter_type = glue::glue("{toupper(mdewakanton_types[x])}")
      iter_geom = mdewakanton_geoms[[x]]
      iter_geom = glue::glue("{iter_geom[,,1]} {iter_geom[,,2]}")
      iter_geom = paste(iter_geom, collapse = ', ')
      iter_out = as.character(glue::glue("{iter_type}(({iter_geom}))"))
      iter_out
    })
    mdewakanton_properties = mdewakanton$properties
    mdewakanton_properties$geometry = mdewakanton_plgs
    mdewakanton_properties = sf::st_as_sf(mdewakanton_properties, wkt = "geometry")
    
    ###  wahpekute 
    wahpekute <- jsonlite::fromJSON("https://native-land.ca/api/index.php?maps=territories&name=wahpekute,/indigenousTerritories.json")
    wahpekute_geoms = wahpekute$geometry
    wahpekute_types = wahpekute_geoms$type
    wahpekute_geoms = wahpekute_geoms$coordinates
    wahpekute_plgs = lapply(1:length(wahpekute_geoms), function(x) {
      iter_type = glue::glue("{toupper(wahpekute_types[x])}")
      iter_geom = wahpekute_geoms[[x]]
      iter_geom = glue::glue("{iter_geom[,,1]} {iter_geom[,,2]}")
      iter_geom = paste(iter_geom, collapse = ', ')
      iter_out = as.character(glue::glue("{iter_type}(({iter_geom}))"))
      iter_out
    })
    wahpekute_properties = wahpekute$properties
    wahpekute_properties$geometry = wahpekute_plgs
    wahpekute_properties = sf::st_as_sf(wahpekute_properties, wkt = "geometry")
    
    
                  
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.22,
        zoom = 10
      ) %>%
      leaflet.extras::addDrawToolbar(
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions = F,
        circleOptions = F,
        rectangleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        polylineOptions = drawPolylineOptions(
          shapeOptions = drawShapeOptions(
            color = "black",
            weight = 2
          ),
          guidelineDistance = 1,
          metric = F,
          feet = T
        )
      ) %>%
      # addMeasure(primaryLengthUnit="miles", secondaryLengthUnit="feet") %>%
      addMapPane("parks_geo", zIndex = 420) %>%
      addMapPane(name = "Carto Positron", zIndex = 430) %>%
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        options = leafletOptions(pane = "Carto Positron"),
        group = "Carto Positron"
      ) %>%
      addProviderTiles(
        "CartoDB.PositronNoLabels",
        group = "Carto Positron"
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Aerial photography"
      ) %>%
      
      #native-land.ca
      addMapPane("Wahpekute Traditional Territory", zIndex = 650) %>%
      addPolygons(
        data = wahpekute_properties,
        color = wahpekute_properties$color,
        opacity = 1.0,
        popup = wahpekute_properties$Name,
        group = "Wahpekute Traditional Territory",
        stroke = T,
        weight = 2,
        options = pathOptions(pane = "Wahpekute Traditional Territory")
      ) %>%
      addMapPane("Mdewakanton Traditional Territory", zIndex = 650) %>%
      addPolygons(
        data = mdewakanton_properties,
        color = mdewakanton_properties$color,
        opacity = 1.0,
        popup = mdewakanton_properties$Name,
        group = "Mdewakanton Traditional Territory",
        stroke = T,
        weight = 2,
        options = pathOptions(pane = "Mdewakanton Traditional Territory")
      ) %>%
      addMapPane("Očhéthi Šakówiŋ Traditional Territory", zIndex = 650) %>%
      addPolygons(
        data = oceti_properties,
        color = oceti_properties$color,
        opacity = 1.0,
        popup = oceti_properties$Name,
        group = "Očhéthi Šakówiŋ Traditional Territory",
        stroke = T,
        weight = 2,
        options = pathOptions(pane = "Očhéthi Šakówiŋ Traditional Territory")
      ) %>%
      addMapPane("Anishinabewaki ᐊᓂᔑᓈᐯᐗᑭ Traditional Territory", zIndex = 650) %>%
      addPolygons(
        data = anishinabek_properties,
        color = anishinabek_properties$color,
        opacity = 1.0,
        popup = anishinabek_properties$Name,
        group = "Anishinabewaki ᐊᓂᔑᓈᐯᐗᑭ Traditional Territory",
        stroke = T,
        weight = 2,
        options = pathOptions(pane = "Anishinabewaki ᐊᓂᔑᓈᐯᐗᑭ Traditional Territory")
      ) %>%
      
      # start of old map
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
      addMapPane("water_access", zIndex = 431) %>%
      addAwesomeMarkers(
        group = "Water Access",
        data = regionalparks.acs::water_access,
        icon = iconwater,
        options = pathOptions(pane = "water_access")
      ) %>%
      groupOptions(
        group = "Water Access",
        zoomLevels = 13:20
      ) %>%
      addMapPane("entrance", zIndex = 432) %>%
      addAwesomeMarkers(
        group = "Park Entrance",
        data = regionalparks.acs::entrance,
        icon = iconentry,
        options = pathOptions(pane = "entrance")
      ) %>%
      groupOptions(
        group = "Park Entrance",
        zoomLevels = 13:20
      ) %>%
      addMapPane("trans", zIndex = 430) %>%
      addCircles(
        # Markers(
        data = regionalparks.acs::trans_stops,
        group = "Active transit stops",
        radius = 20,
        fill = T,
        stroke = TRUE,
        weight = 2,
        color = councilR::colors$transitRed,
        fillColor = councilR::colors$transitRed,
        options = pathOptions(pane = "trans")
      ) %>%
      groupOptions(
        group = "Active transit stops",
        zoomLevels = 13:20
      ) %>%
      addMapPane("riverlake", zIndex = 429) %>%
      addPolygons(
        data = regionalparks.acs::river_lake,
        group = "Rivers & Lakes",
        stroke = TRUE,
        # weight = 0.5,
        color = "black",
        fill = TRUE,
        fillColor = "black",
        fillOpacity = 0.9,
        options = pathOptions(pane = "riverlake")
      ) %>%
      hideGroup(
        c(
          "Buffers",
          "Agency boundaries",
          "Active transit stops",
          "Water Access",
          "Park Entrance",
          "Rivers & Lakes",
          "Anishinabewaki ᐊᓂᔑᓈᐯᐗᑭ Traditional Territory",
          "Mdewakanton Traditional Territory",
          "Očhéthi Šakówiŋ Traditional Territory",
          "Wahpekute Traditional Territory"
        )
      ) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Parks and trails",
          "Buffers",
          "Population data",
          "Active transit stops",
          "Water Access",
          "Park Entrance",
          "Rivers & Lakes",
          "Agency boundaries",
          "Anishinabewaki ᐊᓂᔑᓈᐯᐗᑭ Traditional Territory",
          "Mdewakanton Traditional Territory",
          "Očhéthi Šakówiŋ Traditional Territory",
          "Wahpekute Traditional Territory"
        ),
        baseGroups = c(
          "Carto Positron",
          "Aerial photography"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
    # waiter_hide()
    # w$hide
  })
}

## To be copied in the UI
# mod_map_base_ui("map_base_ui_1")

## To be copied in the server
# callModule(mod_map_base_server, "map_base_ui_1")
