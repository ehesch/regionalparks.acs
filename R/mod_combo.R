#' combo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import tibble
#' @import ggplot2
#' @import cowplot
#' @import plotly
#' @import tidyr
#' @import stringr
#' @import forcats
#' @import dplyr
#' @import leaflet
#'

## set up some legends -----


######### ui --------
mod_combo_ui <- function(id) {
  ns <- NS(id)
  tagList(
   
  )
}

###### server -----
#' combo Server Function
#'
#' @noRd
mod_combo_server <- function(input, output, session) {
  ns <- session$ns

}

## To be copied in the UI
# mod_combo_ui("combo_ui_1")

## To be copied in the server
# callModule(mod_combo_server, "combo_ui_1")
