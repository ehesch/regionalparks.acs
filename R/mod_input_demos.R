#' input_demos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_demos_ui <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(
             selectInput(
               ns("input_acs"),
               label = h5("ACS variable"),
               choices = list(
                 `Age` = list(
                   "Age, % under 15" = "adj_ageunder15_per",
                   "Age, % 15-24" = "adj_age15_24_per",
                   "Age, % 25-64" = "adj_age25_64_per",
                   "Age, % 65+" = "adj_age65up_per"
                 ),
                 `Race` = list(
                   "Race, % Am. Indian" = "adj_amindnh_per",
                   "Race, % Asian" = "adj_asiannh_per",
                   "Race, % Black" = "adj_blacknh_per",
                   "Race, % Other + Multi" = "adj_othermultinh_per",
                   "Race, % White" = "adj_whitenh_per"
                 ),
                 `Ethnicity` = list(
                   "Ethnicity, % Hispanic" = "adj_hisppop_per",
                   "Ethnicity, % not-Hispanic" = "adj_nothisppop_per"
                 ),
                 `National origin` = list(
                   "Origin, % foreign-born" = "adj_forborn_per",
                   "Origin, % US-born" = "adj_usborn_per"
                 ),
                 `Ability` = list(
                   "Ability, % any disability" = "adj_anydis_per"
                 ),
                 `Income` = list("Mean household income ($)" = "adj_meanhhi"),
                 `Transportation` = list("% Housholds without a vehicle" = "adj_novehicle_per"),
                 `Language` = list(
                   "% speaking English less than very well" = "adj_lep_per",
                   "% Spanish speakers" = "adj_span_per"
                 )
               ),
               selected = "adj_ageunder15_per", selectize = F
             )
      )
  )
}

#' input_demos Server Function
#'
#' @noRd
mod_input_demos_server <- function(input, output, session) {
  ns <- session$ns


  vals <- reactiveValues()

  observeEvent(input$inputCensusTracts, {
    vals$selected_var <- input$inputCensusTracts
    vals$color_pal <- dplyr::filter(table_ct, category == input$inputCensusTracts)[[3]]
    vals$tract_data <- census_tract[input$inputCensusTracts]
  })


  return(vals)
}

## To be copied in the UI
# mod_input_demos_ui("input_demos_ui_1")

## To be copied in the server
# callModule(mod_input_demos_server, "input_demos_ui_1")
