
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  observe({
    print(input$nav)
  })

  # # Introduction tab -----------------------------------------------------------
  callModule(mod_intro_server, "intro_ui_1")
  
  # global inputs & utils --------------
  selected_parktrail_vars <- callModule(mod_selections_parktrailunits_server, "selections_parktrailunits_ui_1")

  global_util_parktrail <- callModule(mod_parktrail_utils_server, "parktrail_utils_ui_1",
                                      selected_parktrail = selected_parktrail_vars)

  # ACS Map tab ----------------------------------------------------------------
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data,
             selected_parktrail = selected_parktrail_vars,
             parktrail_util = global_util_parktrail)

  # ACS Summary tab ------------------------------------------------------------
  selected_input_vars <- callModule(mod_summary_selections_server, "summary_selections_ui_1")

  summary_util_vars <- callModule(mod_summary_utils_server, "summary_utils_ui_1",
    selected_vars = selected_input_vars)

  callModule(mod_summary_table_server, "summary_table_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars)

  callModule(mod_mod_summary_ggplot_server, "mod_summary_ggplot_ui_1",
             selected_vars = selected_input_vars,
             summary_util = summary_util_vars)
  
  callModule(mod_summary_map_server, "summary_map_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars)

  # Population growth tab ------------------------------------------------------
  selected_population_vars <- callModule(mod_selections_population_server, "selections_population_ui_1")

  summary_util_popvars <- callModule(mod_pop_utils_server, "pop_utils_ui_1",
                                     selected_population = selected_population_vars)

  callModule(mod_pop_map_server, "pop_map_ui_1",
             selected_popvars = selected_population_vars,
             selected_parktrail = selected_parktrail_vars,
             parktrail_util = global_util_parktrail,
             summary_poputil = summary_util_popvars)

  callModule(mod_pop_demoshifts_server, "pop_demoshifts_ui_1")

}
