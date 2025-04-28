mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "padding: 10px; background-color: green; border-radius: 10px; margin-bottom: 20px;",
      pickerInput(
        inputId = ns("season_input"),
        label = "Select Season:",
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE),
        width = "100%"
      ),
      radioButtons(
        inputId = ns("playoff_input"),
        label = "Season Type:",
        choices = c("Regular Season", "Playoffs"),
        selected = "Regular Season",
        inline = TRUE
      )
    ),
    fluidRow(
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("total_shots"), width = 12)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("total_players"), width = 15)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("success_rate"), width = 15)
      )
    )
  )
}
