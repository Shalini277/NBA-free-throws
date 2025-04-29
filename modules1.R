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

mod_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update season choices dynamically
    observe({
      updatePickerInput(
        session,
        "season_input",
        choices = sort(unique(data()$season)),
        selected = unique(data()$season)
      )
    })
    
    # Reactive filtered data
    filtered_data <- reactive({
      req(input$season_input, input$playoff_input)
      data() %>%
        filter(
          season %in% input$season_input,
          playoffs == input$playoff_input
        )
    })
    
    # Output total shots
    output$total_shots <- shinydashboard::renderValueBox({
      valueBox(
        value = nrow(filtered_data()),
        subtitle = "Total Free Throws",
        icon = icon("bullseye"),
        color = "blue"
      )
    })
    
    # Output total players
    output$total_players <- shinydashboard::renderValueBox({
      valueBox(
        value = length(unique(filtered_data()$player)),
        subtitle = "Unique Players",
        icon = icon("users"),
        color = "purple"
      )
    })
    
    # Output success rate
    output$success_rate <- shinydashboard::renderValueBox({
      success_rate <- mean(filtered_data()$shot_result == "Made") * 100
      valueBox(
        value = sprintf("%.1f%%", success_rate),
        subtitle = "Overall Success Rate",
        icon = icon("percentage"),
        color = "green"
      )
    })
    
    # Return filtered data for use in other modules
    return(filtered_data)
  })
}
