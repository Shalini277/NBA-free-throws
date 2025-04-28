# modules/mod_pressure_analysis.R

mod_pressure_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("player_input"), "Choose Player:", choices = NULL),
    sliderInput(ns("pressure_threshold"), "Pressure Threshold (points):",
                min = 1, max = 20, value = 5, step = 1),
    checkboxInput(ns("win_loss_toggle"), "Include Win/Loss Under Pressure?", FALSE),
    plotlyOutput(ns("pressure_rate_plot")) %>% withSpinner(type = 6, color = "#e74c3c"),
    plotlyOutput(ns("pressure_attempts_plot")) %>% withSpinner(type = 6, color = "#3498db"),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("win_loss_toggle")),
      plotlyOutput(ns("win_loss_plot")) %>% withSpinner(type = 6, color = "#2ecc71")
    ),
    plotlyOutput(ns("pressure_season_plot")) %>% withSpinner(type = 6, color = "#9b59b6")
  )
}

mod_pressure_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Populate player choices
    observe({
      updateSelectInput(session, "player_input", choices = sort(unique(data()$player)))
    })
    
    # Prepare pressure data
    pressure_data <- reactive({
      req(input$player_input)
      df <- data() %>%
        filter(player == input$player_input) %>%
        separate(game, into = c("team1","team2"), sep = " - ", remove = FALSE) %>%
        separate(end_result, into = c("team1_score","team2_score"), sep = " - ", remove = FALSE) %>%
        mutate(
          team1_score = as.numeric(team1_score),
          team2_score = as.numeric(team2_score),
          score_diff = abs(team1_score - team2_score),
          pressure = ifelse(score_diff <= input$pressure_threshold, "High Pressure", "Normal")
        )
      validate(
        need(nrow(df) > 0, "No data available for this player and threshold.")
      )
      df
    })
    
    # Summary for rate and attempts
    pressure_summary <- reactive({
      pressure_data() %>%
        group_by(pressure) %>%
        summarise(
          success_rate = round(mean(shot_made == 1) * 100, 2),
          attempts = n(),
          .groups = 'drop'
        )
    })
    
    # Plot: Success Rate under Pressure
    output$pressure_rate_plot <- renderPlotly({
      df <- pressure_summary()
      plot_ly(df, x=~pressure, y=~success_rate, type='bar',
              marker=list(color=c('#e74c3c','#2ecc71')) ) %>%
        layout(title = paste("Success Rate under Pressure -", input$player_input),
               xaxis=list(title="Situation"), yaxis=list(title="Success Rate (%)"))
    })
    
    # Plot: Attempts under Pressure
    output$pressure_attempts_plot <- renderPlotly({
      df <- pressure_summary()
      plot_ly(df, x=~pressure, y=~attempts, type='bar',
              marker=list(color=c('#3498db','#95a5a6')) ) %>%
        layout(title = paste("Attempts under Pressure -", input$player_input),
               xaxis=list(title="Situation"), yaxis=list(title="Number of Attempts"))
    })
    
    # a) Win/Loss Under Pressure
    output$win_loss_plot <- renderPlotly({
      req(input$win_loss_toggle)
      df <- pressure_data() %>%
        mutate(
          player_team = if_else(player %in% unique(team1), team1, team2),
          winner = if_else(team1_score > team2_score, team1, team2),
          game_outcome = if_else(player_team == winner, "Win", "Loss")
        ) %>%
        group_by(game_id, game_outcome) %>%
        slice(1) %>%
        ungroup() %>%
        count(game_outcome) %>%
        mutate(win_pct = round(n / sum(n) * 100, 1))
      plot_ly(df, labels=~game_outcome, values=~win_pct, type='pie') %>%
        layout(title=paste("Win % under Pressure -", input$player_input))
    })
    
    # b) Pressure vs Season Trend
    output$pressure_season_plot <- renderPlotly({
      df <- pressure_data() %>%
        group_by(season, pressure) %>%
        summarise(success_rate = mean(shot_made == 1) * 100, .groups='drop')
      plot_ly(df, x=~season, y=~success_rate, color=~pressure, type='scatter', mode='lines+markers') %>%
        layout(title = paste("Pressure Success Rate by Season -", input$player_input),
               xaxis=list(title="Season"), yaxis=list(title="Success Rate (%)"))
    })
    
  })
}
