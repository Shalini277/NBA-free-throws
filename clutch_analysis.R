# modules/mod_clutch_analysis.R

mod_clutch_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Player selector
    selectInput(ns("player_input"), "Choose Player:", choices = NULL),
    # 1) Clutch Attempts vs. Success
    h4("Clutch Free Throw: Front vs Back End Success"),
    plotlyOutput(ns("clutch_breakdown_plot")) %>% withSpinner(type = 6, color = "#e67e22"),
    br(),
    # 2) Clutch Volume by Game Situation
    h4("Average Clutch Attempts per Game"),
    plotlyOutput(ns("clutch_attempts_plot")) %>% withSpinner(type = 6, color = "#3498db"),
    br(),
    # 3) Clutch Trend Over Season
    h4("Clutch Success Rate Trend by Season"),
    plotlyOutput(ns("clutch_season_trend_plot")) %>% withSpinner(type = 6, color = "#9b59b6")
  )
}

mod_clutch_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update player input choices
    observe({
      updateSelectInput(session, "player_input", choices = sort(unique(data()$player)))
    })
    
    # Reactive clutch data (period 4 or greater)
    clutch_data <- reactive({
      req(input$player_input)
      data() %>%
        filter(player == input$player_input, period >= 4) %>%
        # extract shot number: 'free throw 1 of 2' => 1
        mutate(
          shot_number = as.integer(str_extract(play, "(?<=free throw )\\d+(?= of)"))
        )
    })
    
    # 1) Clutch Attempts vs. Clutch Success
    output$clutch_breakdown_plot <- renderPlotly({
      df <- clutch_data() %>%
        group_by(shot_number) %>%
        summarise(
          attempts = n(),
          success_rate = round(mean(shot_made == 1) * 100, 1),
          .groups = 'drop'
        )
      validate(
        need(nrow(df) > 0, "No clutch attempts for this player.")
      )
      plot_ly(
        df,
        x = ~factor(shot_number, levels = c(1,2)),
        y = ~success_rate,
        type = 'bar',
        text = ~paste0(success_rate, "%"),
        textposition = 'auto',
        marker = list(color = c('#2ecc71', '#e74c3c'))
      ) %>%
        layout(
          title = paste("Front vs Back End Success -", input$player_input),
          xaxis = list(title = "Shot Number in Pair"),
          yaxis = list(title = "Success Rate (%)"),
          margin = list(b = 50)
        )
    })
    
    # 2) Clutch Volume by Game Situation
    output$clutch_attempts_plot <- renderPlotly({
      df <- clutch_data() %>%
        group_by(game_id) %>%
        summarise(
          attempts = n(),
          .groups = 'drop'
        ) %>%
        summarise(avg_attempts = round(mean(attempts), 2))
      validate(
        need(nrow(df) > 0, "No clutch attempts for this player.")
      )
      plot_ly(
        df,
        x = ~"Avg Attempts",
        y = ~avg_attempts,
        type = 'bar',
        text = ~avg_attempts,
        textposition = 'auto',
        marker = list(color = '#3498db')
      ) %>%
        layout(
          title = paste("Average Clutch Attempts per Game -", input$player_input),
          xaxis = list(title = ""),
          yaxis = list(title = "Attempts")
        )
    })
    
    # 3) Clutch Trend Over Season
    output$clutch_season_trend_plot <- renderPlotly({
      df <- clutch_data() %>%
        group_by(season) %>%
        summarise(
          success_rate = round(mean(shot_made == 1) * 100, 1),
          .groups = 'drop'
        )
      validate(
        need(nrow(df) > 0, "No clutch data for this player.")
      )
      plot_ly(
        df,
        x = ~season,
        y = ~success_rate,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8, color = '#9b59b6'),
        line = list(shape = 'spline')
      ) %>%
        layout(
          title = paste("Clutch Success Rate Trend -", input$player_input),
          xaxis = list(title = "Season"),
          yaxis = list(title = "Success Rate (%)"),
          margin = list(b = 50)
        )
    })
    
  })
}
