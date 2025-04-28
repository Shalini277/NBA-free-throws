mod_player_detail_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("player_input"), "Choose Player:", choices = NULL),
    plotlyOutput(ns("player_breakdown")),
    plotlyOutput(ns("player_trend"))
  )
}

mod_player_detail_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectInput(session, "player_input", choices = unique(data()$player))
    })
    
    # Enhanced Pie Chart: Shot Breakdown with Dynamic Coloring and Hover Info
    output$player_breakdown <- renderPlotly({
      req(input$player_input)
      player_data <- data() %>%
        filter(player == input$player_input)
      
      player_summary <- player_data %>%
        count(shot_result)
      
      # Dynamic colors based on shot result
      colors <- c('Made' = '#32CD32', 'Missed' = '#FF6347')
      
      # Create a more appealing pie chart
      plot_ly(player_summary, labels = ~shot_result, values = ~n, type = 'pie',
              textinfo = 'label+percent', textposition = 'inside',
              hoverinfo = 'label+percent+value',
              marker = list(colors = colors[as.character(player_summary$shot_result)],
                            line = list(color = '#FFFFFF', width = 2))) %>%
        layout(title = paste("Shot Result Breakdown -", input$player_input),
               showlegend = FALSE,
               titlefont = list(size = 18, color = 'rgba(0, 0, 0, 0.8)'),
               plot_bgcolor = 'rgba(255, 255, 255, 0.9)')
    })
    
    # Enhanced Trend Line Chart: Free Throw Success Rate by Period with Dynamic Color & Annotations
    output$player_trend <- renderPlotly({
      req(input$player_input)
      trend_data <- data() %>%
        filter(player == input$player_input) %>%
        group_by(period) %>%
        summarise(success_rate = mean(shot_result == "Made") * 100, 
                  attempts = n())
      
      # Dynamic color based on success rate
      trend_data$color <- ifelse(trend_data$success_rate > 80, '#32CD32', 
                                 ifelse(trend_data$success_rate > 50, '#FFD700', '#FF6347'))
      
      # Add markers and line with smooth curves
      plot_ly(trend_data, x = ~period, y = ~success_rate, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#1f77b4', width = 3, shape = 'spline'),
              marker = list(color = ~color, size = 8, line = list(color = 'black', width = 2)),
              text = paste("Period: ", trend_data$period, 
                           "<br>Success Rate: ", round(trend_data$success_rate, 2), "%",
                           "<br>Attempts: ", trend_data$attempts),
              hoverinfo = 'text') %>%
        layout(title = paste("Free Throw Success Rate by Period -", input$player_input),
               titlefont = list(size = 18, color = 'rgba(0, 0, 0, 0.8)'),
               xaxis = list(title = "Period", showgrid = FALSE, zeroline = FALSE),
               yaxis = list(title = "Success Rate (%)", showgrid = TRUE, zeroline = FALSE),
               plot_bgcolor = 'rgba(255, 255, 255, 0.9)',
               hovermode = 'closest') %>%
        add_annotations(text = paste("Success Rate: ", round(trend_data$success_rate, 2), "%"),
                        x = trend_data$period, y = trend_data$success_rate,
                        showarrow = TRUE, arrowhead = 2, ax = 20, ay = -30)
    })
    
  })
}
