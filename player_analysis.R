# modules/mod_top_players.R

mod_top_players_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("top_players_plot"), height = "500px")
}

mod_top_players_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$top_players_plot <- renderPlotly({
      top_players <- data() %>%
        group_by(player) %>%
        summarise(success_rate = mean(shot_result == "Made")*100, attempts = n()) %>%
        filter(attempts > 20) %>%
        arrange(desc(success_rate)) %>%
        head(10)
      
      # Adding color based on success rate for better visualization
      color_scale <- colorRampPalette(c("#FF6347", "#32CD32"))(10)
      
      plot_ly(
        top_players,
        x = ~success_rate,
        y = ~reorder(player, success_rate),
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = ~success_rate,
          colorscale = 'Viridis',   # You can change the colorscale to something else (e.g., 'YlGnBu')
          showscale = TRUE
        ),
        hoverinfo = 'x+text',
        text = ~paste("Attempts: ", attempts, "<br>Success Rate: ", round(success_rate, 2), "%")
      ) %>%
        layout(
          title = "Top 10 Players by Free Throw Success Rate",
          titlefont = list(size = 20, color = 'rgba(0, 0, 0, 0.8)'),
          xaxis = list(
            title = "Success Rate (%)",
            titlefont = list(size = 14),
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "Player",
            titlefont = list(size = 14),
            tickfont = list(size = 12)
          ),
          plot_bgcolor = 'rgba(255, 255, 255, 0.9)',  # Background color
          margin = list(t = 50, b = 50, l = 80, r = 50),  # Adjusting margins
          barmode = 'group',  # Ensures clean bar arrangement
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)  # Hide the mode bar for a cleaner UI
    })
    
  })
}
