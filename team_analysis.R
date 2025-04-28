# modules/team_analysis.R

mod_team_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("season_filter"),
      label = "Select Season(s):",
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    radioButtons(
      inputId = ns("playoff_filter"),
      label = "Select Season Type:",
      choices = c("Regular Season", "Playoffs"),
      selected = "Regular Season",
      inline = TRUE
    ),
    pickerInput(
      inputId = ns("period_filter"),
      label = "Select Period:",
      choices = c("All", 1, 2, 3, 4),
      selected = "All",
      multiple = FALSE
    ),
    
    h3("🏆 Top 5 Teams by Success Rate"),
    plotlyOutput(ns("top_teams_plot")) %>% withSpinner(),
    
    br(),
    
    h3("📈 Period-wise Team Performance"),
    plotlyOutput(ns("period_success_plot")) %>% withSpinner(),
    
    br(),
    
    h3("🏅 Team Wins, Losses, and Margins"),
    DT::DTOutput(ns("team_summary_table")) %>% withSpinner(),
    downloadButton(ns("download_team_summary"), "Download Summary") # 📥 Download Button
  )
}

mod_team_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      seasons <- unique(data()$season)
      updatePickerInput(session, "season_filter", choices = seasons, selected = seasons)
    })
    
    filtered_team_data <- reactive({
      req(input$season_filter)
      
      df <- data() %>%
        filter(
          season %in% input$season_filter,
          playoffs == input$playoff_filter
        )
      
      if (input$period_filter != "All") {
        df <- df %>% filter(period == as.numeric(input$period_filter))
      }
      
      # 📣 Important: Create team1, team2, team1_score, team2_score properly here!
      df <- df %>%
        separate(game, into = c("team1", "team2"), sep = " - ", remove = FALSE) %>%
        separate(end_result, into = c("team1_score", "team2_score"), sep = " - ", remove = FALSE) %>%
        mutate(
          team1_score = as.numeric(team1_score),
          team2_score = as.numeric(team2_score)
        )
      
      validate(
        need(nrow(df) > 0, "No data available for selected filters!")
      )
      
      df
    })
    
    
    team_summary <- reactive({
      filtered_team_data() %>%
        pivot_longer(
          cols = c(team1, team2),
          names_to = "team_side",
          values_to = "team"
        ) %>%
        group_by(team) %>%
        summarise(
          total_attempts = n(),
          success_rate = mean(shot_result == "Made") * 100,
          avg_score = mean(ifelse(team_side == "team1", team1_score, team2_score)),
          .groups = 'drop'
        ) %>%
        arrange(desc(success_rate))
    })
    
    output$top_teams_plot <- renderPlotly({
      validate(
        need(nrow(team_summary()) > 0, "No data to plot Top Teams!")
      )
      plot_ly(
        team_summary() %>% head(5),
        x = ~success_rate,
        y = ~reorder(team, success_rate),
        type = "bar",
        orientation = "h",
        text = ~paste0("Avg Score: ", round(avg_score, 1)),
        hoverinfo = "text+x+y",
        marker = list(color = 'rgba(0, 123, 255, 0.7)', line = list(color = 'rgba(0, 123, 255, 1)', width = 2))
      ) %>%
        layout(
          title = "Top 5 Teams by Success Rate",
          xaxis = list(title = "Success Rate (%)"),
          yaxis = list(title = "Team"),
          margin = list(l = 100, r = 50)
        )
    })
    
    output$period_success_plot <- renderPlotly({
      filtered_team_data() %>%
        pivot_longer(
          cols = c(team1, team2),
          names_to = "team_side",
          values_to = "team"
        ) %>%
        group_by(team, period) %>%
        summarise(
          success_rate = mean(shot_result == "Made") * 100,
          .groups = 'drop'
        ) %>%
        plot_ly(
          x = ~period,
          y = ~success_rate,
          color = ~team,
          type = "scatter",
          mode = "lines+markers"
        ) %>%
        layout(
          title = "Period-wise Success Rate by Team",
          xaxis = list(title = "Period"),
          yaxis = list(title = "Success Rate (%)"),
          legend = list(orientation = "h", x = 0.1, y = -0.2)
        )
    })
    
    # 🆕 Team Wins, Losses, Margins Table
    team_summary_table_data <- reactive({
      req(filtered_team_data())
      
      team_game_data <- filtered_team_data() %>%
        separate(game, into = c("team1", "team2"), sep = " - ") %>%
        separate(end_result, into = c("team1_score", "team2_score"), sep = " - ") %>%
        mutate(
          team1_score = as.numeric(team1_score),
          team2_score = as.numeric(team2_score),
          winner = ifelse(team1_score > team2_score, team1, team2),
          loser = ifelse(team1_score < team2_score, team1, team2),
          margin = abs(team1_score - team2_score)
        )
      
      wins <- team_game_data %>%
        group_by(winner) %>%
        summarise(
          wins = n(),
          avg_win_margin = mean(margin),
          max_win_margin = max(margin),
          .groups = "drop"
        ) %>%
        rename(team = winner)
      
      losses <- team_game_data %>%
        group_by(loser) %>%
        summarise(
          losses = n(),
          max_loss_margin = max(margin),
          .groups = "drop"
        ) %>%
        rename(team = loser)
      
      full_summary <- full_join(wins, losses, by = "team") %>%
        replace(is.na(.), 0) %>%
        arrange(desc(wins))
      
      full_summary
    })
    
    output$team_summary_table <- DT::renderDT({
      DT::datatable(team_summary_table_data(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # 📥 Download Button logic
    output$download_team_summary <- downloadHandler(
      filename = function() {
        paste0("team_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(team_summary_table_data(), file, row.names = FALSE)
      }
    )
    
  })
}
