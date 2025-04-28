# app.R
# call the required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)

# source the modules
source("modules1.R")
source("player_analysis.R")
source("module4.R")
source("clutch_analysis.R")
source("team_analysis.R")
source("pressure_analysis.R")

# Load Data
free_throws <- read.csv("free_throws.csv")
# clean the data
free_throws <- free_throws %>%
  mutate(
    shot_result = ifelse(str_detect(play, "makes"), "Made", "Missed"),
    season = str_trim(season),
    playoffs = ifelse(playoffs == "regular", "Regular Season", "Playoffs"),
    # SPLIT teams
    team1 = str_split_fixed(game, " - ", 2)[,1],
    team2 = str_split_fixed(game, " - ", 2)[,2],
    # SPLIT scores
    team1_score = as.numeric(str_split_fixed(end_result, " - ", 2)[,1]),
    team2_score = as.numeric(str_split_fixed(end_result, " - ", 2)[,2]),
    winner = ifelse(team1_score > team2_score, team1, team2),
    loser = ifelse(team1_score < team2_score, team1, team2),
    margin = abs(team1_score - team2_score)
  )

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = span("🏀 NBA Free Throws Insights", style = "font-weight: bold; font-size: 24px;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Team Analysis", tabName = "team_analysis", icon = icon("users")),
      menuItem("🧑‍💼 Player Analysis", tabName = "player_analysis", icon = icon("basketball-ball")),
      menuItem("Pressure Analysis", tabName = "pressure_analysis", icon = icon("bolt")),
      menuItem("Clutch Performance", tabName = "clutch_analysis", icon = icon("stopwatch"))
    )
  ),
  dashboardBody(
    
    # Include custom CSS for extra style
    includeCSS("www/style.css"),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
          HTML("<h2 style='font-weight:bold; font-size: 36px; color: #34495e;'>🏀 Welcome to the NBA Free Throw Dashboard!</h2>"),
          HTML("<p style='font-size:18px; color: #7f8c8d;'>Unlock insights. Optimize performance. Win the game!</p>")
        )
      )
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  width = 6,
                  title = span("🎯 Filters & Summary", style = "font-size:20px;"),
                  solidHeader = TRUE,
                  status = "primary",
                  background = "light-blue",
                  mod_summary_ui("summary_ui")
                ),
                box(
                  width = 6,
                  title = span("🏆 Top Players", style = "font-size:20px;"),
                  solidHeader = TRUE,
                  status = "success",
                  background = "teal",
                  mod_top_players_ui("top_players_ui")
                )
              )
      ),
      
      tabItem(tabName = "player_analysis",
              fluidRow(
                box(
                  width = 12,
                  title = span("🧠 Deep Dive: Player Analysis", style = "font-size:22px;"),
                  solidHeader = TRUE,
                  status = "info",
                  background = "navy",
                  mod_player_detail_ui("player_detail_ui")
                )
              )
      ),
      tabItem(tabName = "team_analysis",
              fluidRow(
                box(width = 12, title = "Team Insights", solidHeader = TRUE,
                    mod_team_analysis_ui("team_analysis_ui"))
              )
      ),
      tabItem(tabName = "pressure_analysis",
              fluidRow(
                box(width = 12, title = "Pressure Performance", solidHeader = TRUE,
                    mod_pressure_analysis_ui("pressure_analysis_ui"))
              )
      ),
      tabItem(tabName = "clutch_analysis",
              fluidRow(
                box(width = 12, title = "Clutch Time Performance", solidHeader = TRUE,
                    mod_clutch_analysis_ui("clutch_analysis_ui"))
              )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  full_data <- reactive({ free_throws })
  
  filtered_data <- mod_summary_server("summary_ui", full_data)
  
  mod_top_players_server("top_players_ui", filtered_data)
  
  mod_player_detail_server("player_detail_ui", filtered_data)
  mod_team_analysis_server("team_analysis_ui", filtered_data)
  mod_pressure_analysis_server("pressure_analysis_ui", filtered_data)
  mod_clutch_analysis_server("clutch_analysis_ui", filtered_data)
  
}

# Run
shinyApp(ui, server)
