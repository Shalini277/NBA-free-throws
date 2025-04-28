library(shinytest2)

# Test the app
app <- AppDriver$new("app.R", view = TRUE)

# Test if the app loads
app$expect_values()

# Test clicking the 'Player Analysis' tab
app$click("menuItem-Player_Analysis")
app$expect_values()

# Test setting a filter on the 'season_input' picker
app$set_inputs(season_input = c("2021-22", "2022-23"))
app$expect_values()

# Test if the "Top Players" plot renders correctly
app$expect_output("top_players_plot")

# Close the app after testing
app$stop()

