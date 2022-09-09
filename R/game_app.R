# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(tidytext)
library(scales)
library(lubridate)
library(here)

# Load data --------------------------------------------------------------------

load(file = here("data/processed_data.RData"))


playerData <- playerData %>% 
  filter(season < 2022, round <= 23)

matchData_final <- matchData_final %>% 
  filter(season < 2022, round <= 23)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  fluidRow(
    column(width = 4,
      #tags$head(tags$style("#gameplot{height:100vh !important;}")),
      selectInput(inputId = "s", 
                  label = "Season",
                  choices = 2013:2021, 
                  selected = 2021)),
    column(width = 4,
      selectInput(inputId = "r", 
                  label = "Round",
                  choices = as.character(1:23), 
                  selected = "1")),
    column(width = 4,
      selectInput(inputId = "g", 
                  label = "Game",
                  choices = c(1:9,11), 
      selected = 1))),
    fluidRow(
    # Output: Show scatterplot
        plotOutput(outputId = "gameplot")
      )
    )

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$gameplot <- renderPlot({
    
    title_text <- matchData_final %>% 
      filter(season == input$s,round == input$r,game == input$g) %>% 
      mutate(title = paste(matchup, "at", venueName),
             subtitle = paste("Final score", homeTeamScoreTotalScore, "-", awayTeamScoreTotalScore))
    
    
    playerData %>% 
      filter(season == input$s,round == input$r,game == input$g) %>% 
      transmute(season, round, full_name = paste(givenName,surname), teamName, brownlowVotes,
                centreClearances, goals, totalClearances, stoppageClearances, 
                shotsAtGoal, marksInside50, goalAssists, ratingPoints, inside50S, goalAccuracy) %>% 
      pivot_longer(cols = -c(season, round, full_name, teamName,brownlowVotes), names_to = "stat", values_to = "value") %>% 
      group_by(stat) %>% 
      slice_max(value,n = 8, with_ties = FALSE) %>% 
      mutate(full_name = reorder_within(full_name,value,stat)) %>% 
      ggplot(aes(x = value, y = full_name, fill = factor(brownlowVotes))) +
      geom_point(aes(shape = teamName), size = 4,col = "black") +
      scale_y_reordered() +
      #scale_fill_manual(values = c("0" = "grey80", "1" = "#EDF8B1", "2" = "#7FCDBB", "3" = "#2C7FB8")) +
      scale_fill_manual(values = c("grey80","#FC9272", "#FB6A4A", "#A50F15")) +
      scale_shape_manual(values = c(22,24)) +
      labs(title = title_text$title,
           subtitle = title_text$subtitle,
           x = "",
           y = "Player name",
           shape = "Team",
           fill = "Votes") +
      facet_wrap(~stat, scales = "free") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 21),
            plot.subtitle = element_text(hjust = 0.5, size = 19),
            axis.text = element_text(size = 15),
            strip.text = element_text(size = 15)) +
      expand_limits(x = 0) +
      guides(fill = guide_legend(override.aes = list(shape = c(21))))
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)