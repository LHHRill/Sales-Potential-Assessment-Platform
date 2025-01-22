# global.R

# Load necessary libraries
library(googlesheets4)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(shinyvalidate)
library(caret)
library(recipes)
library(dplyr)
library(stringr)
library(reshape2)

# Define personality questions
personality_questions <- list(
  AS = c(
    "I express myself easily", "I try to lead others", "I automatically take charge",
    "I know how to convince others", "I am the first to act", "I take control of things",
    "I wait for others to lead the way", "I let others make the decisions",
    "I am not highly motivated to succeed", "I can't come up with new ideas"
  ),
  SC = c(
    "I feel comfortable around people", "I don't mind being the center of attention",
    "I am good at making impromptu speeches", "I express myself easily",
    "I have a natural talent for influencing people", "I hate being the center of attention",
    "I lack the talent for influencing people", "I often feel uncomfortable around others",
    "I don't like to draw attention to myself", "I have little to say"
  ),
  AD = c(
    "I prefer variety to routine", "I like to visit new places", "I am interested in many things",
    "I like to begin new things", "I prefer to stick with things that I know",
    "I dislike changes", "I don't like the idea of change", "I am a creature of habit",
    "I dislike new foods", "I am attached to conventional ways"
  ),
  DO = c(
    "I try to surpass others' accomplishments", "I try to outdo others", "I am quick to correct others",
    "I impose my will on others", "I demand explanations from others", "I want to control the conversation",
    "I am not afraid of providing criticism", "I challenge others' points of view",
    "I lay down the law to others", "I put people under pressure"
  )
)

# Function to generate personality questions
generate_questions <- function(trait) {
  lapply(1:10, function(i) {
    sliderInput(
      inputId = paste0(trait, i),
      label = personality_questions[[trait]][i],
      min = 1,
      max = 5,
      value = 3
    )
  })
}