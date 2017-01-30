#' ---
#' title: Soccer Schedule Scraper
#' author: Carlos Blancarte
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'  html_document:
#'    keep_md: true
#'    theme: lumen
#'    highlight: kate
#' ---

#+ message = FALSE, warning = FALSE
# Libraries
library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(stringi)

#+ include=FALSE
# writeFiles = FALSE so that it doesn't rewrite our data
writeFiles <- FALSE

#+ echo = FALSE
# Create object with today's date
dateToday <- Sys.Date()

#' ## Getting Starated
#' 
#' 

# homepage link
mainLink <- "http://www.livesoccertv.com/schedules/2017-01-31/"

# Read the html 
homepage <- read_html(mainLink)

# pull out the starting time for each game
startingTime <-  homepage %>%
  html_nodes(".ts") %>%
  html_text()

# pull out the channel where the game will be shown
tvChannel <- homepage %>%
  html_nodes('.timecell~ td+ td') %>%
  html_text()

# pull out the home and away team
game <- homepage %>%
  html_nodes(".timecell+ td") %>%
  html_text() %>%
  str_trim(side = 'both')

# put it together
schedule <- tibble(
  start_time = startingTime,
  matchup = game,
  showing_on = tvChannel
) %>%
  separate(matchup, c('home_team', 'away_team'), sep = 'vs')
